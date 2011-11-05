-module(http_router_compiler).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("kernel/include/file.hrl").
-include("log.hrl").

-export([generate_and_compile/1, generate_router/1]).
-export([ensure_loaded/1, check/1]).


check(Path) ->
  {ok, Module} = ensure_loaded(Path),
  {ok, #file_info{mtime = MTime}} = file:read_file_info(Path),
  CTime = Module:ctime(),
  if
    CTime < MTime ->
      ?D({reload,Path,CTime,MTime}),
      code:soft_purge(Module),
      generate_and_compile(Path);
    true ->
      ok
  end.  

ensure_loaded(Path) ->
  case erlang:module_loaded(http_router) of
    true -> {ok, http_router};
    false -> generate_and_compile(Path)
  end.


generate_and_compile(ConfigPath) ->
  {ok, Code} = generate_router(ConfigPath),
  {ok, _} = compile_router(http_router, Code).


generate_router(ConfigPath) ->
  Config = http_router_config:file(ConfigPath),
  {ok, #file_info{mtime = MTime}} = file:read_file_info(ConfigPath),
  {ok, Code, _Index} = translate_commands(Config),
  Module = [
  "-module(http_router).\n",
  "-export([handle/2, ctime/0]).\n\n",
  "ctime() -> ", io_lib:format("~p", [MTime]), ".\n\n",
  "handle(Env, Req) -> \n",
  "  handle0(Env, Req).\n\n",
  "handle0(Env0, Req0) -> \n",
  Code
  ],
  {ok, iolist_to_binary(Module)}.


compile_router(Module, Code) ->
  Path = lists:flatten(io_lib:format("~s.erl", [Module])),
  {ModName, Bin} = dynamic_compile:from_string(binary_to_list(Code), [report,verbose]),
  {module, ModName} = code:load_binary(ModName, Path, Bin),
  {ok, ModName}.


translate_commands(Config) ->
  translate_commands(Config, 0, 0, 0, []).

translate_commands([{location, _Name, {Re, Keys}, _Flags, LocationBody}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  RegexKeys = io_lib:format("~p", [[0|Keys]]),
  LocationName = io_lib:format("location~p", [FunIdx+1]),
  NextStep = io_lib:format("location~p", [FunIdx+2]),
  ReS = io_lib:format("~p", [Re]),
  Code = [
  "  case re:run(proplists:get_value(path,Env", integer_to_list(EnvIdx), "), ", ReS,", [{capture,", RegexKeys, ",binary}]) of\n",

  case Keys of
    [] ->
      ["    {match, _} -> \n",
      "      ",LocationName,"(Env", integer_to_list(EnvIdx), ", Req", integer_to_list(ReqIdx), ");\n"];
    _ ->  
      ["    {match, [_MatchedURL|Values]} -> \n",
        io_lib:format("      Env~p = lists:ukeymerge(1, lists:ukeysort(1,lists:zip(~240p, Values)), Env~p),\n", [EnvIdx+1, Keys, EnvIdx]),
      "      ",LocationName,"(Env", integer_to_list(EnvIdx+1), ", Req", integer_to_list(ReqIdx), ");\n"]
  end,
  "    nomatch ->
      ", NextStep, "(Env", integer_to_list(EnvIdx), ", Req", integer_to_list(ReqIdx), ")\n",
  "  end.\n\n",

  LocationName, "(Env0,Req0) -> \n"
  ],

  {ok, LocationCode, NewFunIdx} = translate_commands(LocationBody, FunIdx+2, 0, 0, []),

  Code1 = [NextStep, "(Env0, Req0) -> \n"],

  translate_commands(Rest, NewFunIdx, 0, 0, Acc ++ Code ++ LocationCode ++ Code1);

translate_commands([{rewrite, Re, Replacement}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = [
  "  Env", integer_to_list(EnvIdx+1), " = lists:keyreplace(path, 1, Env", integer_to_list(EnvIdx), ", {path, ",
  io_lib:format("re:replace(proplists:get_value(path, Env~p), ~p, ~p, [{return, binary}])", [EnvIdx, Re, Replacement]),
  "}),\n"
  ],
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx+1, Acc ++ Code);

translate_commands([{set, Key, Value}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = [
  io_lib:format("  Env~p = lists:ukeymerge(1, [{~p,~p}], Env~p),\n", [EnvIdx+1, Key, Value, EnvIdx])
  ],
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx+1, Acc ++ Code);

translate_commands([{handler, M, F, A}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Args = case A of
    [] -> "";
    _ -> "," ++ string:join([lists:flatten(io_lib:format("~p", [Arg])) || Arg <- A], ", ")
  end,
  Code = [
  io_lib:format("  case ~p:~p(Req~p, Env~p~s) of\n", [M, F, ReqIdx, EnvIdx, Args]),
  io_lib:format("    {ok, Req~p} -> {ok, Req~p};\n", [ReqIdx+1, ReqIdx+1]),
  io_lib:format("    unhandled -> handle~p(Env~p, Req~p);\n", [FunIdx+1, EnvIdx, ReqIdx]),
  io_lib:format("    {unhandled, Env~p, Req~p} -> handle~p(Env~p, Req~p)\n", [EnvIdx+1, ReqIdx+1, FunIdx+1, EnvIdx+1, ReqIdx+1]),
  "  end.\n\n",

  "handle", integer_to_list(FunIdx+1), "(Env0, Req0) -> \n"
  ],
  translate_commands(Rest, FunIdx+1, 0, 0, Acc ++ Code);

translate_commands([_|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx, Acc);

translate_commands([], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = io_lib:format("  cowboy_http_req:reply(404, [], <<\"404 \", (proplists:get_value(path, Env~p))/binary, \" not found\\n\">>, Req~p).\n\n",
  [EnvIdx, ReqIdx]),
  {ok, Acc ++ Code, FunIdx+1}.
