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
  case generate_router(ConfigPath) of
    {ok, Code} -> compile_router(http_router, Code);
    {error, Reason} -> {error, Reason}
  end.


generate_router(ConfigPath) ->
  case http_router_config:file(ConfigPath) of
    {ok, Config} ->
      make_compiled_code(ConfigPath, Config);
    {error, Reason} ->
      {error, Reason}
  end.

make_compiled_code(ConfigPath, Config) ->
  {ok, #file_info{mtime = MTime}} = file:read_file_info(ConfigPath),
  {ok, Code, _Index} = translate_commands(Config),
  Module = [
  "-module(http_router).\n",
  "-export([handle/2, ctime/0]).\n\n",
  "ctime() -> ", io_lib:format("~p", [MTime]), ".\n\n",
  "handle(Req, Env) -> \n",
  "  handle0(Req, Env).\n\n",
  "handle0(Req0, Env0) -> \n",
  Code
  ],
  {ok, iolist_to_binary(Module)}.


compile_router(Module, Code) ->
  Path = lists:flatten(io_lib:format("~s.erl", [Module])),
  file:write_file(Path, Code),
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
  "    {match, [_MatchedURL|Values]} -> \n",
  % "io:format(\"Match ~p to ~p~n\", [proplists:get_value(path,Env", integer_to_list(EnvIdx), "), ",ReS, "]), ",
io_lib:format("      Env~p = lists:ukeymerge(1, lists:ukeysort(1,lists:zip(~240p, Values)), Env~p),\n", [EnvIdx+1, Keys, EnvIdx]),
io_lib:format("      case ~s(Req~p, Env~p) of\n", [LocationName, ReqIdx, EnvIdx+1]),
io_lib:format("        {ok, Req~p} -> {ok, Req~p};\n", [ReqIdx+1, ReqIdx+1]),
io_lib:format("        {unhandled, Req~p, Env~p} -> ~s(Req~p, Env~p)\n", [ReqIdx+1, EnvIdx+2, NextStep, ReqIdx+1, EnvIdx+2]),
  "      end;"
  "    nomatch -> \n",
  % "io:format(\"nomatch ~p to ~p~n\", [proplists:get_value(path,Env", integer_to_list(EnvIdx), "), ",ReS, "]), ",
io_lib:format("      ~s(Req~p, Env~p)\n", [NextStep, ReqIdx, EnvIdx]),
  "  end.\n\n",

  LocationName, "(Req0, Env0) -> \n"
  ],

  {ok, LocationCode, NewFunIdx} = translate_commands(LocationBody, FunIdx+2, 0, 0, []),

  Code1 = [NextStep, "(Req0, Env0) -> \n"],

  translate_commands(Rest, NewFunIdx, 0, 0, Acc ++ Code ++ LocationCode ++ Code1);

translate_commands([{rewrite, Val, Re, Replacement}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = [
  io_lib:format("  Env~p = lists:keyreplace(~p, 1, Env~p, {~p, ", [EnvIdx+1, Val, EnvIdx, Val]),
  io_lib:format("re:replace(proplists:get_value(~p, Env~p), ~p, ~p, [{return, binary}])", [Val, EnvIdx, Re, Replacement]),
  "}),\n"
  ],
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx+1, Acc ++ Code);

translate_commands([{set, Key, val, Value}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = [
  io_lib:format("  Env~p = lists:ukeymerge(1, [{~p,~p}], Env~p),\n", [EnvIdx+1, Key, Value, EnvIdx])
  ],
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx+1, Acc ++ Code);

translate_commands([{set, Key, var, Name}|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = [
  io_lib:format("  Env~p = lists:ukeymerge(1, [{~p,proplists:get_value(~p,Env~p)}], Env~p),\n", [EnvIdx+1, Key, Name, EnvIdx, EnvIdx])
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
  io_lib:format("    unhandled -> handle~p(Req~p, Env~p);\n", [FunIdx+1, ReqIdx, EnvIdx]),
  io_lib:format("    {unhandled, Req~p, Env~p} -> handle~p(Req~p, Env~p)\n", [ReqIdx+1, EnvIdx+1, FunIdx+1, ReqIdx+1, EnvIdx+1]),
  "  end.\n\n",

  "handle", integer_to_list(FunIdx+1), "(Req0, Env0) -> \n"
  ],
  translate_commands(Rest, FunIdx+1, 0, 0, Acc ++ Code);

translate_commands([_|Rest], FunIdx, ReqIdx, EnvIdx, Acc) ->
  translate_commands(Rest, FunIdx, ReqIdx, EnvIdx, Acc);

translate_commands([], FunIdx, ReqIdx, EnvIdx, Acc) ->
  Code = io_lib:format("  {unhandled, Req~p, Env~p}.\n\n", [ReqIdx, EnvIdx]),
  {ok, Acc ++ Code, FunIdx+1}.
