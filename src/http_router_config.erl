-module(http_router_config).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([file/1]).


file(Path) ->
  Config = http_router_parser:file(Path),
  parse_config(Config).
  % Config.
  
parse_config(Config) ->
  Sections = [{Name, Value} || {section, Name, Value} <- Config],
  lists:map(fun
    ({location, Name, Path, Flags, Value}) -> 
      convert_location({location, Name, Path, Flags, parse_config(substitute_secion_includes(Value, Sections))});
    ({rewrite, Re, Replacement}) ->
      convert_rewrite(Re, Replacement);
    ({root, Root}) ->
      {set, root, Root};
    (file) ->
      {handler, static_file, send, []};
    ({hds,Command}) ->
      {handler, hds_handler, Command, []};
    ({hls,Command}) ->
      {handler, hls_handler, Command, []};
    ({rack,Path}) ->
      {handler, cowboy_rack_handler, handle, [Path]};
    (Else) -> Else
  end, Config).


substitute_secion_includes(Commands, Sections) ->
  lists:foldr(fun
    ({include_section, Section}, Acc) -> proplists:get_value(Section, Sections) ++ Acc;
    (Command, Acc) -> [Command|Acc]
  end, [], Commands).


convert_location({location, Name, Path, Flags, Instructions}) ->
  Replacement = [{binary_to_atom(Key,latin1), Value} || {re, Key, Value} <- Flags],
  Re = rewrite_route_entry(Path, Replacement),
  ProperFlags = lists:filter(fun
    ({re,_,_}) -> false;
    (_) -> true
  end, Flags),
  {location, Name, Re, ProperFlags, Instructions}.


convert_rewrite(Re, Replacement) ->
  {rewrite, Re, re:replace(Replacement, "\\$(\\d+)", "\\\\\\1", [{return,binary}])}.


rewrite_route_entry(URL, ExList) ->
  {{ok,NewURL},VarList} = case re:run(URL,"\\(:[^\\)]*\\(") of
    nomatch ->
      case re:run(URL,"\\(*:([-_0-9a-zA-Z]+)\\)*",[{capture,all_but_first,list},global]) of
        {match,Vars} -> {convert_to_pattern(URL,ExList), [list_to_atom(Var) || [Var] <- Vars]};
        _ -> {convert_to_pattern(URL,ExList),[]}
      end; 
    {match,_} ->
      {{ok,"parsing_error"},[]}
  end,
  {NewURL, VarList}.

convert_to_pattern(URL,ExList)->
  Convert = fun() ->
	  Pattern1=exlist(URL,ExList),
  	case re:replace(Pattern1,"(\\(*:[-_a-zA-Z0-9]+\\)*)","([^/]+)",[global,{return,list}]) of
  	  Value2 when is_list(Value2) -> {ok,Value2};
  	  nomatch -> ""
  	end
  end,
  case re:run(URL,"(.*\\([^:]+)",[{capture,all_but_first,list},global]) of
    nomatch ->
      Convert();
    {match,Error} ->
      io:format("Route rule error near ~p~n",[Error]),
      error
  end.

exlist(URL,[])->
  URL;
exlist(URL,[{Name,Pattern}|ExList]) ->
  Re = lists:flatten(io_lib:format("\\(?:~s\\)?", [Name])),

  Pattern1 = re:replace(Pattern, "\\\\", "\\\\\\\\", [{return,list}]),

  Pat = lists:flatten(io_lib:format("(?<~s>~s)", [Name, Pattern1])),

  List=re:replace(URL, Re, Pat,[{return,list}]),
  % io:format("Replace ~p with ~p => ~p~n", [Re, Pat, List]),
  exlist(List,ExList).

  