-module(http_router_config).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([file/1]).


file(Path) ->
  case http_router_parser:file(Path) of
    {_ParsedConfig, Rest, {{line,Line},{column,Column}}} ->
      error_logger:error_msg("Couldn't parse config file ~s~nError is nearby here:~n~n~s~n~n", [Path, Rest]),
      {error, {Line,Column,Rest}};
    Config ->  
      {ok, parse_config(Config)}
  end.
  % Config.
  
parse_config(Config) ->
  Sections = [{Name, Value} || {section, Name, Value} <- Config],
  lists:map(fun
    ({location, Name, Path, Flags, Value}) -> 
      convert_location({location, Name, Path, Flags, parse_config(substitute_secion_includes(Value, Sections))});
    ({rewrite, Val, Re, Replacement}) ->
      convert_rewrite(Val, Re, Replacement);
    ({root, Root}) ->
      {set, root, val, Root};
    (file) ->
      {handler, static_file, send, []};
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


convert_rewrite(Val, Re, Replacement) ->
  {rewrite, Val, Re, re:replace(Replacement, "\\$(\\d+)", "\\\\\\1", [{return,binary}])}.


rewrite_route_entry(URL, Options) ->
  case re:run(URL,"(.*\\([^:]+)",[{capture,all_but_first,list},global]) of
    {match, _} -> erlang:throw({invalid_location_route,URL});
    _ -> ok
  end,
  
  {NewURL,VarList} = case re:run(URL,"\\(*:([-_0-9a-zA-Z]+)\\)*",[{capture,all_but_first,list},global]) of
    {match,Vars} -> {convert_to_pattern(URL,Options), [list_to_atom(Var) || [Var] <- Vars]};
    _ -> {convert_to_pattern(URL,Options)++".*",[]}
  end,
  {NewURL, VarList}.

convert_to_pattern(URL, Options)->
  Pattern1 = specify_regexps(URL, Options),
	re:replace(Pattern1,"(\\(*:[-_a-zA-Z0-9]+\\)*)","([^/]+)",[global,{return,list}]).

specify_regexps(URL,[])->
  URL;
  
specify_regexps(URL,[{Name,Pattern}|RegexList]) ->
  Re = lists:flatten(io_lib:format("\\(?:~s\\)?", [Name])),

  Pattern1 = re:replace(Pattern, "\\\\", "\\\\\\\\", [{return,list}]),

  Pat = lists:flatten(io_lib:format("(?<~s>~s)", [Name, Pattern1])),

  List=re:replace(URL, Re, Pat,[{return,list}]),
  % io:format("Replace ~p with ~p => ~p~n", [Re, Pat, List]),
  specify_regexps(List,RegexList).

  