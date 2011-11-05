%%%-------------------------------------------------------------------
%%% @author tthread <ilya@erlyvideo.org>
%%% Created :  3 Nov 2011 by tthread <ilya@erlyvideo.org>
%%%-------------------------------------------------------------------
-module(routes_proper_tests).

-export([prop_tests/0]).
-import(routes,[get_routes/1,get_params/1,parse/2]).
-include_lib("proper/include/proper.hrl").
-include("routes.hrl").

hostname_head_char() ->
  frequency([{50,choose($a,$z)},{25,choose($A,$Z)},{25,choose($0,$9)}]).

hostname_char() ->
  frequency([{5,$-},{25,choose($a,$z)},{25,choose($A,$Z)},{25,choose($0,$9)}]).

variable_char() ->
  frequency([{5,$-},{25,choose($a,$z)},{25,choose($A,$Z)}]).


hostname_label() ->
  ?SUCHTHAT(Label, [hostname_head_char()|list(hostname_char())],
	    length(Label) < 64).

hostname() ->
  ?SUCHTHAT(Hostname,
	    ?LET(Labels, list(hostname_label()), string:join(Labels, ".")),
	    length(Hostname) > 0 andalso length(Hostname) =< 255).

port_number() ->
  choose(1, 16#ffff).

port_str() ->
  oneof(["", ?LET(Port, port_number(), ":" ++ integer_to_list(Port))]).

server() ->
  ?LET({Hostname, PortStr}, {hostname(), port_str()}, Hostname ++ PortStr).


path() ->
  ?LET(Num,nat(),vector(Num,hostname_label())).

variable_char_vector() ->
  ?LET(Num,nat(),vector(Num+1,variable_char())).

variable() ->
  ?LET({Var,Text1,Text2},{variable_char_vector(),variable_char_vector(),variable_char_vector()},
       {Var,Text1++Var++Text2,Text1++"(:"++Var++")"++Text2}
    ).

variable_list() ->
  ?LET(Nat,nat(),vector(Nat,variable())).

complex_variable() ->
  ?LET(
     VariableList,variable_list(),
     begin
       UrlValues= [Element ||{_,Element,_}<-VariableList],
       UrlVars = [Element || {_,_,Element}<-VariableList],
       Values = [Element || {Element,_,_}<-VariableList],
       {Values,lists:merge(UrlValues),lists:merge(UrlVars)} 
     end).

variable_complex_list()->
  ?SUCHTHAT(V,?LET(List,list(complex_variable()),List),length(V)<255).

get_params_prop_test() ->
  ?FORALL(
     {Server,Path}, {server(),path()},
     begin
       get_params("http://"++Server++"/"++string:join(Path,"/"))=:=string:join(Path,"/")
     end). 

parse_complex_prop_test() ->
  ?FORALL(
     {Server,Vars},{server(),variable_complex_list()},
     begin
       RoutePath = [Element || {_,_,Element}<-Vars],
       Vv = [{Element, Element} || {[Element],_,_}<-Vars],
       URLPath = [Element || {_,Element,_}<-Vars],
       URL = "http://"++Server++"/"++string:join(URLPath,"/"),
       Route = [[{string:join(RoutePath,"/"),handler,method}]],
       case parse(URL,#routes{routes=get_routes(Route)}) of
	 {handler,method,Vv} ->
	   true;
	 {error,route_not_found} ->
	   true;
	 _ ->
	   false
       end
     end).

prop_tests()->
  proper:quickcheck(get_params_prop_test()),
  proper:quickcheck(parse_complex_prop_test()).
