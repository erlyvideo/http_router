-module(http_router_handler).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([init/3, handle/2, terminate/2]).


init({_Transport, http}, Req, _Options) ->
  {ok, Req, state}.

handle(Req1, State) ->
  {Path, Req2} = cowboy_http_req:raw_path(Req1),
  Env = [{path,Path}],
  case http_router:handle(Req2, Env) of
    {ok, Req3} -> {ok, Req3, State};
    unhandled -> {ok, reply_404(Req2), State};
    {unhandled, Req3, _Env} -> {ok, reply_404(Req3), State}
  end.

reply_404(Req) ->
  {ok, Req1} = cowboy_http_req:reply(404, [], <<"404  not found\n">>, Req),
  Req1.

terminate(_,_) ->
  ok.



