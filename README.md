HTTP Router
===========


HTTP Router is an application that read nginx-like config file from specified file, tries to reload it regularly
and creates HTTP request processor.



What does it do?
---------------

It reads priv/routes.conf (or what have you configured to) and creates module http_router with function handle/2.
You are free to use it with your own webserver:

```erlang
Req = webserver_request(),
Env = [{path, calculate_request_path(Req)}],
case http_router:handle(Req, Env) of
  {ok, Req1} -> ... % Some module has handled request.
  unhandled ->      % nobody has handled and even modified request
  {unhandled, Req1, Env1} -> % nobody has handled, but request was modified
end
```

This http_router is handling all logic, you tried to put into routes.conf file.

Req object may be special for your webserver: cowboy, misultin or whatever else. It is up to You
to work with it properly inside your handlers

Env is a proplist with atom keys and binary values. You MUST put path key in it for locations code to work.
You are free to put anything in it and modify later.


Structure of config file 
----------------

Config file consists of commands.

Commands may be:

# instructions
# locations 
# sections

instructions are lines that are translated to some Env modifiers: set, rewrite or some instructions like calling other module.

locations are "if" instructions for your path.
sections are just templates, that can be inserted into any other location with instruction "include @section;"


Router goes line by line, entering locations if they are matching current path and evaluating all instructions until it
meets handler call, that returns with {ok, Req}.


Syntax of config file
----------

Take a look at real example in priv/routes.conf. You can refer to it.







Currently there is a Cowboy handler:


```erlang
application:load(http_router),
application:set_env(http_router, config_path, "priv/flussonic.conf"),
application:set_env(http_router, frequency, 1000),
application:start(http_router),
application:start(cowboy),

Dispatch = [
	{'_', [
   {['...'], http_router_handler, []}
	]}
],
cowboy:start_listener(http, 100,
	cowboy_tcp_transport, [{port, 8080}],
	cowboy_http_protocol, [{dispatch, Dispatch}]
),

```




You can add your own handlers with command:

```
handler module function;
```

and you will be called as  module:function(Req, Env) where Req is your webserver request (currently cowboy) and
Env is a proplist with some special variables like regexp matched entries from location.
