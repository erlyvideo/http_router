HTTP Router
===========


HTTP Router is an application that read nginx-like config file from specified file, tries to reload it regularly
and process HTTP request from the beginning to the latest module.

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

Now take a look at self-explanatory priv/routes.conf


You can add your own handlers with command:

```
handler module function;
```

and you will be called as  module:function(Req, Env) where Req is your webserver request (currently cowboy) and
Env is a proplist with some special variables like regexp matched entries from location.
