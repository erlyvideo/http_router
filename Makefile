
test:
	ERL_LIBS=../../deps rebar compile
	erl -pa ebin -noshell -eval 'io:format("~p~n", [http_router_config:file("../../priv/flussonic.conf")])' -s init stop

tests:
	erl -pa ebin -pa ../../deps/proper/ebin -noshell -eval 'io:format("~p~n", [routes_proper_tests:prop_tests()])' -s init stop

raw:
	ERL_LIBS=../../deps rebar compile
	erl -pa ebin -noshell -eval 'io:format("~p~n", [http_router_parser:file("../../priv/flussonic.conf")])' -s init stop

code:
	ERL_LIBS=../../deps rebar compile
	erl -pa ebin -noshell -eval 'io:format("~n~n~s~n", [element(2,http_router_compiler:generate_router("../../priv/flussonic.conf"))])' -s init stop
