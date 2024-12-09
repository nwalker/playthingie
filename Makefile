run: _build/default/lib/cowboy/rebar.config 
	./rebar3 shell

_build/default/lib/cowboy/rebar.config:
	./rebar3 get-deps

test:
	./rebar3 eunit
	./rebar3 ct
