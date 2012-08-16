-module(eco_example).
-export([start/0]).

start() ->
    ok = application:start(eco),
    eco_example_sup:start_link().

