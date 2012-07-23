-module(eco_example).
-export([start/0]).

start() ->
    eco_app:init_clean(), %% do this once -- from the shell
    ok = application:start(eco),
    eco_example_sup:start_link().

