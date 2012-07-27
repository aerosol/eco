-module(eco_shell_std).
-export([reload/1]).

reload(help) ->
    <<"Reload configuration.">>;
reload({completions, 1}) ->
    ["abc", "foo", "bar"];
reload([Filename]) ->
    eco:reload(Filename).
