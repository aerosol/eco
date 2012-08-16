-module(eco_shell_std).
-export([reload/1]).
-export([hi/1]).

reload(help) ->
    {reply, <<"Reload configuration.">>};

reload({completions, 1}) ->
    %eco:names();
    ["elo", <<"jelo">>];

reload([Filename]) ->
    eco:reload(Filename);

reload([_, _]) ->
    erlang:error(wussup).

hi([Name]) ->
    io_lib:format("hi ~s", [Name]);
hi({completions, 1}) ->
    ["adam"].

