%% Copyright (c) 2012, Adam Rutkowski <hq@mtod.org>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.OF SUCH DAMAGE.

%% @doc Extensible SSH CLI
-module(eco_shell).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
            daemon_ref,
            commands = []
        }).

-define(PROMPT, "eco $ ").
-define(CACHE, eco_shell_commands).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_module(Mod) when is_atom(Mod) ->
    gen_server:cast(?MODULE, {register, Mod}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% TODO allow overwrite all ssh opts
    {ok, F} = eco:setup(<<"eco_shell.conf">>),
    Port = eco:term(ssh_port, F, 2222),
    SysDir = eco:term(ssh_sys_dir, F, "priv/ssh-sys"),
    UserDir = eco:term(ssh_user_dir, F, "priv/ssh-usr"),
    {ok, Ref} = ssh:daemon(Port, [{system_dir, SysDir},
                                  {user_dir, UserDir},
                                  {shell, fun start_shell/0}
                                 ]),
    initialize_cache(),
    register_module(eco_shell_std),
    {ok, #state{
                daemon_ref = Ref
            }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register, Module}, State) ->
    true = ets:insert(?CACHE, extract_commands(Module)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ daemon_ref = Ref }) ->
    _ = ssh:stop_daemon(Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initialize_cache() ->
    ?CACHE = ets:new(?CACHE, [ordered_set, named_table, protected]).

%% @doc Loop over all exported functions (except for module_info)
%% and build map of {Module :: string(), {Function :: atom(), Module :: module()}}
extract_commands(Module) when is_atom(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    extract_commands({Module, Module:module_info(exports)}, []).

extract_commands({_, []}, Acc) -> Acc;
extract_commands({Module, [{Fun, 1}|Rest]}, Acc) when Fun =/= module_info ->
    extract_commands({Module, Rest}, [{erlang:atom_to_list(Fun), {Fun, Module}}|Acc]);
extract_commands({Module, [_|Rest]}, Acc) ->
    extract_commands({Module, Rest}, Acc).

%% @doc Spawn REPL loop
start_shell() ->
    spawn(fun() ->
                ok = io:setopts([{expand_fun, fun expand/1}]),
                _ = shell_loop()
        end).

%% @doc REPL loop
shell_loop() ->
    case get_input(?PROMPT) of
        Quit when Quit =:= "exit"; Quit =:= "quit" ->
            io:format("Bye!~n"),
            {ok, exit};
        Cmd ->
            [Command|Args] = string:tokens(Cmd, " "),
            case get_command(Command) of
                [{Command, {F,M}}] ->
                    try M:F(Args) of
                        Result -> reply(Result)
                    catch error:function_clause ->
                            reply({error, <<"Invalid argument(s).">>});
                        Class:Error ->
                            reply({error, {Class, Error}})
                    end;
                _ ->
                  io:format("Command not recognized: ~s~n", [Command])
            end,
            shell_loop()
    end.

reply({ok, Result}) when is_binary(Result); is_atom(Result) ->
    io:format("OK: ~s~n", [Result]);
reply({ok, Result}) ->
    io:format("OK: ~p~n", [Result]);
reply(Result) when is_binary(Result); is_atom(Result) ->
    io:format("~s~n", [Result]);
reply({error, Result}) when is_binary(Result); is_atom(Result) ->
    io:format("ERROR: ~s~n", [Result]);
reply({error, Result}) ->
    io:format("ERROR: ~p~n", [Result]);
reply(Term) ->
    io:format("~p~n", [Term]).

get_command(Command) ->
    ets:lookup(?CACHE, Command).

find_commands_by_prefix(Prefix) ->
    case ets:match_object(?CACHE, {Prefix++'_', '_'}) of
        [ {_,_}, _|_ ] = Commands -> [ C || {C, _} <- Commands ];
        [ {C, _} ] -> [C];
        [] -> []
    end.

%% @doc Return completion suggestions to SSH shell (ssh_cli)
suggest([], _) ->
    io:format("S: ~p~n", [[]]),
    {no, "", []};
suggest([One], {Prefix, _}) when is_list(One) ->
    io:format("S: ~p~n", [One--Prefix]),
    {yes, lists:flatten([One--Prefix, " "]), []};
suggest([One], {_, {Bpref, BprefS}}) when is_binary(One) ->
    <<Bpref:BprefS/binary, Suffix/binary>> = One,
    io:format("S: ~p~n", [Suffix]),
    {yes, Suffix, []};
suggest(Multiple, _) when is_list(Multiple) ->
    %% FIXME cannot return binaries, aaargh!
    io:format("S/M: ~p~n", [Multiple]),
    {yes, "", Multiple}.

%% @doc Try to find matching commands or parameters
expand({full, [], Prefix, 0}) ->
    suggest(find_commands_by_prefix(Prefix), {Prefix, ignore});
expand({full, Cmd, Prefix, Argv}) ->
    [{_, {Fun, Mod}}] = ets:lookup(?CACHE, Cmd),
    try
        %% allow Mod:Fun({completions, _}) to return binaries as well
        Bpref = erlang:list_to_binary(Prefix),
        BprefS = erlang:byte_size(Bpref),

        %% get all available completions
        Completions = Mod:Fun({completions, Argv}),

        io:format("bprefix: ~p (~p)~n", [Bpref, BprefS]),

        %% find matching and suggest matching completions
        Matching = lists:filter(
            fun(Arg) when is_list(Arg) -> lists:prefix(Prefix, Arg);
               (<<B:BprefS/binary, _/binary>>) when B =:= Bpref -> true;
               (_) -> false
            end,
            Completions),
        io:format("Matching ~p~n", [Matching]),
        suggest(Matching, {Prefix, {Bpref, BprefS}})
    catch error:E when E =:= undef ->
        io:format("Error ~p~n", [E]),
        suggest([], {[], ignore})
    end;
expand({args, Cmd, Prefix, Argv}) ->
    expand({full, lists:reverse(Cmd), lists:reverse(Prefix), Argv});
expand({partial, [" ", Cmd]}) when is_list(Cmd) ->
    expand({args, Cmd, "", 1});
expand({partial, [Prefix, Cmd]}) when is_list(Cmd) ->
    expand({args, Cmd, Prefix, 1});
expand({partial, [Cmd]}) when is_list(Cmd) ->
    expand({args, "", Cmd, 0});
expand({partial, [Prefix|L]}) when is_list(Prefix) ->
    Cmd = hd(lists:reverse(L)),
    expand({args, Cmd, Prefix, length(L)});
expand(" " ++ Cmd) when is_list(Cmd) ->
    expand({partial, [" " | string:tokens(Cmd, " ")]});
expand([]) ->
    suggest(find_commands_by_prefix(""), {"", ignore});
expand(Cmd) when is_list(Cmd) ->
    expand({partial, string:tokens(Cmd, " ")}).

get_input(Prompt) ->
    trim(io:get_line(Prompt)).

trim(Line) when is_list(Line) ->
    lists:reverse(trim1(lists:reverse(trim1(Line))));
trim(Other) -> Other.

trim1([$\s|Cs]) -> trim(Cs);
trim1([$\r|Cs]) -> trim(Cs);
trim1([$\n|Cs]) -> trim(Cs);
trim1([$\t|Cs]) -> trim(Cs);
trim1(Cs) -> Cs.

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").

auto_completion_test_() ->
    {setup,
     fun() ->
                ?CACHE = initialize_cache(),
                true = ets:insert(?CACHE, [
                            {"reload", {reload, eco_shell_std}},
                            {"newline",{io, nl}},
                            {"nop",    {foo, bar}}
                            ]),
                setup_ok
        end,
     fun(setup_ok) ->
                Tests = [
                        { {yes, "eload ", []}, "r" },
                        { {yes, "oad ", []}, "rel" },
                        { {yes, "bc ", []}, "reload a" },
                        { {yes, "wline ", []}, "ne" },
                        { {yes, "", ["newline", "nop"]}, "n" },
                        { {yes, "", eco_shell_std:reload({completions, 1}) }, ("reload ") },
                        { {no, [], []}, "newline " },
                        { {no, [], []}, "reload a " },
                        { {no, [], []}, "reload abc y" },
                        { {no, [], []}, "x" }
                        ],
                [ { iolist_to_binary(["User input: ", I]), fun() ->
                        ?assertEqual(R, expand(lists:reverse(I)))
                    end} || {R, I} <- Tests ]
        end}.

-endif.
