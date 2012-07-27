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

-define(PROMPT, <<"eco $ ">>).
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
    ok = eco:setup(<<"eco_shell.conf">>),
    Port = eco:term(ssh_port, <<"eco_shell.conf">>, 2222),
    SysDir = eco:term(ssh_sys_dir, <<"eco_shell.conf">>, "priv/ssh-sys"),
    UserDir = eco:term(ssh_user_dir, <<"eco_shell.conf">>, "priv/ssh-usr"),
    {ok, Ref} = ssh:daemon(Port, [{system_dir, SysDir},
                            {user_dir, UserDir},
                            {shell, fun() ->
                            spawn(fun start_shell/0)
                    end } ]),
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
    ssh:stop_daemon(Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initialize_cache() ->
    ?CACHE = ets:new(?CACHE, [ordered_set, named_table, protected]).

extract_commands(Module) when is_atom(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    extract_commands({Module, Module:module_info(exports)}, []).

extract_commands({_, []}, Acc) -> Acc;
extract_commands({Module, [{Fun, 1}|Rest]}, Acc) when Fun =/= module_info ->
    extract_commands({Module, Rest}, [{erlang:atom_to_list(Fun), {Fun, Module}}|Acc]);
extract_commands({Module, [_|Rest]}, Acc) ->
    extract_commands({Module, Rest}, Acc).

start_shell() ->
    io:setopts([{expand_fun, fun expand/1}]),
    shell_loop().

shell_loop() ->
    case get_input(?PROMPT) of
        Cmd ->
            io:format("Unknown command: ~p~n", [Cmd]),
            shell_loop()
    end.

find_commands_by_prefix(Prefix) ->
    case ets:match_object(?CACHE, {Prefix++'_', '_'}) of
        [ {_,_}, _|_ ] = Commands -> [ C || {C, _} <- Commands ];
        [ {C, _} ] -> [C];
        [] -> []
    end.

return_to_shell([], _) ->
    {no, "", []};
return_to_shell([One], Pref) when is_list(One) ->
    {yes, lists:flatten([One--Pref, " "]), []};
return_to_shell(Multiple, _) when is_list(Multiple) ->
    {yes, "", Multiple}.

expand({full, [], Pref, 0}) ->
    return_to_shell(find_commands_by_prefix(Pref), Pref);
expand({full, Cmd, Pref, Argv}) ->
    [{_, {Fun, Mod}}] = ets:lookup(?CACHE, Cmd),
    try
        Completions = Mod:Fun({completions, Argv}),
        Matching = lists:filter(
            fun(Arg) ->
                lists:prefix(Pref, Arg)
            end,
            Completions),
        return_to_shell(Matching, Pref)
    catch error:E when E =:= function_clause; E =:= undef ->
            return_to_shell([], [])
    end;
expand({args, Cmd, Prefix, Argv}) ->
    expand({full, lists:reverse(Cmd), lists:reverse(Prefix), Argv});
expand({partial, [" ", Cmd]}) when is_list(Cmd) ->
    expand({args, Cmd, "", 1});
expand({partial, [Pref, Cmd]}) when is_list(Cmd) ->
    expand({args, Cmd, Pref, 1});
expand({partial, [Cmd]}) when is_list(Cmd) ->
    expand({args, "", Cmd, 0});
expand({partial, [Pref|L]}) when is_list(Pref) ->
    Cmd = hd(lists:reverse(L)),
    expand({args, Cmd, Pref, length(L)});
expand(" " ++ Cmd) when is_list(Cmd) ->
    expand({partial, [" " | string:tokens(Cmd, " ")]});
expand([]) ->
    return_to_shell(find_commands_by_prefix(""), "");
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
