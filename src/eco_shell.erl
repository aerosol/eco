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
            ref
        }).

-define(PROMPT, <<"eco $ ">>).
-define(PROMPT_CONT, <<"eco (...) $ ">>).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
                            spawn(fun shell_loop/0)
                    end } ]),
    io:format("~p~n", [Ref]),
    {ok, #state{
                ref = Ref
            }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

shell_loop() ->
    case get_input(?PROMPT) of
        Cmd ->
            io:format("~n~s: ~s~n", ["Unknown command", Cmd]),
            shell_loop()
    end.

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
