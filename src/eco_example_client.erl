-module(eco_example_client).

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
            user,
            pass
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    User = eco:term(username, <<"example.conf">>, <<"Anonymous">>),
    Pass = eco:term(password, <<"example.conf">>),
    ok = eco:sub(<<"example.conf">>),
    io:format("Starting example server (~p).~n"
              "Credentials read from 'conf/example.conf':"
              "U: ~p P: ~p~n", [self(), User, Pass]),
    {ok, #state{
            user = User,
            pass = Pass
            }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({eco_reload, Filename}, State) ->
    io:format("Configuration reload received by (~p) ~p: ~p~n"
              "Time to roll!~n",
              [self(), ?MODULE, Filename]),
    %% Let the supervisor take care of this
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("Unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Eco example client terminating...~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
