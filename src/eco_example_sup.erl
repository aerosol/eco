
-module(eco_example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("eco.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    F = <<"example.conf">>,
    ok = eco:setup(#eco_config{
                name = F,
                force_kv = true
                }),
    {ok, { {one_for_one, 5, 10}, [
                {eco_example_client,
                 {eco_example_client, start_link, []},
                 permanent, 5000, worker,
                 [eco_example_client]}
                ]} }.

