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

%% @doc Eco - Erlang environment configuration server supervisor
-module(eco_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_shell/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ConfigDir) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfigDir]).

start_shell() ->
    supervisor:start_child(?MODULE, { eco_shell,
                                     {eco_shell, start_link, []},
                                     permanent, 5000, worker,
                                     [eco_shell]} ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ConfigDir]) ->
    {ok, { {one_for_one, 5, 10}, [
                    {eco, {eco, start_link, [ConfigDir]}, permanent, 5000, worker, [eco]},
                    {eco_ps, {eco_ps, start_link, []}, permanent, 5000, worker, [eco_ps]}
                ]} }.

