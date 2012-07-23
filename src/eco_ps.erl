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

%% @doc A simple gen_server wrapper around pg2 for basic publish/subscribe
-module(eco_ps).
-behaviour(gen_server).

-include("eco.hrl").

%% API
-export([subscribe/1]).
-export([publish/2]).
-export([init_group/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_group(Name) ->
    cast({init_group, Name}).

subscribe(To) ->
    call({subscribe, To}).

publish(To, Msg) ->
    cast({publish, {To, Msg}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, 1}.

handle_call({subscribe, Name}, {CallerPid, _}, State) ->
    Reply = g_join(Name, CallerPid),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({init_group, Name}, State) ->
    ok = pg2:create(groupname(Name)),
    {noreply, State};
handle_cast({publish, {Name, Msg}}, State) ->
    _ = [ Pid ! Msg || Pid <- pg2:get_local_members(groupname(Name)) ],
    {noreply, State};
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

call(Msg) ->
    gen_server:call(?MODULE, Msg).

cast(Msg) ->
    gen_server:cast(?MODULE, Msg).

-spec groupname(filename()) -> {eco, {subscribers, config_path()}}.
groupname(Name) when is_binary(Name) ->
    {eco, {subscribers, Name}}.

-spec g_join(filename(), pid()) -> ok.
g_join(Name, Pid) when is_pid(Pid) ->
    Group = groupname(Name),
    case lists:member(Pid, pg2:get_local_members(Group)) of
        true -> ok;
        false ->
            pg2:join(Group, Pid)
    end.

