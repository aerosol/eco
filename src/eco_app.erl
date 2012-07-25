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

%% @doc Eco - Erlang environment configuration server (application module)
-module(eco_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).
-export([init_clean/0]).

-include("eco.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(eco).

init_clean() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mnesia:delete_table(eco_config),
    mnesia:delete_table(eco_snapshot),
    mnesia:delete_table(eco_kv),
    mnesia:create_table(eco_config, [
            {type, set},
            {attributes, record_info(fields, eco_config)},
            {disc_copies, [node()]}
            ]),
    mnesia:create_table(eco_snapshot, [
            {type, set},
            {attributes, record_info(fields, eco_snapshot)},
            {disc_copies, [node()]}
            ]),
    mnesia:create_table(eco_kv, [
            {type, set},
            {attributes, record_info(fields, eco_kv)},
            {disc_copies, [node()]}
            ]),
    stopped = mnesia:stop(),
    ok.

start(_StartType, StartArgs) ->
    application:start(pg2),
    application:start(mnesia),
    case mnesia:wait_for_tables([eco_snapshot, eco_kv], 5000) of
        ok ->
            ConfigDir = proplists:get_value(config_dir, StartArgs),
            Ret = eco_sup:start_link(ConfigDir),
            start_plugins(proplists:get_value(plugins, StartArgs, [])),
            Ret;
        Error ->
            error_logger:error_msg("Eco could not initialize Mnesia tables.~n"
                                   "Possible solution: run eco_app:init_clean/0~n"
                                   "The error message was: ~n~p~n", [Error]
                                  ),
            Error
    end.

start_plugins([]) ->
    ok;
start_plugins([shell|Rest]) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssh),
    eco_sup:start_shell(),
    start_plugins(Rest);
start_plugins([Unknown|_]) ->
    erlang:error({unknown_eco_plugin, Unknown}).

stop(_State) ->
    ok.
