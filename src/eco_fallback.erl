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

%% @doc Fallback module providing snapshot recovery in case of disk configuration
%% failures and human-friendly error messages.
-module(eco_fallback).
-include("eco.hrl").
-export([handle/2]).

%% @doc Try explain the error to the user and run configuration fallback if possible.
%%
%% This functions tries to find human-readable error explanation and seeks
%% for previous configuration snapshot in the mnesia database.
%% If snapshot is found it is used producing a log info message, if not -
%% the function fails with the original reason.

-spec handle(Filename :: filename(), Reason :: any()) ->
    {fallback, {snapshot, calendar:datetime()}, {reason, any()}} | {error, Reason :: any()}.
handle(Filename, Reason) ->
    try_explain(Filename, Reason),
    case eco:find_snapshot(Filename) of
        {ok, #eco_snapshot{timestamp = TS}} ->
            error_logger:info_msg("Falling back to configuration snapshot from ~p~n", [TS]),
            {fallback, {snapshot, TS}, {reason, Reason}};
        undefined ->
            error_logger:error_msg("Could not find any previous configuration"
                                   " snapshot for '~s'~n", [Filename]),
            {error, Reason}
    end.

-spec try_explain(filename(), term()) -> ok.
try_explain(Filename, {{validation_error, Reason}, _Stacktrace}) ->
    error_logger:error_msg("Configuration data in '~s' did not validate: ~p~n", [Filename, Reason]);
try_explain(Filename, {{_, {error,{L1, erl_parse, E}, L2}}, _}) ->
    error_logger:error_msg("Error processing configuration file '~s'. ~n"
                           "Corrupted term is close to lines ~p or ~p. ~n"
                           "The error message was: '~s'~n~n", [Filename, L1, L2, E]);
try_explain(Filename, {undef, [{Adapter, process_config, _, _}|_]}) ->
    case code:ensure_loaded(Adapter) of
        {module, Adapter} ->
            case erlang:function_exported(Adapter, process_config, 1) of
                false ->
                    error_logger:error_msg(
                        "Adapter ~s does not export configuration processing function. ~n"
                        "(~s:process_config/1). Unable to process configuration",
                        [Adapter, Adapter])
            end;
        {error, What} ->
            error_logger:error_msg(
                    "Could not load adapter module: ~p (~p). ~n"
                    "File '~s' cannot be processed.", [Adapter, What, Filename]
                )
        end;
try_explain(Filename, {Error, [{Adapter, process_config, _, _}|_]}) ->
    error_logger:error_msg(
        "Adapter ~s wasn't able to process '~s': ~p~n", [Adapter, Filename, Error]);
try_explain(Filename, {{kv_expected, InvalidTerm}, _}) ->
    error_logger:error_msg(
        "Configuration file '~s' contains non key-value entries. ~n"
        "Invalid entry: ~p~n", [Filename, InvalidTerm]);
try_explain(Filename, {{badmatch, Error}, _}) ->
            error_logger:error_msg("Eco was expecting the adapter "
                                   "processing '~s' to return {ok, Conf} ~n"
                                   "Got ~p instead.~n", [Filename, Error]);
try_explain(Filename, {not_initialized, Filename}) ->
    error_logger:error_msg("Configuration file '~s' has not been initialized yet.", [Filename]);
try_explain(Filename, Error) ->
    error_logger:error_msg("Could not determine error details processing file '~s'.~n"
                           "Please file an issue at https://github.com/aerosol/eco/~n"
                           "Error reason caught was: ~p~n", [Filename, Error]).


