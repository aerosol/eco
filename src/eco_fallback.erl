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

handle(Eco = #eco_config{ config_path = CP}, Reason) ->
    try_explain(Eco, Reason),
    case eco:find_snapshot(Eco) of
        {ok, #eco_snapshot{timestamp = TS}} ->
            error_logger:info_msg("Falling back to configuration snapshot from ~p~n",
                                  [TS]),
            ok;
        undefined ->
            error_logger:error_msg("Could not find any previous configuration snapshot for ~s~n",
                                   [CP]),
            {error, Reason}
    end.

-spec try_explain(#eco_config{}, term()) -> ok.
try_explain(#eco_config{config_path = CP},
            {{_, {error,{L1, erl_parse, E}, L2}}, _}) ->
    error_logger:error_msg("There is an error in configuration data of ~s. ~n"
                           "Corrupted term is close to lines ~p or ~p. ~n"
                           "The error message was: '~s'~n~n", [CP, L1, L2, E]);
try_explain(#eco_config{config_path = CP},
            {undef, [{Adapter, process_config, _, _}|_]}) ->
    case code:ensure_loaded(Adapter) of
        {module, Adapter} ->
            case erlang:function_exported(Adapter, process_config, 1) of
                false ->
                    error_logger:error_msg(
                        "Adapter ~p does not export configuration processing function. ~n"
                        "(~p:process_config/1). Unable to process configuration",
                        [Adapter, Adapter])
            end;
        {error, What} ->
            error_logger:error_msg(
                    "Could not load adapter module: ~p (~p). ~n"
                    "Configuration ~s could not be processed.", [Adapter, What, CP]
                )
        end;
try_explain(#eco_config{config_path = CP, adapter = Adapter},
                {Error, [{Adapter, process_config, _, _}|_]}) ->
    error_logger:error_msg(
        "Adapter '~s' wasn't able to process ~s: ~p~n", [Adapter, CP, Error]);
try_explain(#eco_config{config_path = CP}, {{kv_expected, InvalidTerm}, _}) ->
    error_logger:error_msg(
        "Configuration file ~s contains non key-value entries. ~n"
        "Invalid entry: ~p~n", [CP, InvalidTerm]);
try_explain(#eco_config{config_path = CP}, {{badmatch, Error}, _}) ->
            error_logger:error_msg("Eco was expecting the adapter processing ~s to return {ok, Conf} ~n"
            "Got ~p instead.~n", [CP, Error]);
try_explain(#eco_config{config_path = CP}, Error) ->
    error_logger:error_msg("~s - Could not determine error details.~n"
                           "Please file an issue at https://github.com/aerosol/eco/~n"
                           "Error reason caught was: ~p~n", [CP, Error]).

