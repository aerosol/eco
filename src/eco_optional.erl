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

%% @doc Module produces compilation warnings if optional dependencies required
%% by adapters have not been fetched. Adapters should be compiled with
%% {parse_transform, eco_optional} flag.
%%
%% The -require([module()]) module attribute indicates dependency list that
%% will be checked against code:ensure_loaded/1 function.

-module(eco_optional).
-export([parse_transform/2]).
-export([format_error/1]).

parse_transform(AST, _Options) ->
    walk_ast(AST, []).

walk_ast([], Acc) ->
    lists:reverse(Acc);
walk_ast([{attribute, _, module, Mod}=H|T], Acc) ->
    put(mod, Mod),
    walk_ast(T, [H|Acc]);
walk_ast([{attribute, Line, require, Deps}=H|T], Acc) ->
    case ensure_loaded(Deps) of
        {error, {Dep, Err}} ->
            Warn = {warning,{Line, eco_optional, {deps_missing, get(mod), Dep, Err}}},
            walk_ast(T, [Warn|Acc]);
        ok ->
            walk_ast(T, [H|Acc])
    end;
walk_ast([H|T], Acc) ->
    walk_ast(T, [H|Acc]).

ensure_loaded([]) ->
    ok;
ensure_loaded([Dep|Rest]) ->
    case code:ensure_loaded(Dep) of
        {module, Dep} ->
            ensure_loaded(Rest);
        {error, Err} ->
            {error, {Dep, Err}}
    end.

format_error({deps_missing, Mod, Dep, Err}) ->
    io_lib:format("Adapter '~p' could not load '~p' dependency (~p). "
                  "Adapters are optional; you can ignore this warning.~n", [Mod, Dep, Err]).




