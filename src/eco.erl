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

%% @doc Eco - Lightweight Erlang environment configuration server with Mnesia backend
-module(eco).
-behaviour(gen_server).

%% User API
-export([start/0]).
-export([initialize/0]).

-export([setup/1, setup/2]).
-export([terms/1]).
-export([term/2, term/3]).
-export([sub/1]).
-export([reload/1]).
-export([names/0]).

%% gen_server API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% private within the application scope
-export([find_snapshot/1]).

-include("eco.hrl").
-include_lib("stdlib/include/qlc.hrl" ).
-record(state, {
            config_dir
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the eco application

-spec start() -> {ok, pid()} | {error, Reason :: any()}.

start() ->
    application:start(eco).

%% @doc Initialize mnesia schema and create tables.
%%
%% This function should be called just once when first time initializing
%% your working environment.
%%
%% You can use the commandline switch <em>-eco_auto_init true</em>, so
%% eco will try to figure out if the schema has not been initialized
%% and make this call internally for you.

-spec initialize() -> ok.

initialize() ->
    eco_app:init_clean().

%% @doc Start configuration server loading files from <em>ConfigDir</em>.
%%
%% NOTE:
%% This function initializes configuration server in its basic state.
%% No file will be loaded unless you do a proper <em>setup/1</em> call.

-spec start_link(binary() | string()) -> {ok, pid()}.

start_link(ConfigDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfigDir], []).

%% @doc Fetch *all* terms from the latest configuration file snapshot.
%%
%% This function will fail with badarg if there is no snapshot at all,
%% indicating that configuration has not been initialized.

-spec terms(filename()) -> Terms :: [any()].

terms(Filename) when is_binary(Filename) ->
    case mnesia:dirty_read({eco_snapshot, Filename}) of
        [#eco_snapshot{terms = T}] -> T;
        _ -> throw({badarg, Filename})
    end;
terms(Filename) ->
    terms(to_binary(Filename)).

%% @doc Fetch a single term from the configuration file snapshot.
%%
%% Key is an %% arbitary term.
%% Returns atom <em>undefined</em> if no value has been found.

-spec term(Key :: any(), Filename :: filename()) -> Term :: term() | undefined.

term(Key, Filename) ->
    term(Key, Filename, undefined).

%% @doc Same as term/2 except allows to pass the default fallback value
%% in the third argument.

-spec term(Key :: any(), Filename :: filename(), Default :: term()) -> Term :: term().

term(Key, Filename, Default) when is_binary(Filename) ->
    case mnesia:dirty_read({eco_kv, {Filename, Key}}) of
        [#eco_kv{value = V}] -> V;
        [] -> Default
    end;
term(Key, Filename, Default) ->
    term(Key, to_binary(Filename), Default).

%% @doc Subscribe calling process to configuration reload notifications.
%%
%% This is optional. If you want to receive notifications about configuration
%% being reloaded, your process should accept message in a form of:
%%
%% {eco_reload, Filename :: filename()}
%%
%% You are responsible for making use of that information.
%% One common technique to deal with configuration reloads would be gracefully
%% stopping your notified process so the supervisor will restart it with new
%% configuration. If you are not happy with the idea of stopping your process,
%% most likely you should alter its state by manually fetching
%% the configuration values via the <em>term/2,3</em> or %% <em>terms/1</em>
%% interface.

-spec sub(Filename :: filename()) -> ok.

sub(Filename) ->
    eco_ps:subscribe(Filename).

%% @doc Do all the work related to proper configuration file initialization.
%%
%% This function does all the necessary checks and rewrites file configuration
%% into mnesia tables.
%%
%% It may use a custom adapter to parse the configuration file (i.e. JSON, YAML
%% or whatever format you can support with third party libraries).
%% Configuration adapter should be provided along with its module definition
%% with <em>[adapter, module()]</em> option.
%% A custom adapter should implement and export <em>process_config/1</em>
%% function that will return <em>{ok, Config}</em> on success.
%%
%% You may validate/transform your config on-the-fly using the <em>validators</em>
%% option. It accepts a list of functions in the form of funs or {M,F}.
%% Validation functions are expected to accept only one argument (Terms) and
%% return either {ok, Terms} (data checkers) or {ok, NewTerms} (transformers).
%% The validation functions will be called in the order they were provided,
%% passing the result from current to the latter (function chain).
%% To report a validation error, the validator function should return anything
%% that does not match the pattern of {ok, any()} or terminate abnormally.

-spec setup(Filename :: filename(), Opts :: opts()) ->
    {ok, Filename :: filename()} | {error, Reason :: any()}.

setup(Filename, Opts) when is_list(Opts) ->
    {ok, Eco = #eco_config{}} = parse_opts(Filename, Opts),
    call({setup_config, Eco}).

-spec setup(Filename :: filename()) ->
    {ok, Filename :: filename()} | {error, Reason :: any()}.

setup(Filename) when is_binary(Filename) ->
    setup(Filename, []);
setup(Filename) ->
    setup(to_binary(Filename), []).

%% @doc Reload previously initialized configuration file.
%%
%% This function does all the necessary checks and cleanups.
%% Current working configuration will be dumped into a file in $CONF/dump/
%% directory. Dump filename will be extended with current timestamp.

-spec reload(filename() | #eco_config{}) ->
    {ok, Filename :: filename()} |
    {fallback, {snapshot, calendar:datetime()}, {reason, any()}} |
    {error, Reason :: any()}.

reload(Filename) when is_binary(Filename) ->
    call({reload_config, Filename});
reload(#eco_config{name = Filename}) ->
    reload(Filename);
reload(Filename) ->
    reload(to_binary(Filename)).

%% @doc Fetch list of config filenames that were set up
-spec names() -> [filename()].

names() ->
    %% foldl would be just enough; leaving the gate open with qlc
    do_qlc(
        qlc:q([ Eco#eco_config.name || Eco <- mnesia:table(eco_config) ])
    ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ConfigDir]) ->
    {ok, #state{
            config_dir = ConfigDir
            }}.

handle_call({setup_config,
             E = #eco_config{
                    name = Name
                    }},
            _, State = #state{ config_dir = Dir }) ->
    Eco = E#eco_config{
            name = to_binary(Name),
            config_path = make_path({Dir, Name})
            },
    Reply = do_trans(Name,
                     fun() ->
                        eco_ps:init_group(Eco#eco_config.name),
                        ok = setup_mirror(Eco),
                        {ok, Name}
                     end),
    {reply, Reply, State};

handle_call({reload_config, Filename}, _, State = #state{config_dir = Dir})
        when is_binary(Filename) ->
    Reply = do_trans(Filename,
                     fun() ->
                        case mnesia:read(eco_config, Filename) of
                            [#eco_config{} = Eco] ->
                                ok = dump_current(Filename, Dir),
                                ok = clear_kvs(Eco),
                                ok = setup_mirror(Eco),
                                ok = eco_ps:publish(Filename,
                                                    {eco_reload, Filename}),
                                {ok, Filename};
                            [] ->
                                mnesia:abort({not_initialized, Filename})
                        end
            end),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = unknown_call,
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

call(Msg) ->
    gen_server:call(?MODULE, Msg).

-spec parse_opts(filename(), [opt()]) -> {ok, #eco_config{}}.

parse_opts(Filename, Opts) when is_list(Opts) ->
    parse_opts2(
        proplists:unfold(Opts),
        #eco_config{name = Filename}
        ).

parse_opts2([], Eco = #eco_config{}) ->
    {ok, Eco};
parse_opts2([{adapter, Module}|Rest], Eco = #eco_config{}) ->
    parse_opts2(Rest, Eco#eco_config{adapter = Module});
parse_opts2([{force_kv, Bool}|Rest], Eco = #eco_config{})
        when Bool =:= true; Bool =:= false ->
    parse_opts2(Rest, Eco#eco_config{force_kv = Bool});
parse_opts2([{validators, Validators}|Rest], Eco = #eco_config{}) ->
    %% I couldn not find any reliable way to validate the validators,
    %% since you can do F = fun erlang:nonexisting/1 and fun_info
    %% doesn't complain then. Late bindings FTW.
    %% We'll crash anyway, but wanted to do this earlier with
    %% user-friendly error message.
    parse_opts2(Rest, Eco#eco_config{validators = Validators});
parse_opts2([Opt|_], _) ->
    erlang:error({invalid_option, Opt}).

-spec ensure_dump_dir(binary()) ->
    {ok, binary()} | {error, {binary(), eacces | eexist | enoent | enospc | enotdir}}.

ensure_dump_dir(Dir) ->
    D = filename:join([Dir, <<"dump">>]),
    case filelib:is_dir(D) of
        true -> {ok, D};
        false ->
            case file:make_dir(D) of
                ok -> {ok, D};
                {error, Reason} ->
                    erlang:error({error, {D, Reason}})
            end
    end.

-spec make_dump_name(binary()) -> {ok, binary()}.

make_dump_name(Name) ->
    N = iolist_to_binary([Name, ".", format_time(erlang:localtime()), ".dump"]),
    {ok, N}.

-spec format_time(calendar:datetime()) -> string().

format_time({{Y,M,D},{H,Min,S}}) ->
    io_lib:format("~4.10.0B_~2.10.0B_~2.10.0B_~2.10.0B_~2.10.0B_~2.10.0B",
    [Y, M, D, H, Min, S]).

-spec dump_current(filename(), binary()) ->
    ok | {error, enoent | enotdir | enospc | eacces | eisdir}.

dump_current(Name, ConfigDir) ->
    {ok, #eco_snapshot{name = Name, raw = Raw}} = find_snapshot(Name),
    {ok, DumpDir} = ensure_dump_dir(ConfigDir),
    {ok, DumpFname} = make_dump_name(Name),
    Fname = filename:join(DumpDir, DumpFname),
    file:write_file(Fname, Raw).

-spec find_snapshot(filename()) -> {ok, #eco_snapshot{}} | undefined.

find_snapshot(Filename) ->
    case mnesia:transaction(
            fun() ->
                    mnesia:read(eco_snapshot, Filename)
            end) of
        {atomic, [Snapshot]} ->
            {ok, Snapshot};
        {atomic, []} ->
            error_logger:error_msg("No previous configuration snapshot found.~n"),
            undefined
    end.

-spec do_trans(Subject :: filename(), Transaction :: function()) ->
    {ok, any()} | {error, any()}.

do_trans(Filename, Transaction) when is_function(Transaction) ->
    case mnesia:transaction(Transaction) of
        {aborted, Reason} ->
            eco_fallback:handle(Filename, Reason);
        {atomic, {ok, Result}} ->
            {ok, Result}
    end.

-spec do_qlc(QLC :: term()) -> any().

do_qlc(QLC) ->
    F = fun() -> qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

-spec make_path({binary() | string(),binary() | [byte()]}) ->
    binary() | string().

make_path({Dir, Name}) ->
    filename:join([Dir, Name]).

-spec setup_mirror(#eco_config{}) -> ok | {error, any()}.

setup_mirror(#eco_config{name = Filename, config_path = CP,
                         adapter = A, force_kv = FKV, validators = V} = Eco) ->
    {ok, _} = ensure_file_ok(CP),
    {ok, {Raw, Terms}} = load_config(A, CP, V, FKV),
    mnesia:write(#eco_snapshot{
            name        = Filename,
            timestamp   = erlang:localtime(),
            raw         = Raw,
            terms       = Terms
            }),
    case Terms of
        T when is_list(T) ->
            _ = [ mnesia:write(#eco_kv{
                        key = Key,
                        value = Val
                        }) || {Key, Val} <- kv_unfold(Filename, T) ];
        _ ->
            error_logger:warning_msg("Configuration in '~s' eventually does not appear to be a list.~n"
                                     "Warning! eco:term/2 interface will not work as expected!~n", [Filename]),
            ignore
    end,
    mnesia:write(Eco),
    ok.

-spec clear_kvs(#eco_config{}) -> ok | {error, any()}.

clear_kvs(#eco_config{name = Filename}) ->
    KVs = mnesia:match_object({eco_kv, {Filename, '_'}, '_'}),
    _ = [ mnesia:delete_object(Obj) || Obj <- KVs ],
    ok.

-spec kv_unfold(filename(), list()) -> list().

kv_unfold(Filename, List) ->
    lists:map(fun({K,V}) -> {{Filename, K}, V};
            (Else) -> {{Filename, Else}, true}
        end, List).

-spec maybe_kv_check(term(), boolean()) -> term().

maybe_kv_check(Terms, false) -> Terms;
maybe_kv_check(Terms, true) when is_list(Terms) ->
    lists:map(fun({K,V}) -> {K,V};
            (T) -> erlang:error({kv_expected, T})
        end, Terms);
maybe_kv_check(Terms, true) ->
    erlang:error({kv_expected, Terms}).

-spec ensure_file_ok(config_path()) ->
    {ok, config_path()} | {error, {not_regular_file, config_path()}}.

ensure_file_ok(CP) ->
    case filelib:is_regular(CP) of
        true -> {ok, regular};
        false ->
            {error, {not_regular_file, CP}}
    end.

-spec load_config(A :: adapter(), CP :: config_path(),
    V :: validators(), FKV :: boolean()) -> {ok, {binary(), term()} | {error, any()}}.

load_config(A, CP, V, FKV) ->
    {ok, Raw} = file:read_file(CP),
    {ok, Terms} = load_config2(A, CP),
    case validate(Terms, V) of
        {ok, ValidTerms} ->
            NewTerms = maybe_kv_check(ValidTerms, FKV),
            {ok, {Raw, NewTerms}};
        Error ->
            {error, Error}
    end.

-spec load_config2(adapter(), config_path()) -> {ok, term()}.

load_config2(native, File) ->
    u_consult(File);
load_config2(Custom, File) when is_function(Custom) ->
    Custom(File);
load_config2({Mod, Fun}, File) ->
    Mod:Fun(File);
load_config2(Custom, File) ->
    Custom:process_config(File).

-spec validate(Terms :: [any()], Vs :: validators()) ->
    {ok, [any()]} | {error, {validation_error, any()}}.

validate(Terms, []) -> {ok, Terms};
validate(Terms, Vs) when is_list(Vs) ->
    {ok, lists:foldl(
            fun(Validator, NT) ->
                case f_call(Validator, NT) of
                    {ok, NT2} ->
                        NT2;
                    Error ->
                        erlang:error({validation_error, Error})
                end
            end,
            Terms, Vs)}.

f_call({M, F}, A) ->
    erlang:apply({M,F,A});
f_call(F, A) when is_function(F) ->
    F(A).

%% @doc Same as file:consult/1 except reads unicode

-spec u_consult(config_path()) -> {ok, term()} | {error, any()}.

u_consult(File) ->
    case file:open(File, [read, {encoding, unicode}]) of
        {ok, Fd} ->
            R = consult_stream(Fd),
            _ = file:close(Fd),
            R;
        Error ->
            Error
    end.

%% @doc lib/kernel-2.15.1/src/file.erl

consult_stream(Fd) ->
    consult_stream(Fd, 1, []).
consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
        {ok,Term,EndLine} ->
            consult_stream(Fd, EndLine, [Term|Acc]);
        {error, Error, Line} ->
            {error, {Error, Line}};
        {eof,_Line} ->
            {ok,lists:reverse(Acc)}
    end.

-spec to_binary(binary() | string()) -> binary().

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> iolist_to_binary(L).

%% @doc Calculate CRC of erlang binary term
%-spec make_checksum(list()) -> {ok, integer()}.
%make_checksum(L) ->
    %Sum = erlang:crc32(erlang:term_to_binary(L)),
    %{ok, Sum}.
