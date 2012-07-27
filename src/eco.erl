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

-export([setup/1]).
-export([terms/1]).
-export([term/2, term/3]).
-export([sub/1]).
-export([reload/1]).

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
-spec start() -> {ok, pid()}.
start() ->
    application:start(eco).

%% @doc Initialize mnesia schema and create tables.
%%
%% This function should be called just once when first time initializing
%% your working environment.
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
%% within record's <em>adapter</em> field. A custom adapter should
%% implement and export <em>process_config/1</em> function that will
%% return <em>{ok, Config}</em> on success.
%%
%% Calling <em>setup/1</em> function with <em>#eco_config{}</em> record means
%% you would like to customize things like key-value enforcement,
%% adapter or validators.
%% If you are OK with the provided defaults there is no need to create a record,
%% as the API will also accept <em>filename()</em> type.
-spec setup(#eco_config{} | filename()) -> ok | {error, Reason :: any()}.
setup(Eco = #eco_config{}) ->
    call({setup_config, Eco});
setup(Filename) when is_binary(Filename) ->
    setup(#eco_config{name = Filename});
setup(Filename) ->
    setup(to_binary(Filename)).

%% @doc Reload previously initialized configuration file.
%%
%% This function does all the necessary checks and cleanups.
%% Current working configuration will be dumped into a file in $CONF/dump/
%% directory. Dump filename will be extended with current timestamp.
-spec reload(filename() | #eco_config{}) -> ok | {error, Reason :: any()}.
reload(Filename) ->
    call({reload_config, Filename}).

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
    Reply = do_trans(Eco,
                    fun() ->
                            eco_ps:init_group(Eco#eco_config.name),
                            ok = setup_mirror(Eco)
                    end),
    {reply, Reply, State};
handle_call({reload_config, Filename}, _, State = #state{config_dir = Dir})
        when is_binary(Filename) ->
    Reply = do_trans(
                fun() ->
                    case mnesia:read(eco_config, Filename) of
                        [#eco_config{} = Eco] ->
                            ok = dump_current(Eco, Dir),
                            ok = clear_kvs(Eco),
                            ok = setup_mirror(Eco),
                            ok = eco_ps:publish(Filename,
                                                {eco_reload, Filename});
                        [] ->
                            mnesia:abort({not_initialized, Filename})
                    end
                end
                ),
    {reply, Reply, State};
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

call(Msg) ->
    gen_server:call(?MODULE, Msg).

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

-spec dump_current(#eco_config{}, binary()) ->
    ok | {error, enoent | enotdir | enospc | eacces | eisdir}.
dump_current(Eco = #eco_config{}, ConfigDir) ->
    {ok, #eco_snapshot{name = Name, raw = Raw}} = find_snapshot(Eco),
    {ok, DumpDir} = ensure_dump_dir(ConfigDir),
    {ok, DumpFname} = make_dump_name(Name),
    Fname = filename:join(DumpDir, DumpFname),
    file:write_file(Fname, Raw).

-spec find_snapshot(#eco_config{}) -> {ok, #eco_snapshot{}} | undefined.
find_snapshot(#eco_config{ name = Name }) ->
    case mnesia:transaction(
            fun() ->
                    mnesia:read(eco_snapshot, Name)
            end) of
        {atomic, [Snapshot]} ->
            {ok, Snapshot};
        {atomic, []} ->
            error_logger:error_msg("No previous configuration snapshot found.~n"),
            undefined
    end.

-spec do_trans(#eco_config{}, function()) -> ok | {error, any()}.
do_trans(Eco = #eco_config{}, Transaction) when is_function(Transaction) ->
    case mnesia:transaction(Transaction) of
        {aborted, Reason} ->
            eco_fallback:handle(Eco, Reason);
        {atomic, ok} ->
            ok
    end.
-spec do_trans(function()) -> ok | {error, {aborted, any()}}.
do_trans(Transaction) when is_function(Transaction) ->
    case mnesia:transaction(Transaction) of
        {aborted, Reason} ->
            {error, {aborted, Reason}};
        {atomic, ok} ->
             ok
     end.

-spec make_path({binary() | string(),binary() | [byte()]}) -> binary() | string().
make_path({Dir, Name}) ->
    filename:join([Dir, Name]).

-spec setup_mirror(#eco_config{}) -> ok | {error, any()}.
setup_mirror(#eco_config{name = Filename, config_path = CP,
                         adapter = A, force_kv = FKV} = Eco) ->
    {ok, _} = ensure_file_ok(CP),
    {ok, {Raw, Terms}} = load_config(A, CP),
    mnesia:write(#eco_snapshot{
            name        = Filename,
            timestamp   = erlang:localtime(),
            raw         = Raw,
            terms       = kv_check(Terms, FKV)
            }),
    _ = [ mnesia:write(#eco_kv{
                key = K,
                value = V
                }) || {K, V} <- kv_unfold(Filename, Terms) ],
    mnesia:write(Eco),
    ok.

-spec clear_kvs(#eco_config{}) -> ok | {error, any()}.
clear_kvs(#eco_config{name = Filename}) ->
    {atomic, KVs} = mnesia:match_object({eco_kv, {Filename, '_'}, '_'}),
    _ = [ mnesia:delete_object(Obj) || Obj <- KVs ],
    ok.

-spec kv_unfold(filename(), list()) -> list().
kv_unfold(Filename, List) ->
    lists:map(fun({K,V}) -> {{Filename, K}, V};
            (Else) -> {{Filename, Else}, true}
        end, List).

-spec kv_check(term(), boolean()) -> term().
kv_check(Terms, false) -> Terms;
kv_check(Terms, true) ->
    lists:map(fun({K,V}) -> {K,V};
            (T) -> erlang:error({kv_expected, T})
        end, Terms).

-spec ensure_file_ok(config_path()) ->
    {ok, config_path()} | {error, {not_regular_file, config_path()}}.
ensure_file_ok(CP) ->
    case filelib:is_regular(CP) of
        true -> {ok, regular};
        false ->
            {error, {not_regular_file, CP}}
    end.

-spec load_config(adapter(), config_path()) -> {ok, {binary(), term()}}.
load_config(Adapter, CP) ->
    {ok, Raw} = file:read_file(CP),
    {ok, Terms} = load_config2(Adapter, CP),
    {ok, {Raw, Terms}}.

-spec load_config2(adapter(), config_path()) -> {ok, term()}.
load_config2(native, File) ->
    u_consult(File);
load_config2(Custom, File) when is_function(Custom) ->
    Custom(File);
load_config2({Mod, Fun}, File) ->
    Mod:Fun(File);
load_config2(Custom, File) ->
    Custom:process_config(File).

%% Same as file:consult/1 except reads unicode
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

%% This comes from lib/kernel-2.15.1/src/file.erl
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

