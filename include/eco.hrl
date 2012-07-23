-type config_path()  :: binary().
-type filename()     :: binary() | string().
-type config_dir()   :: binary() | string().
-type adapter()      :: module() | function() | {module(), atom()}.
-type user_opt()     :: {sub, msg} | {sub, function()} | nosub.

-record(eco_config, {
        name                    :: filename(),
        config_path             :: config_path(),
        adapter  = native       :: adapter(),
        force_kv = false        :: boolean()
        }).

-record(eco_snapshot, {
        name         :: filename(),
        timestamp    :: calendar:datetime(),
        raw          :: binary(),
        terms        :: any()
        }).

-record(eco_kv, {
        key    :: {filename(), term()},
        value  :: term()
        }).
