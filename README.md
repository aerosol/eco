# eco

> **NOTE!** This README may be incomplete or not entirely true as the project is
still under development.

Eco is hopefully your number one choice when it comes to building configurable
Erlang applications.

Eco provides tools to create accessible configuration
management, even for people that are not (or not willing) familiar with
Erlang itself (think sysadmins, maintenance deparments etc.). It is backed
up by an Mnesia database which offers some unique features in the spirit of Erlang.

Eco has evolved from a pervious project named "confetti"; each having similar goals.

## Main features

* Easy, intuitive programming interface
* Multiple file formats supported (Erlang terms natively, however
  JSON/YAML/XML/INI or your own custom adapter are options)
* SSH Management console allows the programmer to expose API functions
  to end-users via a simple command-line interface, as well as perform some
  basic operations like configuration reloading.
* Fault Tolerance is provided through a multi-master mirroring of Mnesia database tables.
  Eco will not crash due to incorrect configuration files so long as they pass
  customizable validation and are syntactically correct.
* Eco will always try to fallback safely to the last known working configuration.

### Feature matrix

*"There are many like it but this one is mine..."*

There are few similar projects out there at github. Here is the quick comparsion:

<table>
<tr><th></th><th>eco</th><th>confetti</th><th>econf</th><th>nakaz</th></tr>
<tr><td>Auto-reload</td><td>-</td><td>-</td><td>YES (1)</td><td>-</td></tr>
<tr><td>Runtime-reload</td><td>YES</td><td>YES</td><td>YES</td><td>(?)</td></tr>
<tr><td>Reload notifications</td><td>YES</td><td>YES</td><td>-</td><td>YES</td></tr>
<tr><td>Supported formats</td><td>* (0)</td><td>Erlang</td><td>INI</td><td>YAML</td></tr>
<tr><td>Management interface</td><td>SSH (1)</td><td>telnet/nc (1)</td><td>-</td><td>-</td></tr>
<tr><td>Management ACL</td><td>YES (5)</td><td>-</td><td>-</td><td>-</td></tr>
<tr><td>Custom management commands</td><td>YES</td><td>YES</td><td>-</td><td>-</td></tr>
<tr><td>Custom validation</td><td>YES</td><td>YES</td><td>-</td><td>YES</td></tr>
<tr><td>Automatic backup on reload</td><td>YES</td><td>YES</td><td>-</td><td>-</td></tr>
<tr><td>Multiple config files</td><td>YES</td><td>YES</td><td>YES</td><td>-</td></tr>
<tr><td>Caching</td><td>Mnesia</td><td>-</td><td>ETS</td><td>?</td></tr>
<tr><td>Dependencies</td><td>- (2)</td><td>-</td><td>YES (3)</td><td>YES (4)</td></tr>
<tr><td>Key-Value support enforcement</td><td>YES (1)</td><td>-</td><td>(?)</td><td>(?)</td></tr>
<tr><td>UTF-8 support</td><td>YES</td><td>YES</td><td>(?)</td><td>(?)</td></tr>
</table>

* (0) - Format agnostic - custom format adapters
* (1) - optional/can be disabled
* (2) - optional dependencies for specific configuration adapters
* (3) - gproc, edown
* (4) - lager, yamler, z_validate, parse_trans
* (5) - SSH authentication (keys or passwords)
* (?) - To be verified


## Getting started

> **NOTE!** eco source code is well documented. If in doubt, please refer
directly to the source files or generate additional API documentation
with `make doc` target.

### Quick start

1. Requirements:

    * Erlang/OTP (tested with R15B01)
    * Rebar

2. Build the source code:

        $ git clone git://github.com/aerosol/vim-conf.git

3. Run the example client:

        $ make example

This make target will compile eco and execute the following:

    erl -pa ebin -boot start_sasl -sname example -s eco_example -eco_plugins shell -eco_auto_init true

You might be wondering what are the extra options and how to use them, but let's
skip that part for now. What you need to know is the `eco_example.erl` gen_server
will be started reading configuration values from `conf/example.conf`.

### Start for real

Most likely you would like to add eco to your rebar dependencies. You can do that
by putting the following directive into your `rebar.conf`:

    {deps, [
     {'eco', ".*", {git, "git://github.com/aerosol/eco.git"}}
    ]}.

Next, pull and compile eco with `rebar get-deps compile`. If everything went OK,
you should be able to see compiled Erlang BEAMS inside of `deps/eco/ebin`.
You are now ready to plug eco into your existing OTP application.

#### Starting the eco application

Because eco relies on mnesia, first time you run it, a schema must be initialized.
Eco will do it automatically if you provide the `eco_auto_init` argument:

```
$ erl -pa ebin -pa deps/*/ebin -s eco -eco_auto_init true
```

Otherwise you should initialize mnesia schema by hand. To do this start plain
erlang shell (with appropriate code paths added) and execute `eco:initialize()`
as follows:

    $ erl -pa ebin -pa deps/*/ebin -s eco
    Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.9.1  (abort with ^G)
    1> eco:initialize().

    =INFO REPORT==== 22-Aug-2012::11:31:53 ===
    Trying to initialize mnesia schema...
    ok

    =INFO REPORT==== 22-Aug-2012::11:31:54 ===
        application: mnesia
        exited: stopped
        type: temporary
    2>

If the schema has been initialized you should be able to verify it by confirming
the existence of `Mnesia.nonode@nohost` directory (the direcory name will change
if you provide the node/host names, you should initialize eco again then).

Starting eco from within your application is an fairly easy task:

```erlang
eco:start().
```

Note, that it will also start internal OTP dependencies such as `pg2` and `mnesia`.
Don't worry if you have already started these on your own.

#### Setting up the configuration files

Eco can handle any configuration file format (in worst case scenario, with a
little bit of additional programming). For now, let's just stick to native
format which is simply an Erlang term, that can be read using `file:consult/1`.

Configuration files are stored in `conf/` directory by default. This directory
is relative to your application directory. If you are not happy with the default
path, you can change it by providing `config_dir` setting within the `StartArgs`.
We will cover that later.

Every configuration file you will make use of needs to be set up. This is done
by calling the `eco:setup/1` or `eco:setup/2` function.
Let's assume you have already created a configuration file for your application:

    ```erlang
    %% conf/foo.conf

    {username, "root"}.
    {password, "123456"}. %% it is your root password, isn't it?
    ```

Most likely you have an OTP process that will make use of that data, therefore
you should call `eco:setup`:

    ```erlang
        {ok, F} = eco:setup(<<"foo.conf">>),
        ...
    ```

The `setup` call might be done in your process' supervisor or the `init/1`
function itself -- that is entirely up to you, but please note that there is no
reason for you to call `setup` more than once. And you may definitely expect some
strange behaviour when calling `setup` in more than one place with different
arguments in each.

The above call assumes that:

* You are using native Erlang terms format to store your configuration
* You are not using any validators except for the basic syntax coherence.
* The configuration read is a list of key-value terms.

If any of the above is false, eco will crash with an error message that should
be self-explainatory. If you want to modify those assumptions, you might be
interested in calling `setup/2` which accepts a list of additional setup options.

`setup` returns the `Filename` argument, which is done just for convinience.
The configuration is loaded now and are ready to fetch its values.


    ```erlang
        ...
        User = eco:term(username, F),
        Pass = eco:term(password, F),
        ...
    ```

`eco:term` interface is similar to `proplists:get_value`'s, so you can provide
the defaults as well:

    ```erlang
        ...
        User = eco:term(username, F, <<"Anonymous">>),
        Pass = eco:term(password, F),
        ...
    ```

If a key cannot be found and there is no default value provided, `eco:term`
will return `undefined`. Don't expect it to crash!

> **NOTE!** Refer to the source files/API documentation for more details.

#### Reloading the conifguration

There are two ways to reload a configuration file. One is by calling the API
function `reload/1` from within you code or the Erlang shell.

Each time you will successfully reload a configuration file, a backup of the
previous one will be saved to `conf/dump/` directory. Every "dump" file will be
tagged with the current timestamp for easier maintenance.

##### Reloading using the Erlang shell

Let's change some configuration values:


    ```erlang
    %% conf/foo.conf

    {username, "joe"}.
    {password, "moar entrOpy for greater good!"}.
    ```

And reload them using the Erlang shell:

    1> eco:reload(<<"foo.conf">>).
    {ok, <<"foo.conf">>}.

Looks like everything went fine and the new credentials are now accessible for
your code. But that will be noticed by your process the next time it fetches
the values. Let's make it more robust by adding a configuration reload subscription.

    ```erlang
        ...
        ok = eco:sub(<<"foo.conf">>),
        ...
    ```

The `sub` call basically means that the calling process will receieve a message
in the form of `{eco_reload, <<"foo.conf">>}` everytime the configuration has
been successfully reloaded. So what you need now is to handle that message.
Assuming your process is a gen_server, you will most likely do:

    ```erlang
        ...
        handle_info({eco_reload, <<"foo.conf">>}, State) ->
            %% Let the supervisor take care of this
            {stop, normal, State};
        ...
    ```

This way, your process will be stopped and hopefully restarted by a supervisor
fetching the new values. Alternatively you can just return `{noreply, State}`
with an updated `State` or perform any action that will assure the new configuration
is now known to the client.

##### Reloading using the eco shell

Eco comes with an SSH server for basic configuration management commands support.
This is probably the tool you will give to your customers.

###### Setting up the SSH server

1. Key-based authentication
2. Password-based authentication

    **TODO / under construction**


Now you can access your management console by simply doing:

    $ ssh admin@localhost -p50000
    eco $

The eco shell supports command autocompletion, so whenever in doubt you can hit
TAB to see the available commands or command parameters. The command we are
looking for is surprisingly named `reload`.

    eco $ reload foo.conf
    OK: eco_shell.conf
    eco $

#### Extending the eco shell

    **TODO / under construction**

#### Adding custom validation

    **TODO / under construction**

#### Configuration adapters - how to use different file formats?

    **TODO / under construction**

