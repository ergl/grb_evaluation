#!/usr/bin/env escript

-mode(compile).

-export([main/1]).

-define(APP_NAME, grb).
-define(DEFAULT_BRANCH, "master").
-define(DEFAULT_PROFILE, "default").
-define(DEFAULT_TCP_ID_LEN, 16).
-define(DEFAULT_INTER_DC_POOL, 16).
-define(DEFAULT_OP_LOG_READERS, 20).
-define(DEFAULT_LOG_SIZE, 25).
-define(DEFAULT_RING_SIZE, 32).
-define(DEFAULT_HB_INTERVAL, 5).
-define(DEFAULT_PARTITION_WAIT_MS, 5).
-define(DEFAULT_REPL_INTERVAL, 5).
-define(DEFAULT_UNI_REPL_INTERVAL, 5000).
-define(DEFAULT_FAULT_TOLERANCE_FACTOR, 1).
-define(DEFAULT_BCAST_INTERVAL, 5).
-define(DEFAULT_PRUNE_INTERVAL, 50).
-define(DEFAULT_CLOCK_INTERVAL, 10000).
-define(DEFAULT_RED_INTERVAL, 5).
-define(DEFAULT_RED_FIXED_HB, 250).
-define(DEFAULT_RED_DELIVERY, 10).
-define(DEFAULT_RED_PRUNE, 20).
-define(DEFAULT_RED_ABORT_DELAY_MS, 100).
-define(DEFAULT_RED_COORD_SIZE, 50).
-define(DEFAULT_INTER_DC_PORT, 8989).
-define(DEFAULT_VISIBILITY_RATE, 1000).
-define(DEFAULT_BLUE_STALL_MS, 0).
-define(DEFAULT_REDBLUE_SEQUENCER_POOL_SIZE, 8).
-define(REPO_URL, "https://github.com/ergl/grb.git").
-define(COMMANDS, [
    {download, false},
    {compile, false},
    {start, false},
    {stop, false},
    {recompile, false},
    {restart, false},
    {rebuild, false},
    {join, true},
    {connect_dcs, false},
    {measurements, true},
    {visibility, true}
]).

-define(IP_CONF, ip_configuration_table).

usage() ->
    Name = filename:basename(escript:script_name()),
    Commands = lists:foldl(
        fun({Command, NeedsArg}, Acc) ->
            CommandStr =
                case NeedsArg of
                    true -> io_lib:format("~s=arg", [Command]);
                    false -> io_lib:format("~s", [Command])
                end,
            case Acc of
                "" -> io_lib:format("< ~s", [CommandStr]);
                _ -> io_lib:format("~s | ~s", [Acc, CommandStr])
            end
        end,
        "",
        ?COMMANDS
    ),
    ok = io:fwrite(
        standard_error,
        "Usage: [-dv] ~s -r <region-name> -f <config-file> -p <ip-config-file> -c ~s~n",
        [Name, Commands ++ " >"]
    ).

main(Args) ->
    case parse_args(Args) of
        error ->
            usage(),
            halt(1);
        {ok, Parsed} ->
            #{
                config := ConfigFile,
                ip_config := IpConfigFile,
                region := Region,
                command := Command
            } = Parsed,

            erlang:put(dry_run, maps:get(dry_run, Parsed, false)),
            erlang:put(verbose, maps:get(verbose, Parsed, false)),
            erlang:put(current_region, Region),

            {ok, Config} = file:consult(ConfigFile),
            {ok, IpConfig} = file:consult(IpConfigFile),

            _ = ets:new(?IP_CONF, [ordered_set, named_table]),
            true = ets:insert(?IP_CONF, IpConfig),

            case maps:get(command_arg, Parsed, undefined) of
                undefined -> execute_command(Command, Config);
                Arg -> execute_command({Command, Arg}, Config)
            end,
            true = ets:delete(?IP_CONF),
            ok
    end.

execute_command(download, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Folder = io_lib:format("sources/~s", [Branch]),
    Cmd = io_lib:format("git clone ~s --single-branch --branch ~s ~s", [?REPO_URL, Branch, Folder]),
    os_cmd(Cmd),
    ok;
execute_command(compile, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s compile", [Branch, Profile])),
    os_cmd(
        io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])
    ),
    ok;
execute_command(start, Config) ->
    ok = start_grb(Config);
execute_command(stop, Config) ->
    ok = stop_grb(Config);
execute_command(recompile, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    os_cmd(io_lib:format("rm -rf sources/~s/_build/~s/rel", [Branch, Profile])),
    os_cmd(
        io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])
    ),
    ok;
execute_command(restart, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    ok = stop_grb(Config),
    os_cmd(io_lib:format("rm -rf sources/~s/_build/~s/rel", [Branch, Profile])),
    os_cmd(
        io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])
    ),
    ok = start_grb(Config),
    ok;
execute_command(rebuild, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    os_cmd(io_lib:format("rm -rf sources/~s/_build", [Branch])),
    os_cmd(io_lib:format("cd sources/~s && git fetch origin", [Branch])),
    os_cmd(io_lib:format("cd sources/~s && git reset --hard origin/~s", [Branch, Branch])),
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s compile", [Branch, Profile])),
    os_cmd(
        io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])
    ),
    ok;
execute_command({join, Region}, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    NodeArgs = get_region_grb_nodes_str(Region),
    Cmd = io_lib:format(
        "./sources/~s/bin/join_cluster_script.erl -c ~s ~s",
        [Branch, Region, NodeArgs]
    ),
    os_cmd(Cmd),
    ok;
execute_command(connect_dcs, Config) ->
    {red_leader_cluster, LeaderCluster} = lists:keyfind(red_leader_cluster, 1, Config),
    LeaderIP = get_public_leader_grb_ip(atom_to_list(LeaderCluster)),
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    InterDCPort = get_config_key(inter_dc_port, Config, ?DEFAULT_INTER_DC_PORT),
    NodeArgs = get_public_leader_grb_ips_str(),
    Cmd = io_lib:format(
        "./sources/~s/bin/connect_dcs.erl -i -p ~b -l ~s ~s",
        [Branch, InterDCPort, LeaderIP, NodeArgs]
    ),
    os_cmd(Cmd),
    ok;
execute_command({measurements, Region}, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    NodeArgs = get_region_grb_nodes_str(Region),
    Cmd = io_lib:format(
        "./sources/~s/bin/get_measurements.escript -o /home/ubuntu/measurements.bin ~s",
        [Branch, NodeArgs]
    ),
    os_cmd(Cmd),
    ok;
execute_command({visibility, Region}, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    NodeArgs = get_region_grb_nodes_str(Region),
    Cmd = io_lib:format(
        "./sources/~s/bin/visibility.escript -o /home/ubuntu/visibility.bin ~s",
        [Branch, NodeArgs]
    ),
    os_cmd(Cmd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_grb(Config) ->
    _ = os_cmd("sudo sysctl net.ipv4.ip_local_port_range=\"15000 61000\""),
    _ = os_cmd(
        "grep -qxF '* soft nofile 1048576' /etc/security/limits.conf"
        ++ "|| echo \"* soft nofile 1048576\" | sudo tee -a /etc/security/limits.conf"
    ),
    _ = os_cmd(
        "grep -qxF '* hard nofile 1048576' /etc/security/limits.conf"
        ++ "|| echo \"* hard nofile 1048576\" | sudo tee -a /etc/security/limits.conf"
    ),
    IP = get_current_ip_addres(),
    INTER_DC_IP = get_public_ip_address(IP),
    REDBLUE_SEQUENCER_IP = get_sequencer_private_ip(),

    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),

    TCP_ID_LEN = get_config_key(tcp_id_len_bits, Config, ?DEFAULT_TCP_ID_LEN),
    OP_LOG_READERS = get_config_key(oplog_readers, Config, ?DEFAULT_OP_LOG_READERS),
    VSN_LOG_SIZE = get_config_key(version_log_size, Config, ?DEFAULT_LOG_SIZE),
    RIAK_RING_SIZE = get_config_key(ring_creation_size, Config, ?DEFAULT_RING_SIZE),
    INTER_DC_SENDER_POOL_SIZE = get_config_key(
        inter_dc_pool_size,
        Config,
        ?DEFAULT_INTER_DC_POOL
    ),
    SELF_HB_INTERVAL_MS = get_config_key(
        self_blue_heartbeat_interval,
        Config,
        ?DEFAULT_HB_INTERVAL
    ),
    PARTITION_RETRY_MS = get_config_key(
        partition_ready_wait_ms,
        Config,
        ?DEFAULT_PARTITION_WAIT_MS
    ),
    REPLICATION_INTERVAL_MS = get_config_key(
        basic_replication_interval,
        Config,
        ?DEFAULT_REPL_INTERVAL
    ),
    PREPARED_BLUE_STALE_MS = get_config_key(
        prepared_blue_stale_check_ms,
        Config,
        ?DEFAULT_BLUE_STALL_MS
    ),
    UNIFORM_REPLICATION_INTERVAL_MS = get_config_key(
        uniform_replication_interval,
        Config,
        ?DEFAULT_UNI_REPL_INTERVAL
    ),
    FAULT_TOLERANCE_FACTOR = get_config_key(
        fault_tolerance_factor,
        Config,
        ?DEFAULT_FAULT_TOLERANCE_FACTOR
    ),
    BCAST_KNOWN_VC_INTERVAL_MS = get_config_key(
        local_broadcast_interval,
        Config,
        ?DEFAULT_BCAST_INTERVAL
    ),
    COMMITTED_BLUE_PRUNE_INTERVAL_MS = get_config_key(
        prune_committed_blue_interval,
        Config,
        ?DEFAULT_PRUNE_INTERVAL
    ),
    UNIFORM_CLOCK_INTERVAL_MS = get_config_key(
        remote_clock_broadcast_interval,
        Config,
        ?DEFAULT_CLOCK_INTERVAL
    ),
    RED_HB_SCHEDULE_MS = get_config_key(
        red_heartbeat_schedule_ms,
        Config,
        ?DEFAULT_RED_INTERVAL
    ),
    RED_HB_FIXED_SCHEDULE_MS = get_config_key(
        red_heartbeat_fixed_schedule_ms,
        Config,
        ?DEFAULT_RED_FIXED_HB
    ),
    RED_DELIVER_INTERVAL_MS = get_config_key(red_delivery_interval, Config, ?DEFAULT_RED_DELIVERY),
    RED_PRUNE_INTERVAL = get_config_key(red_prune_interval, Config, ?DEFAULT_RED_PRUNE),
    RED_ABORT_INTERVAL_MS = get_config_key(
        red_abort_interval_ms,
        Config,
        ?DEFAULT_RED_ABORT_DELAY_MS
    ),
    RED_COORD_SIZE = get_config_key(red_coord_pool_size, Config, ?DEFAULT_RED_COORD_SIZE),
    UNIFORM_VISIBILITY_RATE = get_config_key(
        visibility_sample_rate,
        Config,
        ?DEFAULT_VISIBILITY_RATE
    ),
    REDBLUE_SEQUENCER_POOL_SIZE = get_config_key(
        redblue_sequencer_connection_pool,
        Config,
        ?DEFAULT_REDBLUE_SEQUENCER_POOL_SIZE
    ),
    EnvVarString = io_lib:format(
        "TCP_ID_LEN=~b INTER_DC_SENDER_POOL_SIZE=~b OP_LOG_READERS=~b VSN_LOG_SIZE=~b RIAK_RING_SIZE=~b SELF_HB_INTERVAL_MS=~b PARTITION_RETRY_MS=~b REPLICATION_INTERVAL_MS=~b PREPARED_BLUE_STALE_MS=~b UNIFORM_REPLICATION_INTERVAL_MS=~b FAULT_TOLERANCE_FACTOR=~b BCAST_KNOWN_VC_INTERVAL_MS=~b COMMITTED_BLUE_PRUNE_INTERVAL_MS=~b UNIFORM_CLOCK_INTERVAL_MS=~b RED_HB_SCHEDULE_MS=~b RED_HB_FIXED_SCHEDULE_MS=~b RED_DELIVER_INTERVAL_MS=~b RED_PRUNE_INTERVAL=~b RED_ABORT_INTERVAL_MS=~b RED_COORD_POOL_SIZE=~b UNIFORM_VISIBILITY_RATE=~b REDBLUE_SEQUENCER_POOL_SIZE=~b REDBLUE_SEQUENCER_IP=~s IP=~s INTER_DC_IP=~s",
        [
            TCP_ID_LEN,
            INTER_DC_SENDER_POOL_SIZE,
            OP_LOG_READERS,
            VSN_LOG_SIZE,
            RIAK_RING_SIZE,
            SELF_HB_INTERVAL_MS,
            PARTITION_RETRY_MS,
            REPLICATION_INTERVAL_MS,
            PREPARED_BLUE_STALE_MS,
            UNIFORM_REPLICATION_INTERVAL_MS,
            FAULT_TOLERANCE_FACTOR,
            BCAST_KNOWN_VC_INTERVAL_MS,
            COMMITTED_BLUE_PRUNE_INTERVAL_MS,
            UNIFORM_CLOCK_INTERVAL_MS,
            RED_HB_SCHEDULE_MS,
            RED_HB_FIXED_SCHEDULE_MS,
            RED_DELIVER_INTERVAL_MS,
            RED_PRUNE_INTERVAL,
            RED_ABORT_INTERVAL_MS,
            RED_COORD_SIZE,
            UNIFORM_VISIBILITY_RATE,
            REDBLUE_SEQUENCER_POOL_SIZE,
            REDBLUE_SEQUENCER_IP,
            IP,
            INTER_DC_IP
        ]
    ),

    Cmd = io_lib:format(
        "~s ./sources/~s/_build/~s/rel/~s/bin/env daemon",
        [EnvVarString, Branch, Profile, ?APP_NAME]
    ),

    os_cmd(Cmd),

    case erlang:get(dry_run) of
        true -> ok;
        false -> timer:sleep(2000)
    end,

    PingCmd = io_lib:format(
        "IP=~s ./sources/~s/_build/~s/rel/~s/bin/env ping",
        [IP, Branch, Profile, ?APP_NAME]
    ),

    os_cmd(PingCmd),
    ok.

stop_grb(Config) ->
    IP = get_current_ip_addres(),
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    Cmd = io_lib:format(
        "IP=~s ./sources/~s/_build/~s/rel/~s/bin/env stop",
        [IP, Branch, Profile, ?APP_NAME]
    ),
    os_cmd(Cmd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec os_cmd(string()) -> ok.
os_cmd(Cmd) ->
    Verbose = erlang:get(verbose),
    DryRun = erlang:get(dry_run),
    case Verbose of
        true -> io:format("$ ~s~n", [Cmd]);
        false -> ok
    end,
    case DryRun of
        true ->
            ok;
        false ->
            Ret = os:cmd(Cmd),
            case Verbose of
                true -> io:format("~s~n", [Ret]);
                false -> ok
            end
    end.

-spec get_current_ip_addres() -> string().
get_current_ip_addres() ->
    {ok, Hostname} = inet:gethostname(),
    ip_for_node(Hostname).

ip_for_node([$i, $p, $- | Rest]) ->
    %% For aws, node names are ip-XXX-XXX-XXX-XXX
    %% We trim ip- prefix and change the dashes with points
    string:join(string:replace(Rest, "-", ".", all), "").

-spec get_public_ip_address(string()) -> string().
get_public_ip_address(PrivateIP) ->
    Region = erlang:get(current_region),
    ets:lookup_element(?IP_CONF, {servers, public_mapping, Region, PrivateIP}, 2).

-spec get_sequencer_private_ip() -> string().
get_sequencer_private_ip() ->
    Region = erlang:get(current_region),
    case
        ets:select(?IP_CONF, [{ {{sequencers, private, Region}, ['$1' | '_']}, [], ['$1'] }])
    of
        [IP] -> IP;
        [] -> ""
    end.

-spec get_region_grb_nodes(string()) -> [string()].
get_region_grb_nodes(Region) ->
    ets:lookup_element(?IP_CONF, {servers, private, Region}, 2).

-spec get_region_grb_nodes_str(string()) -> string().
get_region_grb_nodes_str(Region) ->
    lists:foldl(
        fun(NodeIP, Acc) ->
            unicode:characters_to_list(
                io_lib:format("~s '~s@~s'", [Acc, atom_to_list(?APP_NAME), NodeIP])
            )
        end,
        "",
        get_region_grb_nodes(Region)
    ).

-spec get_public_leader_grb_ips() -> [string()].
get_public_leader_grb_ips() ->
    ets:select(?IP_CONF, [{{{servers, public, '_'}, ['$1' | '_']}, [], ['$1']}]).

-spec get_public_leader_grb_ips_str() -> string().
get_public_leader_grb_ips_str() ->
    lists:foldl(
        fun(NodeIP, Acc) ->
            unicode:characters_to_list(
                io_lib:format("~s '~s'", [Acc, NodeIP])
            )
        end,
        "",
        get_public_leader_grb_ips()
    ).

-spec get_public_leader_grb_ip(string()) -> string().
get_public_leader_grb_ip(Region) ->
    [IP] = ets:select(?IP_CONF, [{{{servers, public, Region}, ['$1' | '_']}, [], ['$1']}]),
    IP.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_config_key(Key, Config, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false -> Default;
        {Key, Value} -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args([]) ->
    error;
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

parse_args([], Acc) ->
    {ok, Acc};
parse_args([[$- | Flag] | Args], Acc) ->
    case Flag of
        [$f] ->
            parse_flag(Args, fun(Arg) -> Acc#{config => Arg} end);
        [$p] ->
            parse_flag(Args, fun(Arg) -> Acc#{ip_config => Arg} end);
        [$r] ->
            parse_flag(Args, fun(Arg) -> Acc#{region => Arg} end);
        [$c] ->
            parse_flag(Args, fun(Arg) -> parse_command(Arg, Acc) end);
        [$v] ->
            parse_args(Args, Acc#{verbose => true});
        [$d] ->
            parse_args(Args, Acc#{dry_run => true});
        _ ->
            error
    end;
parse_args(_, _) ->
    error.

parse_flag(Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args(Rest, Fun(FlagArg));
        _ -> error
    end.

parse_command(Arg, Acc) ->
    case string:str(Arg, "=") of
        0 ->
            Acc#{command => list_to_atom(Arg)};
        _ ->
            % crash on malformed command for now
            [Command, CommandArg | _Ignore] = string:tokens(Arg, "="),
            Acc#{command_arg => CommandArg, command => list_to_atom(Command)}
    end.

required(Opts) ->
    Required = [config, ip_config, command, region],
    Valid = lists:all(fun(F) -> maps:is_key(F, Opts) end, Required),
    case Valid of
        false ->
            error;
        true ->
            case maps:is_key(command, Opts) of
                true -> check_command(Opts);
                false -> {ok, Opts}
            end
    end.

check_command(Opts = #{command := Command}) ->
    case lists:member({Command, maps:is_key(command_arg, Opts)}, ?COMMANDS) of
        true ->
            {ok, Opts};
        false ->
            error
    end.
