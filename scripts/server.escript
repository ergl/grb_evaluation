#!/usr/bin/env escript

-mode(compile).
-export([main/1]).

-define(APP_NAME, grb).
-define(DEFAULT_BRANCH, "master").
-define(DEFAULT_PROFILE, "default").
-define(DEFAULT_LOG_SIZE, 25).
-define(DEFAULT_RING_SIZE, 64).
-define(DEFAULT_HB_INTERVAL, 5).
-define(DEFAULT_REPL_INTERVAL, 5).
-define(DEFAULT_UNI_REPL_INTERVAL, 5000).
-define(DEFAULT_BCAST_INTERVAL, 5).
-define(DEFAULT_PRUNE_INTERVAL, 50).
-define(REPO_URL, "https://github.com/ergl/grb.git").
-define(COMMANDS, [ {download, false}
                  , {compile, false}
                  , {start, false}
                  , {stop, false}
                  , {restart, false}
                  , {rebuild, false}
                  , {tc, true}
                  , {tclean, false}
                  , {join, false}
                  , {connect_dcs, false} ]).

usage() ->
    Name = filename:basename(escript:script_name()),
    Commands = lists:foldl(fun({Command, NeedsArg}, Acc) ->
        CommandStr = case NeedsArg of
            true -> io_lib:format("~s=arg", [Command]);
            false -> io_lib:format("~s", [Command])
        end,
        case Acc of
            "" -> io_lib:format("< ~s", [CommandStr]);
            _ -> io_lib:format("~s | ~s", [Acc, CommandStr])
        end
    end, "", ?COMMANDS),
    ok = io:fwrite(
        standard_error,
        "Usage: [-dv] ~s -f <config-file> -c ~s~n",
        [Name, Commands ++ " >"]
    ).

main(Args) ->
    case parse_args(Args) of
        error ->
            usage(),
            halt(1);
        {ok, Parsed = #{config := ConfigFile, command := Command}} ->
            erlang:put(dry_run, maps:get(dry_run, Parsed, false)),
            erlang:put(verbose, maps:get(verbose, Parsed, false)),

            {ok, Config} = file:consult(ConfigFile),
            case maps:get(command_arg, Parsed, undefined) of
                undefined -> execute_command(Command, Config);
                Arg -> execute_command({Command, Arg}, Config)
            end
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
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])),
    ok;

execute_command(start, Config) ->
    ok = start_grb(Config);

execute_command(stop, Config) ->
    ok = stop_grb(Config);

execute_command(restart, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    ok = stop_grb(Config),
    os_cmd(io_lib:format("rm -rf sources/~s/_build/~s/rel", [Branch, Profile])),
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])),
    ok = start_grb(Config),
    ok;

execute_command(rebuild, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),
    os_cmd(io_lib:format("rm -rf sources/~s/_build", [Branch])),
    os_cmd(io_lib:format("cd sources/~s && git fetch origin", [Branch])),
    os_cmd(io_lib:format("cd sources/~s && git reset --hard origin/~s", [Branch, Branch])),
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s compile", [Branch, Profile])),
    os_cmd(io_lib:format("cd sources/~s && ./rebar3 as ~s release -n ~s", [Branch, Profile, ?APP_NAME])),
    ok;

execute_command({tc, ClusterName}, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Cmd = io_lib:format(
        "escript -c -n ./sources/~s/bin/build_tc_rules.escript -c ~s -f /home/borja.deregil/cluster.config",
        [Branch, ClusterName]
    ),
    os_cmd(Cmd),
    ok;

execute_command(tclean, _Config) ->
    Iface = case inet:gethostname() of
        {ok, "apollo-2-4"} -> "eth0";
        _ -> "enp1s0"
    end,
    os_cmd(io_lib:format("sudo tc qdisc del dev ~s root", [Iface])),
    ok;

execute_command(join, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Cmd = io_lib:format("./sources/~s/bin/join_cluster_script.erl -f /home/borja.deregil/cluster.config", [Branch]),
    os_cmd(Cmd),
    ok;

execute_command(connect_dcs, Config) ->
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Cmd = io_lib:format("./sources/~s/bin/connect_dcs.erl -f /home/borja.deregil/cluster.config", [Branch]),
    os_cmd(Cmd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_grb(Config) ->
    IP = get_current_ip_addres(),
    Branch = get_config_key(grb_branch, Config, ?DEFAULT_BRANCH),
    Profile = get_config_key(grb_rebar_profile, Config, ?DEFAULT_PROFILE),

    VSN_LOG_SIZE = get_config_key(version_log_size, Config, ?DEFAULT_LOG_SIZE),
    RIAK_RING_SIZE = get_config_key(ring_creation_size, Config, ?DEFAULT_RING_SIZE),
    SELF_HB_INTERVAL_MS = get_config_key(self_blue_heartbeat_interval, Config, ?DEFAULT_HB_INTERVAL),
    REPLICATION_INTERVAL_MS = get_config_key(basic_replication_interval, Config, ?DEFAULT_REPL_INTERVAL),
    UNIFORM_REPLICATION_INTERVAL_MS = get_config_key(uniform_replication_interval, Config, ?DEFAULT_UNI_REPL_INTERVAL),
    BCAST_KNOWN_VC_INTERVAL_MS = get_config_key(local_broadcast_interval, Config, ?DEFAULT_BCAST_INTERVAL),
    COMMITTED_BLUE_PRUNE_INTERVAL_MS = get_config_key(prune_committed_blue_interval, Config, ?DEFAULT_PRUNE_INTERVAL),

    Cmd = io_lib:format(
        "VSN_LOG_SIZE=~b SELF_HB_INTERVAL_MS=~b REPLICATION_INTERVAL_MS=~b UNIFORM_REPLICATION_INTERVAL_MS=~b BCAST_KNOWN_VC_INTERVAL_MS=~b COMMITTED_BLUE_PRUNE_INTERVAL_MS=~b RIAK_RING_SIZE=~b IP=~s ./sources/~s/_build/~s/rel/~s/bin/env start",
        [VSN_LOG_SIZE, SELF_HB_INTERVAL_MS,
        REPLICATION_INTERVAL_MS, UNIFORM_REPLICATION_INTERVAL_MS,
        BCAST_KNOWN_VC_INTERVAL_MS, COMMITTED_BLUE_PRUNE_INTERVAL_MS,
        RIAK_RING_SIZE, IP,
        Branch, Profile, ?APP_NAME]),

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
        true -> ok;
        false ->
            Ret = os:cmd(Cmd),
            case Verbose of
                true -> io:format("~s~n", [Ret]);
                false -> ok
            end
    end.

-spec get_current_ip_addres() -> string().
get_current_ip_addres() ->
    IFace = case inet:gethostname() of
        {ok, "apollo-2-4"} -> "eth0";
        {ok, "apollo-1-6"} -> "enp1s0.1004";
        _ -> "enp1s0"
    end,
    {ok, IFaceInfo} = inet:getifaddrs(),
    PossibleIPs = get_config_key_all(addr, get_config_key(IFace, IFaceInfo)),
    ParsedIPs = lists:filtermap(fun
        (IPv4={_,_,_,_}) -> {true, inet:ntoa(IPv4)};
        (_) -> false
    end, PossibleIPs),
    1 = length(ParsedIPs),
    hd(ParsedIPs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_config_key(Key, Config) ->
    {Key, Value} = lists:keyfind(Key, 1, Config),
    Value.

get_config_key(Key, Config, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false -> Default;
        {Key, Value} -> Value
    end.

get_config_key_all(Key, Config) ->
    get_config_key_all(Key, Config, []).

get_config_key_all(_Key, [], Acc) -> Acc;
get_config_key_all(Key, [{Key, Val} | Rest], Acc) -> get_config_key_all(Key, Rest, [Val | Acc]);
get_config_key_all(Key, [{_Other, _} | Rest], Acc) -> get_config_key_all(Key, Rest, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args([]) -> error;
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

parse_args([], Acc) -> {ok, Acc};
parse_args([ [$- | Flag] | Args], Acc) ->
    case Flag of
        [$f] ->
            parse_flag(Args, fun(Arg) -> Acc#{config => Arg} end);
        [$c] ->
            parse_flag(Args, fun(Arg) -> parse_command(Arg, Acc) end);
        [$v] ->
            parse_args(Args, Acc#{verbose => true});
        [$d] ->
            parse_args(Args, Acc#{dry_run => true});
        _ ->
            error
    end;
parse_args(_, _) -> error.

parse_flag(Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args(Rest, Fun(FlagArg));
        _ -> error
    end.

parse_command(Arg, Acc) ->
    case string:str(Arg, "=") of
        0 -> Acc#{command => list_to_atom(Arg)};
        _ ->
            % crash on malformed command for now
            [Command, CommandArg | _Ignore] = string:tokens(Arg, "="),
            Acc#{command_arg => CommandArg, command => list_to_atom(Command)}
    end.

required(Opts) ->
    Required = [config, command],
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

check_command(Opts=#{command := Command}) ->
    case lists:member({Command, maps:is_key(command_arg, Opts)}, ?COMMANDS) of
        true ->
            {ok, Opts};
        false ->
            error
    end.
