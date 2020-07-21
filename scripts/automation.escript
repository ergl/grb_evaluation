#!/usr/bin/env escript

-mode(compile).
-export([main/1]).

%% Example configuration file:
%% ```cluster.config:
%% {latencies, #{
%%     nancy => [{rennes, 10}, {tolouse, 20}],
%%     rennes => [{nancy, 10}, {tolouse, 15}],
%%     tolouse => [{rennes, 15}, {nancy, 20}]
%% }}.
%%
%% {clusters, #{
%%     nancy => #{servers => ['apollo-1-1.imdea', ...],
%%                clients => ['apollo-2-1.imdea', ...]},
%%     rennes => #{servers => ['apollo-1-4.imdea', ...],
%%                 clients => ['apollo-2-4.imdea', ...]},
%%     tolouse => #{servers => ['apollo-1-7.imdea', ...],
%%                  clients => ['apollo-2-7.imdea', ...]}
%% }}.
%% ```

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts").
-define(SSH_PRIV_KEY, "/Users/ryan/.ssh/imdea_id_ed25519").

-define(IN_NODES_PATH, unicode:characters_to_list(io_lib:format("~s/execute-in-nodes.sh", [?SELF_DIR]))).
-define(CONFIG_DIR, unicode:characters_to_list(io_lib:format("~s/configuration", [?SELF_DIR]))).

-define(GRB_BRANCH, "master").
-define(LASP_BENCH_BRANCH, "bench_grb").

-define(JOIN_TIMEOUT, timer:minutes(5)).

-define(CONF, configuration).

-define(COMMANDS, [ {check, false}
                  , {sync, false}
                  , {server, false}
                  , {clients, false}
                  , {prologue, false}

                  , {start, false}
                  , {stop, false}
                  , {join, false}
                  , {connect_dcs, false}
                  , {latencies, false}
                  , {prepare, false}

                  , {load, false}
                  , {bench, false}

                  , {restart, false}
                  , {rebuild, false}
                  , {cleanup, false}]).

usage() ->
    Name = filename:basename(escript:script_name()),
    Commands = lists:foldl(fun(El, Acc) -> io_lib:format("~s | ~p", [Acc, El]) end, "", [C || {C, _} <- ?COMMANDS]),
    ok = io:fwrite(
        standard_error,
        "Usage: [-ds] ~s -f <config-file> -c <command=arg>~nCommands: ~s~n",
        [Name, Commands]
    ).

main(Args) ->
    case parse_args(Args) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~s~n", [Reason]),
            usage(),
            halt(1);

        {ok, Opts = #{config := ConfigFile}} ->
            _ = ets:new(?CONF, [set, named_table]),

            {ok, ConfigTerms} = file:consult(ConfigFile),
            {clusters, ClusterMap} = lists:keyfind(clusters, 1, ConfigTerms),

            RingTuple = lists:keyfind(ring_size, 1, ConfigTerms),
            PruneTuple = lists:keyfind(prune_interval_ms, 1, ConfigTerms),
            ReplicationTuple = lists:keyfind(replication_interval_ms, 1, ConfigTerms),
            BroadcastTuple = lists:keyfind(local_broadcast_interval_ms, 1, ConfigTerms),

            true = ets:insert(?CONF, {dry_run, maps:get(dry_run, Opts, false)}),
            true = ets:insert(?CONF, {silent, maps:get(verbose, Opts, false)}),
            true = ets:insert(?CONF, [RingTuple, PruneTuple, ReplicationTuple, BroadcastTuple]),

            Command = maps:get(command, Opts),
            CommandArg = maps:get(command_arg, Opts, false),

            io:format("Running command: ~p (arg ~p)~n", [Command, CommandArg]),

            ok = do_command(Command, CommandArg, ClusterMap),
            true = ets:delete(?CONF),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Commands

do_command(check, _, ClusterMap) -> ok = check_nodes(ClusterMap);
do_command(sync, _, ClusterMap) -> ok = sync_nodes(ClusterMap);
do_command(server, _, ClusterMap) -> ok = prepare_server(ClusterMap);
do_command(clients, _, ClusterMap) -> ok = prepare_lasp_bench(ClusterMap);
do_command(prologue, _, ClusterMap) ->
    ok = check_nodes(ClusterMap),
    ok = sync_nodes(ClusterMap),
    ok = prepare_server(ClusterMap),
    ok = prepare_lasp_bench(ClusterMap),
    alert("Prologue finished!"),
    ok;

do_command(start, _, ClusterMap) ->
    do_in_nodes_par(server_command("start"), server_nodes(ClusterMap)),
    ok;

do_command(stop, _, ClusterMap) ->
    do_in_nodes_par(server_command("stop"), server_nodes(ClusterMap)),
    ok;

do_command(prepare, Arg, ClusterMap) ->
    ok = do_command(join, Arg, ClusterMap),
    ok = do_command(connect_dcs, Arg, ClusterMap),
    ok = do_command(latencies, Arg, ClusterMap),
    % ok = do_command(load, Arg, ClusterMap),
    alert("Prepare finished!"),
    ok;

do_command(join, _, ClusterMap) ->
    NodeNames = server_nodes(ClusterMap),
    Parent = self(),
    Reference = erlang:make_ref(),
    ChildFun = fun() ->
        Reply = do_in_nodes_seq(server_command("join", "/home/borja.deregil/cluster.config"), [hd(NodeNames)]),
        Parent ! {Reference, Reply}
    end,
    Start = erlang:timestamp(),
    ChildPid = erlang:spawn(ChildFun),
    receive
        {Reference, Reply} ->
            End = erlang:timestamp(),
            io:format("~p~n", [Reply]),
            io:format("Ring done after ~p~n", [timer:now_diff(End, Start)]),
            ok
    after ?JOIN_TIMEOUT ->
        io:fwrite(standard_error, "Ring timed out after ~b milis~n", [?JOIN_TIMEOUT]),
        erlang:exit(ChildPid, kill),
        error
    end;

do_command(connect_dcs, _, ClusterMap) ->
    [MainNode | _] = server_nodes(ClusterMap),
    Rep = do_in_nodes_seq(
        server_command("connect_dcs", "/home/borja.deregil/cluster.config"),
        [MainNode]
    ),
    io:format("~p~n", [Rep]),
    ok;

do_command(load, _, ClusterMap) ->
    NodeNames = client_nodes(ClusterMap),
    TargetNode = hd(server_nodes(ClusterMap)),
    io:format("~p~n", [do_in_nodes_seq(client_command("-y load", atom_to_list(TargetNode)), [hd(NodeNames)])]),
    ok;

do_command(latencies, _, ClusterMap) ->
    ok = maps:fold(fun(ClusterName, #{servers := ClusterServers}, _Acc) ->
        io:format(
            "~p~n",
            [do_in_nodes_par(
                server_command("tc", atom_to_list(ClusterName), "/home/borja.deregil/cluster.config"),
                ClusterServers)
            ]
        ),
        ok
    end, ok, ClusterMap),
    ok;

do_command(bench, _, ClusterMap) ->
    NodeNames = client_nodes(ClusterMap),
    BootstrapInfo = build_bootstrap_info(ClusterMap),
    BootstrapPort = 7878,
    pmap(fun(Node) -> transfer_config(Node, "run.config") end, NodeNames),

    pmap(fun(Node) ->
        Command = client_command(
            "run",
            "/home/borja.deregil/run.config",
            atom_to_list(maps:get(Node, BootstrapInfo)),
            integer_to_list(BootstrapPort)
        ),
        Cmd = io_lib:format("~s \"~s\" ~s", [?IN_NODES_PATH, Command, atom_to_list(Node)]),
        safe_cmd(Cmd)
    end, NodeNames),
    alert("Benchmark finished!"),
    ok;

do_command(restart, _, ClusterMap) ->
    io:format("~p~n", [do_in_nodes_par(server_command("restart"), server_nodes(ClusterMap))]),
    ok;

do_command(rebuild, _, ClusterMap) ->
    DBNodes = server_nodes(ClusterMap),
    ClientNodes = client_nodes(ClusterMap),

    do_in_nodes_par(server_command("rebuild"), DBNodes),
    do_in_nodes_par(client_command("rebuild"), ClientNodes),
    ok;

do_command(cleanup, _, ClusterMap) ->
    AllNodes = all_nodes(ClusterMap),
    ServerNodes = server_nodes(ClusterMap),

    io:format("~p~n", [do_in_nodes_par(server_command("tclean"), ServerNodes)]),
    io:format("~p~n", [do_in_nodes_par("rm -rf sources; mkdir -p sources", AllNodes)]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Command Impl

check_nodes(ClusterMap) ->
    io:format("Checking that all nodes are up and on the correct governor mode~n"),

    AllNodes = all_nodes(ClusterMap),

    UptimeRes = do_in_nodes_par("uptime", AllNodes),
    false = lists:any(fun(Res) -> string:str(Res, "timed out") =/= 0 end, UptimeRes),

    % Set all nodes to performance governor status, then verify
    _ = do_in_nodes_par("echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor", AllNodes),
    GovernorStatus = do_in_nodes_par("cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor", AllNodes),
    false = lists:any(fun(Res) -> string:str(Res, "powersave") =/= 0 end, GovernorStatus),

    % Transfer server, bench and cluster config
    io:format("Transfering benchmark config files (server, bench, cluster)...~n"),
    pmap(fun(Node) ->
        transfer_script(Node, "server.sh"),
        transfer_script(Node, "bench.sh"),
        transfer_config(Node, "cluster.config")
    end, AllNodes),
    ok.

sync_nodes(ClusterMap) ->
    io:format("Resyncing NTP on all nodes~n"),
    AllNodes = all_nodes(ClusterMap),
    _ = do_in_nodes_par("sudo service ntp stop", AllNodes),
    _ = do_in_nodes_par("sudo ntpd -gq system.imdea", AllNodes),
    _ = do_in_nodes_par("sudo service ntp start", AllNodes),
    ok.

prepare_server(ClusterMap) ->
    NodeNames = server_nodes(ClusterMap),
    io:format("~p~n", [do_in_nodes_par(server_command("dl"), NodeNames)]),
    _ = do_in_nodes_par(server_command("compile"), NodeNames),
    io:format("~p~n", [do_in_nodes_par(server_command("start"), NodeNames)]),
    ok.

prepare_lasp_bench(ClusterMap) ->
    NodeNames = client_nodes(ClusterMap),
    io:format("~p~n", [do_in_nodes_par(client_command("dl"), NodeNames)]),
    _ = do_in_nodes_par(client_command("compile"), NodeNames),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Util

server_command(Command) ->
    io_lib:format("~s ~s", [server_template(), Command]).

server_command(Command, Arg) ->
    io_lib:format("~s ~s ~s", [server_template(), Command, Arg]).

server_command(Command, Arg1, Arg2) ->
    io_lib:format("~s ~s ~s ~s", [server_template(), Command, Arg1, Arg2]).

server_template() ->
    Ring = get_conf(ring_size),
    Prune = get_conf(prune_interval_ms),
    Repl = get_conf(replication_interval_ms),
    Broad = get_conf(local_broadcast_interval_ms),
    io_lib:format(
        "./server.sh -r ~b -p ~b -l ~b -u ~b -b ~s",
        [Ring, Prune, Broad, Repl, ?GRB_BRANCH]
    ).

client_command(Command) ->
    io_lib:format("./bench.sh -b ~s ~s", [?LASP_BENCH_BRANCH, Command]).

client_command(Command, Arg) ->
    io_lib:format("./bench.sh -b ~s ~s ~s", [?LASP_BENCH_BRANCH, Command, Arg]).

client_command(Command, Arg1, Arg2, Arg3) ->
    io_lib:format(
        "./bench.sh -b ~s ~s ~s ~s ~s",
        [?LASP_BENCH_BRANCH, Command, Arg1, Arg2, Arg3]
    ).

transfer_script(Node, File) ->
    transfer_from(Node, ?SELF_DIR, File).

transfer_config(Node, File) ->
    transfer_from(Node, ?CONFIG_DIR, File).

transfer_from(Node, Path, File) ->
    Cmd = io_lib:format(
        "scp -i ~s ~s/~s borja.deregil@~s:/home/borja.deregil",
        [?SSH_PRIV_KEY, Path, File, atom_to_list(Node)]
    ),
    safe_cmd(Cmd).

all_nodes(Map) ->
    lists:usort(lists:flatten([S ++ C || #{servers := S, clients := C} <- maps:values(Map)])).

server_nodes(Map) ->
    lists:usort(lists:flatten([N || #{servers := N} <- maps:values(Map)])).

client_nodes(Map) ->
    lists:usort(lists:flatten([N || #{clients := N} <- maps:values(Map)])).

build_bootstrap_info(Map) ->
    maps:fold(fun(_, #{servers := Servers, clients := Clients}, Acc) ->
        [BootstrapNode | _] = lists:usort(Servers),
        lists:foldl(fun(Elt, ListAcc) ->
            ListAcc#{Elt => BootstrapNode}
        end, Acc, Clients)
    end, #{}, Map).

do_in_nodes_seq(Command, Nodes) ->
    Cmd = io_lib:format("~s \"~s\" ~s", [?IN_NODES_PATH, Command, list_to_str(Nodes)]),
    safe_cmd(Cmd).

do_in_nodes_par(Command, Nodes) ->
    pmap(fun(Node) ->
        Cmd = io_lib:format("~s \"~s\" ~s", [?IN_NODES_PATH, Command, atom_to_list(Node)]),
        safe_cmd(Cmd)
    end, Nodes).

list_to_str(Nodes) ->
    lists:foldl(fun(Elem, Acc) -> Acc ++ io_lib:format("~s ", [Elem]) end, "", Nodes).

safe_cmd(Cmd) ->
    case get_conf(silent, false) of
        true -> ok;
        false -> ok = io:format("~s~n", [Cmd])
    end,
    case get_conf(dry_run, false) of
        true -> "";
        false -> os:cmd(Cmd)
    end.

get_conf(Key) -> get_conf(Key, undefined).
get_conf(Key, Default) ->
    case ets:lookup(?CONF, Key) of
        [] -> Default;
        [{Key, Val}] -> Val
    end.

pmap(F, L) ->
    Parent = self(),
    lists:foldl(fun(X, N) ->
        spawn_link(fun() -> Parent ! {pmap, N, F(X)} end),
        N+1
    end, 0, L),
    L2 = [receive {pmap, N, R} -> {N,R} end || _ <- L],
    L3 = lists:keysort(1, L2),
    [R || {_,R} <- L3].

% Alert user with sound when benchmark is finished
alert(Msg) ->
    safe_cmd(io_lib:format("osascript -e 'display notification with title \"~s\"'", [Msg])),
    safe_cmd("afplay /System/Library/Sounds/Glass.aiff"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args([]) -> {error, noargs};
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

parse_args([], Acc) -> {ok, Acc};
parse_args([ [$- | Flag] | Args], Acc) ->
    case Flag of
        [$s] ->
            parse_args(Args, Acc#{silent => true});
        [$d] ->
            parse_args(Args, Acc#{dry_run => true});
        [$f] ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{config => Arg} end);
        [$c] ->
            parse_flag(Flag, Args, fun(Arg) -> parse_command(Arg, Acc) end);
        [$h] ->
            usage(),
            halt(0);
        _ ->
            {error, io_lib:format("badarg ~p", [Flag])}
    end;

parse_args(_, _) ->
    {error, "noarg"}.

parse_flag(Flag, Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args(Rest, Fun(FlagArg));
        _ -> {error, io_lib:format("noarg ~p", [Flag])}
    end.

parse_command(Arg, Acc) ->
    case string:str(Arg, "=") of
        0 -> Acc#{command => list_to_atom(Arg)};
        _ ->
            % crash on malformed command for now
            [Command, CommandArg | _Ignore] = string:tokens(Arg, "="),
            Acc#{command_arg => {true, CommandArg}, command => list_to_atom(Command)}
    end.

required(Opts) ->
    Required = [config, command],
    Valid = lists:all(fun(F) -> maps:is_key(F, Opts) end, Required),
    case Valid of
        false ->
            {error, io_lib:format("Missing required fields: ~p", [Required])};
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
            {error, io_lib:format("Bad command \"~p\", or command needs arg, but none was given", [Command])}
    end.
