#!/usr/bin/env escript

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/aws").

% https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/set-time.html
-define(NTP_IP, "169.254.169.123").

-define(IN_NODES_PATH,
    unicode:characters_to_list(io_lib:format("~s/execute-in-nodes.sh", [?SELF_DIR]))
).

-define(CONFIG_DIR,
    unicode:characters_to_list(io_lib:format("~s/configuration", [?SELF_DIR]))
).

-define(GRB_BRANCH, "master").
-define(LASP_BENCH_BRANCH, "bench_grb").

-define(JOIN_TIMEOUT, timer:minutes(5)).
-define(CONF, configuration).
-define(PROC_FILE_KEY, '$processed_path').
-define(CLIENT_PROFILE_KEY, '$lasp_bench_rebar_profile').
-define(DIGEST_RANGE, trunc(math:pow(2, 32))).

-define(RUN_CONFIG, "run.config").
-define(RUBIS_PROPS, "rubis_properties.config").

-define(COMMANDS, [
    {init, false},
    {check, false},
    {sync, false},
    {server, false},
    {clients, false},
    {prologue, false},

    {start, false},
    {stop, false},
    {join, false},
    {connect_dcs, false},
    {prepare, false},

    {grb_load, false},
    {rubis_load, false},
    {bench, false},
    {brutal_client_kill, false},
    {brutal_server_kill, false},

    {restart, false},
    {recompile, false},
    {recompile_clients, false},
    {rebuild, false},
    {rebuild_grb, false},
    {rebuild_clients, false},
    {cleanup, false},
    {cleanup_servers, false},
    {cleanup_clients, false},
    {visibility, false},
    {measurements, false},
    {pull, true},
    {terminate, false}
]).

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
            _ = ets:new(?CONF, [ordered_set, named_table]),
            true = ets:insert(?CONF, {dry_run, maps:get(dry_run, Opts, false)}),
            true = ets:insert(?CONF, {silent, maps:get(verbose, Opts, false)}),
            case load_processed_data(?CONF, ConfigFile) of
                {error, no_leader} ->
                    true = ets:delete(?CONF),
                    io:fwrite(
                        standard_error,
                        "Bad cluster map: leader cluster not present ~n",
                        []
                    ),
                    halt(1);

                {error, cluster_overlap} ->
                    true = ets:delete(?CONF),
                    io:fwrite(
                        standard_error,
                        "Bad cluster map: clients and servers overlap~n",
                        []
                    ),
                    halt(1);

                ok ->
                    Command = maps:get(command, Opts),
                    Cmd = case maps:get(command_arg, Opts, false) of
                        false -> Command;
                        {true, Arg} -> {Command, Arg}
                    end,
                    ok = do_command(Cmd),
                    true = ets:delete(?CONF),
                    ok
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prompt_gate(Msg, Default, Fun) ->
    case prompt(Msg, Default) of
        true ->
            Fun(),
            ok;
        false ->
            io:format("Cancelling~n"),
            ok
    end.

prompt(Msg, Default) ->
    {Prompt, Validate} = case Default of
        default_no ->
            {
                Msg ++ " [N/y]: ",
                fun(I) when I =:= "y\n" orelse I =:= "Y\n" -> true; (_) -> false end
            };

        default_yes ->
            {
                Msg ++ " [Y/n]: ",
                fun(I) when I =:= "n\n" orelse I =:= "N\n" -> true; (_) -> false end
            }
    end,
    Validate(io:get_line(Prompt)).

do_command(visibility) ->
    MainNodes = main_region_server_nodes(),
    pmap(
        fun({Region, NodeIP}) ->
            do_in_nodes_seq(server_command("visibility", Region), [{Region, NodeIP}])
        end,
        MainNodes
    ),
    ok;

do_command(measurements) ->
    MainNodes = main_region_server_nodes(),
    pmap(
        fun({Region, NodeIP}) ->
            do_in_nodes_seq(server_command("measurements", Region), [{Region, NodeIP}])
        end,
        MainNodes
    ),
    ok;

do_command({pull, Path}) ->
    PullClients = fun() ->
        pmap(
        fun({Region, NodeIP}) ->
            NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
            TargetPath = io_lib:format("~s/aws-~s-~s", [Path, Region, NodeIP]),
            Cmd0 = io_lib:format("mkdir -p ~s", [TargetPath]),
            safe_cmd(Cmd0),
            Cmd1 = io_lib:format(
                "scp -i ~s ubuntu@~s:/home/ubuntu/sources/lasp-bench/tests/current/* ~s",
                [NodeKey, NodeIP, TargetPath]
            ),
            safe_cmd(Cmd1),
            Cmd2 = io_lib:format(
                "scp -i ~s ubuntu@~s:/home/ubuntu/cluster.config ~s",
                [NodeKey, NodeIP, TargetPath]
            ),
            safe_cmd(Cmd2),
            Cmd3 = io_lib:format(
                "scp -i ~s ubuntu@~s:/home/ubuntu/pcluster.config ~s",
                [NodeKey, NodeIP, TargetPath]
            ),
            safe_cmd(Cmd3),
            Cmd4 = io_lib:format(
                "scp -i ~s ubuntu@~s:/home/ubuntu/~s ~s",
                [NodeKey, NodeIP, ?RUBIS_PROPS, TargetPath]
            ),
            safe_cmd(Cmd4),
            ok
        end,
        client_nodes()
        )
    end,

    % PullVisibility = fun() ->
    %     pmap(
    %         fun({Region, NodeIP}) ->
    %             NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
    %             TargetFile = io_lib:format("~s/visibility-aws-~s.bin", [Path, Region]),
    %             Cmd = io_lib:format(
    %                 "scp -i ~s ubuntu@~s:/home/ubuntu/visibility.bin ~s",
    %                 [NodeKey, NodeIP, TargetFile]
    %             ),
    %             safe_cmd(Cmd),
    %             ok
    %         end,
    %         main_region_server_nodes()
    %     )
    % end,

    % PullMeasurements = fun() ->
    %     pmap(
    %         fun({Region, NodeIP}) ->
    %             NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
    %             TargetFile = io_lib:format("~s/measurements-aws-~s.bin", [Path, Region]),
    %             Cmd = io_lib:format(
    %                 "scp -i ~s ubuntu@~s:/home/ubuntu/measurements.bin ~s",
    %                 [NodeKey, NodeIP, TargetFile]
    %             ),
    %             safe_cmd(Cmd),
    %             ok
    %         end,
    %         main_region_server_nodes()
    %     )
    % end,

    % PullCaptures = fun() ->
    %     pmap(
    %         fun({Region, NodeIP}) ->
    %             NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
    %             TargetFile = io_lib:format("~s/capture-aws-~s.cap", [Path, Region]),
    %             Cmd = io_lib:format(
    %                 "scp -i ~s ubuntu@~s:/home/ubuntu/capture.cap ~s",
    %                 [NodeKey, NodeIP, TargetFile]
    %             ),
    %             safe_cmd(Cmd),
    %             ok
    %         end,
    %         main_region_server_nodes()
    %     )
    % end,

    DoFun = fun() ->
        _ = PullClients(),
        % _ = PullVisibility(),
        % _ = PullMeasurements(),
        % _ = PullCaptures(),
        ok
    end,

    case filelib:is_dir(Path) of
        false ->
            DoFun(),
            ok;
        true ->
            prompt_gate(
                io_lib:format("Target directory ~s already exists, do you want to overwrite it?", [Path]),
                default_no,
                DoFun
            )
    end;

do_command(terminate) ->
    Instances = all_instance_ids(),
    io:format("Will terminate instances~n~p~n", [Instances]),
    prompt_gate("Are you sure you want to proceed?", default_no, fun() ->
        lists:foreach(fun({Region, Instance}) ->
            terminate_instance(Region, Instance)
        end, Instances)
    end);

do_command(brutal_client_kill) ->
    NodeNames = client_nodes(),
    Res = do_in_nodes_par("pkill -9 beam.smp", NodeNames),
    io:format("~p~n", [Res]),
    ok;

do_command(brutal_server_kill) ->
    io:format("Will abruptly stop nodes~n"),
    prompt_gate("Are you sure you want to proceed?", default_no, fun() ->
        NodeNames = server_nodes(),
        Res = do_in_nodes_par("pkill -9 beam.smp", NodeNames),
        io:format("~p~n", [Res])
    end),
    ok;

% Does nothing, useful to preload the ip data
do_command(init) ->
    ok;

do_command(check) ->
    check_nodes();

do_command(sync) ->
    ok = sync_nodes();

do_command(server) ->
    ok = prepare_server();

do_command(clients) ->
    ok = prepare_lasp_bench();

do_command(prologue) ->
    ok = check_nodes(),
    ok = sync_nodes(),
    ok = prepare_server(),
    ok = prepare_lasp_bench(),
    alert("Prologue finished!"),
    ok;

do_command(start) ->
    Rep = do_in_nodes_par(server_command("start"), server_nodes()),
    io:format("~p~n", [Rep]),
    ok;

do_command(stop) ->
    do_in_nodes_par(server_command("stop"), server_nodes()),
    ok;

do_command(prepare) ->
    ok = do_command(join),
    ok = do_command(connect_dcs),
    alert("Prepare finished!"),
    ok;

do_command(join) ->
    MainNodes = main_region_server_nodes(),
    Parent = self(),
    Reference = erlang:make_ref(),
    SpawnFun = fun() ->
        pmap(
            fun({Region, NodeIP}) ->
                do_in_nodes_seq(server_command("join", Region), [{Region, NodeIP}])
            end,
            MainNodes
        ),
        Parent ! {ok, Reference}
    end,
    Start = erlang:timestamp(),
    ChildPid = erlang:spawn(SpawnFun),
    receive
        {ok, Reference} ->
            End = erlang:timestamp(),
            io:format("Ring done after ~p~n", [timer:now_diff(End, Start)]),
            ok
    after ?JOIN_TIMEOUT ->
        io:fwrite(standard_error, "Ring timed out after ~b milis~n", [?JOIN_TIMEOUT]),
        erlang:exit(ChildPid, kill),
        error
    end;

do_command(connect_dcs) ->
    MainNode = hd(server_nodes()),
    Rep = do_in_nodes_seq(
        server_command("connect_dcs"),
        [MainNode]
    ),
    io:format("~p~n", [Rep]),
    ok;

do_command(grb_load) ->
    pmap(
        fun({Region, Node}) ->
            transfer_config(Region, Node, ?RUBIS_PROPS)
        end,
        client_nodes()
    ),
    Start = erlang:timestamp(),
    pmap(
        fun({Region, NodeIP}) ->
            TargetServer = main_private_ip(Region),
            NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
            CommandFun = client_command(
                "-y load_grb",
                TargetServer,
                unicode:characters_to_list(io_lib:format("/home/ubuntu/~s", [?RUBIS_PROPS]))
            ),
            Cmd = io_lib:format(
                "~s -i ~s \"~s\" ~s",
                [?IN_NODES_PATH, NodeKey, CommandFun(Region), NodeIP]
            ),
            safe_cmd(Cmd)
        end,
        main_region_client_nodes()
    ),
    End = erlang:timestamp(),
    Took = timer:now_diff(End, Start),
    alert(
        io_lib:format(
            "GRB load finished after ~b seconds (~b micros)~n",
            [erlang:trunc(Took div 1_000_000), Took]
        )
    ),
    ok;

do_command(rubis_load) ->
    pmap(
        fun({Region, Node}) ->
            transfer_config(Region, Node, ?RUBIS_PROPS)
        end,
        client_nodes()
    ),
    Start = erlang:timestamp(),
    pmap(
        fun({Region, NodeIP}) ->
            TargetServer = main_private_ip(Region),
            NodeKey = ets:lookup_element(?CONF, {NodeIP, Region, key}, 2),
            CommandFun = client_command(
                "-y load_rubis",
                TargetServer,
                unicode:characters_to_list(io_lib:format("/home/ubuntu/~s", [?RUBIS_PROPS]))
            ),
            Cmd = io_lib:format(
                "~s -i ~s \"~s\" ~s",
                [?IN_NODES_PATH, NodeKey, CommandFun(Region), NodeIP]
            ),
            safe_cmd(Cmd)
        end,
        main_region_client_nodes()
    ),
    End = erlang:timestamp(),
    Took = timer:now_diff(End, Start),
    alert(
        io_lib:format(
            "Rubis load finished after ~b seconds (~b micros)~n",
            [erlang:trunc(Took div 1_000_000), Took]
        )
    ),
    ok;

do_command(bench) ->
    NodeNames = client_nodes(),
    BootstrapPort = 7878,
    pmap(fun({Region, Node}) -> transfer_config(Region, Node, ?RUN_CONFIG) end, NodeNames),

    pmap(
        fun({Region, NodeIP}) ->
            BootstrapIP = main_private_ip(Region),
            CommandFun = client_command(
                "run",
                unicode:characters_to_list(io_lib:format("/home/ubuntu/~s", [?RUN_CONFIG])),
                BootstrapIP,
                integer_to_list(BootstrapPort)
            ),
            Cmd = io_lib:format(
                "~s -i ~s \"~s\" ~s",
                [?IN_NODES_PATH, ets:lookup_element(?CONF, {NodeIP, Region, key}, 2), CommandFun(Region), NodeIP]
            ),
            safe_cmd(Cmd)
        end,
        NodeNames
    ),
    alert("Benchmark finished!"),
    ok;

do_command(recompile) ->
    io:format("~p~n", [do_in_nodes_par(server_command("recompile"), server_nodes())]),
    ok;

do_command(recompile_clients) ->
    do_in_nodes_par(client_command("compile"), client_nodes()),
    ok;

do_command(restart) ->
    io:format("~p~n", [do_in_nodes_par(server_command("restart"), server_nodes())]),
    ok;

do_command(rebuild) ->
    do_command(rebuild_grb),
    do_command(rebuild_clients),
    ok;

do_command(rebuild_grb) ->
    do_in_nodes_par(server_command("rebuild"), server_nodes()),
    ok;

do_command(rebuild_clients) ->
    do_in_nodes_par(client_command("rebuild"), client_nodes()),
    ok;

do_command(cleanup) ->
    do_command(cleanup_servers),
    do_command(cleanup_clients),
    ok;

do_command(cleanup_servers) ->
    AllNodes = server_nodes(),
    io:format("~p~n", [do_in_nodes_par("rm -rf sources; mkdir -p sources", AllNodes)]),
    ok;

do_command(cleanup_clients) ->
    AllNodes = client_nodes(),
    io:format("~p~n", [do_in_nodes_par("rm -rf sources; mkdir -p sources", AllNodes)]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Command Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_nodes() ->
    io:format("Checking that all nodes are up~n"),

    AllNodes = all_nodes(),
    UptimeRes = do_in_nodes_par("uptime", AllNodes),
    false = lists:any(fun(Res) -> string:str(Res, "timed out") =/= 0 end, UptimeRes),

    % Transfer server, bench and cluster config
    io:format("Transfering benchmark config files (server, bench, cluster)...~n"),
    pmap(
        fun({Region, Node}) ->
            transfer_script(Region, Node, "server.escript"),
            transfer_script(Region, Node, "bench.sh"),
            transfer_script(Region, Node, "my_ip"),
            transfer_script(Region, Node, "leader_ip"),
            transfer_config(Region, Node, "cluster.config"),
            transfer_direct(
                Region,
                Node,
                ets:lookup_element(?CONF, ?PROC_FILE_KEY, 2),
                "pcluster.config"
            )
        end,
        AllNodes
    ),
    ok.

sync_nodes() ->
    io:format("Resyncing NTP on all nodes~n"),
    AllNodes = all_nodes(),
    _ = do_in_nodes_par("sudo service ntp stop", AllNodes),
    _ = do_in_nodes_par(io_lib:format("sudo ntpd -gq ~s", [?NTP_IP]), AllNodes),
    _ = do_in_nodes_par("sudo service ntp start", AllNodes),
    ok.

prepare_server() ->
    NodeNames = server_nodes(),
    io:format("~p~n", [do_in_nodes_par(server_command("download"), NodeNames)]),
    _ = do_in_nodes_par(server_command("compile"), NodeNames),
    io:format("~p~n", [do_in_nodes_par(server_command("start"), NodeNames)]),
    ok.

prepare_lasp_bench() ->
    NodeNames = client_nodes(),
    io:format("~p~n", [do_in_nodes_par(client_command("dl"), NodeNames)]),
    _ = do_in_nodes_par(client_command("compile"), NodeNames),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

server_command(Command) ->
    fun(Region) ->
        io_lib:format(
            "./server.escript -v -r ~s -f /home/ubuntu/cluster.config -p /home/ubuntu/pcluster.config -c ~s",
            [Region, Command]
        )
    end.

server_command(Command, Arg) ->
    fun(Region) ->
        io_lib:format(
            "./server.escript -v -r ~s -f /home/ubuntu/cluster.config -p /home/ubuntu/pcluster.config -c ~s=~s",
            [Region, Command,Arg]
        )
    end.

client_command(Command) ->
    Profile = ets:lookup_element(?CONF, ?CLIENT_PROFILE_KEY, 2),
    fun(_) -> io_lib:format("./bench.sh -b ~s -p ~p ~s", [?LASP_BENCH_BRANCH, Profile, Command]) end.

client_command(Command, Arg1, Arg2) ->
    Profile = ets:lookup_element(?CONF, ?CLIENT_PROFILE_KEY, 2),
    fun(_) ->
        io_lib:format(
            "./bench.sh -b ~s -p ~p ~s ~s ~s",
            [?LASP_BENCH_BRANCH, Profile, Command, Arg1, Arg2]
        )
    end.

client_command(Command, Arg1, Arg2, Arg3) ->
    Profile = ets:lookup_element(?CONF, ?CLIENT_PROFILE_KEY, 2),
    fun(_) ->
        io_lib:format(
            "./bench.sh -b ~s -p ~p ~s ~s ~s ~s",
            [?LASP_BENCH_BRANCH, Profile, Command, Arg1, Arg2, Arg3]
        )
    end.

transfer_script(Region, IP, File) ->
    transfer_from(Region, IP, ?SELF_DIR, File).

transfer_config(Region, IP, File) ->
    transfer_from(Region, IP, ?CONFIG_DIR, File).

transfer_from(Region, IP, Path, File) ->
    KeyPath = ets:lookup_element(?CONF, {IP, Region, key}, 2),
    Cmd = io_lib:format(
        "scp -i ~s ~s/~s ubuntu@~s:/home/ubuntu",
        [KeyPath, Path, File, IP]
    ),
    safe_cmd(Cmd).

transfer_direct(Region, IP, FilePath, TargetName) ->
    KeyPath = ets:lookup_element(?CONF, {IP, Region, key}, 2),
    Cmd = io_lib:format(
        "scp -i ~s ~s ubuntu@~s:/home/ubuntu/~s",
        [KeyPath, filename:absname(FilePath), IP, TargetName]
    ),
    safe_cmd(Cmd).

all_nodes() ->
    All = ets:select(?CONF, [{ {{'_', public, '$1'}, '$2'}, [], [{{'$1', '$2'}}] }]),
    lists:usort(lists:foldl(fun({R, L}, Acc) -> Acc ++ [ {R, N} || N <- L] end, [], All)).

all_instance_ids() ->
    All = ets:select(?CONF, [{ {{instance_ids, '$1'}, '$2'}, [], [{{'$1', '$2'}}]  }]),
    lists:usort(lists:foldl(fun({R, L}, Acc) -> Acc ++ [ {R, N} || N <- L] end, [], All)).

server_nodes() ->
    All = ets:select(?CONF, [{ {{servers, public, '$1'}, '$2'}, [], [{{'$1', '$2'}}] }]),
    lists:usort(lists:foldl(fun({R, L}, Acc) -> Acc ++ [ {R, N} || N <- L] end, [], All)).

main_region_server_nodes() ->
    ets:select(?CONF, [{ {{servers, public, '$1'}, [ '$2' | '_' ]}, [], [{{'$1', '$2'}}] }]).

client_nodes() ->
    All = ets:select(?CONF, [{ {{clients, public, '$1'}, '$2'}, [], [{{'$1', '$2'}}] }]),
    lists:usort(lists:foldl(fun({R, L}, Acc) -> Acc ++ [ {R, N} || N <- L] end, [], All)).

main_region_client_nodes() ->
    ets:select(?CONF, [{ {{clients, public, '$1'}, [ '$2' | '_' ]}, [], [{{'$1', '$2'}}] }]).

main_private_ip(Region) ->
    [Node] = ets:select(?CONF, [{ {{servers, private, Region}, ['$1' | '_' ]}, [], ['$1']  }]),
    Node.

do_in_nodes_seq(CommandFun, Nodes) ->
    lists:foreach(fun({Region, IP}) ->
        Command = if
            erlang:is_function(CommandFun) -> CommandFun(Region);
            true -> CommandFun
        end,
        Cmd = io_lib:format(
            "~s -i ~s \"~s\" ~s",
            [?IN_NODES_PATH, ets:lookup_element(?CONF, {IP, Region, key}, 2), Command, IP]
        ),
        safe_cmd(Cmd)
    end, Nodes).

do_in_nodes_par(CommandFun, Nodes) ->
    pmap(
        fun({Region, IP}) ->
            Command = if
                erlang:is_function(CommandFun) -> CommandFun(Region);
                true -> CommandFun
            end,
            Cmd = io_lib:format(
                "~s -i ~s \"~s\" ~s",
                [?IN_NODES_PATH, ets:lookup_element(?CONF, {IP, Region, key}, 2), Command, IP]
            ),
            safe_cmd(Cmd)
        end,
        Nodes
    ).

safe_cmd(Cmd) ->
    case get_conf(silent, false) of
        true -> ok;
        false -> ok = io:format("~s~n", [Cmd])
    end,
    case get_conf(dry_run, false) of
        true -> "";
        false -> os:cmd(Cmd)
    end.

get_conf(Key, Default) ->
    case ets:lookup(?CONF, Key) of
        [] -> Default;
        [{Key, Val}] -> Val
    end.

pmap(F, L) ->
    Parent = self(),
    lists:foldl(
        fun(X, N) ->
            spawn_link(fun() -> Parent ! {pmap, N, F(X)} end),
            N + 1
        end,
        0,
        L
    ),
    L2 = [
        receive
            {pmap, N, R} -> {N, R}
        end
        || _ <- L
    ],
    L3 = lists:keysort(1, L2),
    [R || {_, R} <- L3].

% Alert user with sound when benchmark is finished
alert(Msg) ->
    safe_cmd(io_lib:format("osascript -e 'display notification with title \"~s\"'", [Msg])),
    safe_cmd("afplay /System/Library/Sounds/Glass.aiff"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cluster Info Loading / Translation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_processed_data(Table, ConfigPath) ->
    ConfigDir = filename:dirname(ConfigPath),
    {ok, Terms0} = file:consult(ConfigPath),

    {clusters, ClusterMap} = lists:keyfind(clusters, 1, Terms0),
    {red_leader_cluster, LeaderCluster} = lists:keyfind(red_leader_cluster, 1, Terms0),
    {lasp_bench_rebar_profile, ClientProfile} = lists:keyfind(lasp_bench_rebar_profile, 1, Terms0),

    Digest = erlang:phash2(ClusterMap, ?DIGEST_RANGE),
    ProcessedPath = filename:join(ConfigDir, io_lib:format("pcluster-~b.config", [Digest])),
    ProcTerms = case filelib:is_file(ProcessedPath) of
        true ->
            io:format("Reading cached data~n"),
            {ok, Terms1} = file:consult(ProcessedPath),
            Terms1;

        false ->
            io:format("Processing data~n"),
            Terms1 = preprocess_cluster_map(ClusterMap),
            Format = fun(T) -> io_lib:format("~tp.~n", [T]) end,
            Serialized = lists:map(Format, Terms1),
            ok = file:write_file(ProcessedPath, Serialized),
            Terms1
    end,

    true = ets:insert(Table, ProcTerms),
    true = ets:insert(Table, {?PROC_FILE_KEY, ProcessedPath}),
    true = ets:insert(Table, {?CLIENT_PROFILE_KEY, ClientProfile}),
    if
        not (is_map_key(LeaderCluster, ClusterMap)) ->
            {error, no_leader};
        true ->
            Servers = ordsets:from_list(server_nodes()),
            Clients = ordsets:from_list(client_nodes()),
            case ordsets:is_disjoint(Servers, Clients) of
                false ->
                    {error, cluster_overlap};
                true ->
                    ok
            end
    end.

preprocess_cluster_map(ClusterMap) ->
    Terms0 = [{regions, [ atom_to_list(R) || R <- maps:keys(ClusterMap)]}],
    lists:foldl(
        fun({RegionAtom, #{servers := ServerIds, clients := ClientIds}}, Acc) ->
            RName = atom_to_list(RegionAtom),
            RelPrivateKeyPath = io_lib:format("./keys/kp-~s.pem", [RName]),
            PrivateKeyPath = filename:absname_join(?SELF_DIR, RelPrivateKeyPath),

            PublicServers = [ public_ip(RName, Id) || Id <- ServerIds ],
            PrivateServers = [ private_ip(RName, Id) || Id <- ServerIds ],
            %% So we can map private to public addresses
            PrivateMappings = lists:zipwith(
                fun(Priv, Pub) ->
                    {{servers, public_mapping, RName, Priv}, Pub}
                end,
                PrivateServers,
                PublicServers
            ),
            PublicClients = [ public_ip(RName, Id) || Id <- ClientIds ],
            PrivateClients = [ private_ip(RName, Id) || Id <- ClientIds ],

            ServerKeys = [ {{Ip, RName, key}, PrivateKeyPath} || Ip <- PublicServers],
            ClientKeys = [ {{Ip, RName, key}, PrivateKeyPath} || Ip <- PublicClients],
            [
                {{key, RName}, PrivateKeyPath},
                {{servers, public, RName}, PublicServers},
                {{clients, public, RName}, PublicClients},
                {{servers, private, RName}, PrivateServers},
                {{clients, private, RName}, PrivateClients},
                {{instance_ids, RName}, ServerIds ++ ClientIds}
            ] ++ ServerKeys ++ ClientKeys ++ PrivateMappings ++ Acc
        end,
        Terms0,
        maps:to_list(ClusterMap)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AWS Util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

public_ip(Region, InstanceId) ->
    Command = io_lib:format(
        "aws ec2 describe-instances --instance-ids ~s --output text --region ~s --query 'Reservations[*].Instances[*].PublicIpAddress'",
        [InstanceId, Region]
    ),
    nonl(os:cmd(Command)).

private_ip(Region, InstanceId) ->
    Command = io_lib:format(
        "aws ec2 describe-instances --instance-ids ~s --output text --region ~s --query 'Reservations[*].Instances[*].PrivateIpAddress'",
        [InstanceId, Region]
    ),
    nonl(os:cmd(Command)).

terminate_instance(Region, InstanceId) ->
    Command = io_lib:format(
        "aws ec2 terminate-instances --instance-ids ~s --output text --region ~s",
        [InstanceId, Region]
    ),
    nonl(os:cmd(Command)).

nonl(S) -> string:trim(S, trailing, "$\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args([]) ->
    {error, noargs};
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

parse_args([], Acc) ->
    {ok, Acc};
parse_args([[$- | Flag] | Args], Acc) ->
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
        0 ->
            Acc#{command => list_to_atom(Arg)};
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

check_command(Opts = #{command := Command}) ->
    case lists:member({Command, maps:is_key(command_arg, Opts)}, ?COMMANDS) of
        true ->
            {ok, Opts};
        false ->
            {error,
                io_lib:format("Bad command \"~p\", or command needs arg, but none was given", [
                    Command
                ])}
    end.
