#!/usr/bin/env escript

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/aws").

% https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/set-time.html
-define(NTP_IP, "169.254.169.123").

-define(IN_NODES_PATH,
    unicode:characters_to_list(io_lib:format("~s/execute-in-nodes.sh", [?SELF_DIR]))
).

-define(CONFIG_DIR, unicode:characters_to_list(io_lib:format("~s/configuration", [?SELF_DIR]))).

-define(GRB_BRANCH, "master").
-define(LASP_BENCH_BRANCH, "bench_grb").

-define(JOIN_TIMEOUT, timer:minutes(5)).

-define(CONF, configuration).

-define(COMMANDS, [
    {check, false},
    {sync, false},
    {server, false},
    {clients, false},
    {prologue, false},

    {start, false},
    {stop, false},
    {join, false},
    {connect_dcs, false},
    {latencies, false},
    {prepare, false},

    {load, false},
    {bench, false},
    {brutal_client_kill, false},
    {brutal_server_kill, false},

    {restart, false},
    {recompile, false},
    {rebuild, false},
    {cleanup_latencies, false},
    {cleanup, false},
    {pull, true}
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

% todo(borja): Cache information?
preprocess_cluster_map(ClusterMap, Table) ->
    true = ets:insert(Table, {regions, [ atom_to_list(R) || R <- maps:keys(ClusterMap)] }),

    lists:foreach(
        fun({RegionAtom, #{servers := ServerIds, clients := ClientIds}}) ->
            RName = atom_to_list(RegionAtom),

            PrivateKeyPath = io_lib:format("./keys/kp-~s.pem", [RName]),
            true = ets:insert(Table, {{key, RName}, PrivateKeyPath}),

            PublicServers = [ public_ip(RName, Id) || Id <- ServerIds ],
            PublicClients = [ public_ip(RName, Id) || Id <- ClientIds ],

            true = ets:insert(Table, {{servers, public, RName}, PublicServers}),
            true = ets:insert(Table, {{clients, public, RName}, PublicClients}),

            true = ets:insert(Table, [ {{Ip, key}, PrivateKeyPath} || Ip <- PublicServers] ),
            true = ets:insert(Table, [ {{Ip, key}, PrivateKeyPath} || Ip <- PublicClients] ),

            PrivateServers = [ private_ip(RName, Id) || Id <- ServerIds ],
            PrivateClients = [ private_ip(RName, Id) || Id <- ClientIds ],

            true = ets:insert(Table, {{servers, private, RName}, PrivateServers}),
            true = ets:insert(Table, {{clients, private, RName}, PrivateClients}),

            ok
        end,
        maps:to_list(ClusterMap)
    ),

    ok.

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

nonl(S) -> string:trim(S, trailing, "$\n").

main(Args) ->
    case parse_args(Args) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~s~n", [Reason]),
            usage(),
            halt(1);
        {ok, Opts = #{config := ConfigFile}} ->
            _ = ets:new(?CONF, [ordered_set, named_table]),
            {ok, ConfigTerms} = file:consult(ConfigFile),
            {clusters, ClusterMap0} = lists:keyfind(clusters, 1, ConfigTerms),
            ok = preprocess_cluster_map(ClusterMap0, ?CONF),

            io:format("Table: ~p~n", [ets:tab2list(?CONF)]),

            {red_leader_cluster, LeaderCluster} = lists:keyfind(red_leader_cluster, 1, ConfigTerms),
            case maps:is_key(LeaderCluster, ClusterMap0) of
                false ->
                    io:fwrite(standard_error, "Bad cluster map: leader cluster not present ~n", []),
                    halt(1);
                true ->
                    ok
            end,

            Servers = ordsets:from_list(server_nodes()),
            Clients = ordsets:from_list(client_nodes()),
            case ordsets:is_disjoint(Servers, Clients) of
                false ->
                    io:fwrite(
                        standard_error,
                        "Bad cluster map: clients and servers overlap~n",
                        []
                    ),
                    halt(1);
                true ->
                    ok
            end,

            true = ets:insert(?CONF, {dry_run, maps:get(dry_run, Opts, false)}),
            true = ets:insert(?CONF, {silent, maps:get(verbose, Opts, false)}),

            Command = maps:get(command, Opts),
            CommandArg = maps:get(command_arg, Opts, false),

            ok = do_command(Command, CommandArg),
            true = ets:delete(?CONF),
            ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Commands

% prompt_gate(Msg, Default, Fun) ->
%     case prompt(Msg, Default) of
%         true ->
%             Fun(),
%             ok;
%         false ->
%             io:format("Cancelling~n"),
%             ok
%     end.

% prompt(Msg, Default) ->
%     {Prompt, Validate} = case Default of
%         default_no ->
%             {
%                 Msg ++ " [N/y]: ",
%                 fun(I) when I =:= "y\n" orelse I =:= "Y\n" -> true; (_) -> false end
%             };

%         default_yes ->
%             {
%                 Msg ++ " [Y/n]: ",
%                 fun(I) when I =:= "n\n" orelse I =:= "N\n" -> true; (_) -> false end
%             }
%     end,
%     Validate(io:get_line(Prompt)).

% do_command(pull, {true, Path}, ClusterMap) ->
%     DoFun = fun() ->
%         pmap(
%         fun(Node) ->
%             NodeStr = atom_to_list(Node),
%             TargetPath = io_lib:format("~s/~s", [Path, NodeStr]),
%             Cmd0 = io_lib:format("mkdir -p ~s", [TargetPath]),
%             safe_cmd(Cmd0),
%             Cmd1 = io_lib:format(
%                 "scp -i ~s borja.deregil@~s:/home/borja.deregil//sources/lasp-bench/tests/current/* ~s",
%                 [?SSH_PRIV_KEY, NodeStr, TargetPath]
%             ),
%             safe_cmd(Cmd1),
%             Cmd2 = io_lib:format(
%                 "scp -i ~s borja.deregil@~s:/home/borja.deregil/cluster.config ~s",
%                 [?SSH_PRIV_KEY, NodeStr, TargetPath]
%             ),
%             safe_cmd(Cmd2),
%             ok
%         end,
%         client_nodes(ClusterMap)
%         )
%     end,

%     case filelib:is_dir(Path) of
%         false ->
%             DoFun(),
%             ok;
%         true ->
%             prompt_gate(
%                 io_lib:format("Target directory ~s already exists, do you want to overwrite it?", [Path]),
%                 default_no,
%                 DoFun
%             )
%     end;

do_command(brutal_client_kill, _) ->
    NodeNames = client_nodes(),
    Res = do_in_nodes_seq("pkill -9 beam.smp", NodeNames),
    io:format("~p~n", [Res]),
    ok;
do_command(brutal_server_kill, _) ->
    NodeNames = server_nodes(),
    io:format("Server nodes: ~p~n", [NodeNames]),
    Res = do_in_nodes_seq("pkill -9 beam.smp", NodeNames),
    io:format("~p~n", [Res]),
    ok;
do_command(check, _) ->
    check_nodes().
% do_command(sync, _) ->
%     ok = sync_nodes(ClusterMap);
% do_command(server, _) ->
%     ok = prepare_server(ClusterMap);
% do_command(clients, _) ->
%     ok = prepare_lasp_bench(ClusterMap);
% do_command(prologue, Arg) ->
%     ok = check_nodes(ClusterMap),
%     ok = sync_nodes(ClusterMap),
%     ok = prepare_server(ClusterMap),
%     ok = prepare_lasp_bench(ClusterMap),
%     ok = do_command(latencies, Arg, ClusterMap),
%     alert("Prologue finished!"),
%     ok;
% do_command(start, _) ->
%     Rep = do_in_nodes_par(server_command("start"), server_nodes(ClusterMap)),
%     io:format("~p~n", [Rep]),
%     ok;
% do_command(stop, _) ->
%     do_in_nodes_par(server_command("stop"), server_nodes(ClusterMap)),
%     ok;
% do_command(prepare, Arg) ->
%     ok = do_command(join, Arg, ClusterMap),
%     ok = do_command(connect_dcs, Arg, ClusterMap),
%     % ok = do_command(load, Arg, ClusterMap),
%     alert("Prepare finished!"),
%     ok;
% do_command(join, _) ->
%     NodeNames = server_nodes(ClusterMap),
%     Parent = self(),
%     Reference = erlang:make_ref(),
%     ChildFun = fun() ->
%         Reply = do_in_nodes_seq(server_command("join"), [hd(NodeNames)]),
%         Parent ! {Reference, Reply}
%     end,
%     Start = erlang:timestamp(),
%     ChildPid = erlang:spawn(ChildFun),
%     receive
%         {Reference, Reply} ->
%             End = erlang:timestamp(),
%             io:format("~p~n", [Reply]),
%             io:format("Ring done after ~p~n", [timer:now_diff(End, Start)]),
%             ok
%     after ?JOIN_TIMEOUT ->
%         io:fwrite(standard_error, "Ring timed out after ~b milis~n", [?JOIN_TIMEOUT]),
%         erlang:exit(ChildPid, kill),
%         error
%     end;
% do_command(connect_dcs, _) ->
%     [MainNode | _] = server_nodes(ClusterMap),
%     Rep = do_in_nodes_seq(
%         server_command("connect_dcs"),
%         [MainNode]
%     ),
%     io:format("~p~n", [Rep]),
%     ok;
% do_command(load, _) ->
%     NodeNames = client_nodes(ClusterMap),
%     TargetNode = hd(server_nodes(ClusterMap)),
%     io:format("~p~n", [
%         do_in_nodes_seq(client_command("-y load", atom_to_list(TargetNode)), [hd(NodeNames)])
%     ]),
%     ok;
% do_command(latencies, _) ->
%     ok = maps:fold(
%         fun(ClusterName, #{servers := ClusterServers}, _Acc) ->
%             io:format(
%                 "~p~n",
%                 [
%                     do_in_nodes_par(
%                         server_command("tc", atom_to_list(ClusterName)),
%                         ClusterServers
%                     )
%                 ]
%             ),
%             ok
%         end,
%         ok,
%         ClusterMap
%     ),
%     ok;
% do_command(bench, _) ->
%     NodeNames = client_nodes(ClusterMap),
%     BootstrapInfo = build_bootstrap_info(ClusterMap),
%     BootstrapPort = 7878,
%     pmap(fun(Node) -> transfer_config(Node, "run.config") end, NodeNames),

%     pmap(
%         fun(Node) ->
%             Command = client_command(
%                 "run",
%                 "/home/borja.deregil/run.config",
%                 atom_to_list(maps:get(Node, BootstrapInfo)),
%                 integer_to_list(BootstrapPort)
%             ),
%             Cmd = io_lib:format("~s \"~s\" ~s", [?IN_NODES_PATH, Command, atom_to_list(Node)]),
%             safe_cmd(Cmd)
%         end,
%         NodeNames
%     ),
%     alert("Benchmark finished!"),
%     ok;
% do_command(recompile, _) ->
%     io:format("~p~n", [do_in_nodes_par(server_command("recompile"), server_nodes(ClusterMap))]),
%     ok;
% do_command(restart, _) ->
%     io:format("~p~n", [do_in_nodes_par(server_command("restart"), server_nodes(ClusterMap))]),
%     ok;
% do_command(rebuild, _) ->
%     DBNodes = server_nodes(ClusterMap),
%     ClientNodes = client_nodes(ClusterMap),

%     do_in_nodes_par(server_command("rebuild"), DBNodes),
%     do_in_nodes_par(client_command("rebuild"), ClientNodes),
%     ok;
% do_command(cleanup_latencies, _) ->
%     ServerNodes = server_nodes(ClusterMap),
%     io:format("~p~n", [do_in_nodes_par(server_command("tclean"), ServerNodes)]),
%     ok;
% do_command(cleanup, _) ->
%     AllNodes = all_nodes(ClusterMap),
%     ServerNodes = server_nodes(ClusterMap),

%     io:format("~p~n", [do_in_nodes_par(server_command("tclean"), ServerNodes)]),
%     io:format("~p~n", [do_in_nodes_par("rm -rf sources; mkdir -p sources", AllNodes)]),
%     ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Command Impl

check_nodes() ->
    io:format("Checking that all nodes are up and on the correct governor mode~n"),

    AllNodes = all_nodes(),

    UptimeRes = do_in_nodes_par("uptime", AllNodes),
    false = lists:any(fun(Res) -> string:str(Res, "timed out") =/= 0 end, UptimeRes),

    % Transfer server, bench and cluster config
    io:format("Transfering benchmark config files (server, bench, cluster)...~n"),
    pmap(
        fun(Node) ->
            transfer_script(Node, "server.escript"),
            transfer_script(Node, "bench.sh"),
            transfer_config(Node, "cluster.config")
        end,
        AllNodes
    ),
    ok.

% sync_nodes(ClusterMap) ->
%     io:format("Resyncing NTP on all nodes~n"),
%     AllNodes = all_nodes(ClusterMap),
%     _ = do_in_nodes_par("sudo service ntp stop", AllNodes),
%     _ = do_in_nodes_par(io_lib:format("sudo ntpd -gq ~s", [?NTP_IP]), AllNodes),
%     _ = do_in_nodes_par("sudo service ntp start", AllNodes),
%     ok.

% prepare_server(ClusterMap) ->
%     NodeNames = server_nodes(ClusterMap),
%     io:format("~p~n", [do_in_nodes_par(server_command("download"), NodeNames)]),
%     _ = do_in_nodes_par(server_command("compile"), NodeNames),
%     io:format("~p~n", [do_in_nodes_par(server_command("start"), NodeNames)]),
%     ok.

% prepare_lasp_bench(ClusterMap) ->
%     NodeNames = client_nodes(ClusterMap),
%     io:format("~p~n", [do_in_nodes_par(client_command("dl"), NodeNames)]),
%     _ = do_in_nodes_par(client_command("compile"), NodeNames),
%     ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Util

% server_command(Command) ->
%     io_lib:format("./server.escript -v -f /home/borja.deregil/cluster.config -c ~s", [Command]).

% server_command(Command, Arg) ->
%     io_lib:format("./server.escript -v -f /home/borja.deregil/cluster.config -c ~s=~s", [
%         Command,
%         Arg
%     ]).

% client_command(Command) ->
%     io_lib:format("./bench.sh -b ~s ~s", [?LASP_BENCH_BRANCH, Command]).

% client_command(Command, Arg) ->
%     io_lib:format("./bench.sh -b ~s ~s ~s", [?LASP_BENCH_BRANCH, Command, Arg]).

% client_command(Command, Arg1, Arg2, Arg3) ->
%     io_lib:format(
%         "./bench.sh -b ~s ~s ~s ~s ~s",
%         [?LASP_BENCH_BRANCH, Command, Arg1, Arg2, Arg3]
%     ).

transfer_script(Node, File) ->
    transfer_from(Node, ?SELF_DIR, File).

transfer_config(Node, File) ->
    transfer_from(Node, ?CONFIG_DIR, File).

transfer_from(Node, Path, File) ->
    KeyPath = ets:lookup_element(?CONF, {Node, key}, 2),
    Cmd = io_lib:format(
        "scp -i ~s ~s/~s ubuntu@~s:/home/ubuntu",
        [KeyPath, Path, File, atom_to_list(Node)]
    ),
    safe_cmd(Cmd).

all_nodes() ->
    All = ets:select(?CONF, [{ {{'_', public, '_'}, '$1'}, [], ['$1'] }]),
    lists:usort(lists:foldl(fun(L, Acc) -> Acc ++ L end, [], All)).

server_nodes() ->
    All = ets:select(?CONF, [{ {{servers, public, '_'}, '$1'}, [], ['$1'] }]),
    lists:usort(lists:foldl(fun(L, Acc) -> Acc ++ L end, [], All)).

client_nodes() ->
    All = ets:select(?CONF, [{ {{clients, public, '_'}, '$1'}, [], ['$1'] }]),
    lists:usort(lists:foldl(fun(L, Acc) -> Acc ++ L end, [], All)).

% build_bootstrap_info(Map) ->
%     maps:fold(
%         fun(_, #{servers := Servers, clients := Clients}, Acc) ->
%             [BootstrapNode | _] = lists:usort(Servers),
%             lists:foldl(
%                 fun(Elt, ListAcc) ->
%                     ListAcc#{Elt => BootstrapNode}
%                 end,
%                 Acc,
%                 Clients
%             )
%         end,
%         #{},
%         Map
%     ).

do_in_nodes_seq(Command, Nodes) ->
    lists:foreach(fun(IP) ->
        Cmd = io_lib:format(
            "~s -s ~s \"~s\" ~s",
            [?IN_NODES_PATH, ets:lookup_element(?CONF, {IP, key}, 2), Command, IP]
        ),
        safe_cmd(Cmd)
    end, Nodes).

do_in_nodes_par(Command, Nodes) ->
    % todo(borja): Can't access ets table inside process, fix it
    pmap(
        fun(IP) ->
            Cmd = io_lib:format(
                "~s -s ~s \"~s\" ~s",
                [?IN_NODES_PATH, ets:lookup_element(?CONF, {IP, key}, 2), Command, IP]
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