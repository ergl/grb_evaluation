#!/usr/bin/env escript

-mode(compile).
-type latencies() :: #{atom() => [tuple()]}.
-type latencies_graph() :: digraph:graph().

-export([main/1]).

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(
        standard_error,
        "Usage: ~s -c <config-file> -r <region-name> -f <fault-tolerance>~n",
        [Name]
    ).

main(Args) ->
    case parse_args(Args) of
        error ->
            usage(),
            halt(1);
        {ok, #{config := ConfigFile, region := RegionStr, f_factor := FStr}} ->
            Region = list_to_atom(RegionStr),
            FaultTolerance = list_to_integer(FStr),
            {ok, Config} = file:consult(ConfigFile),
            {ok, Latencies} = get_config_key(latencies, Config),
            {ok, Leader} = get_config_key(red_leader_cluster, Config),
            if
                not is_map_key(Region, Latencies) ->
                    io:fwrite(standard_error, "latencies map doesn't contain site ~p~n", [Region]),
                    usage(),
                    halt(1);
                FaultTolerance < 0 orelse FaultTolerance >= map_size(Latencies) ->
                    io:fwrite(standard_error, "bad \"f\" value ~b~n", [FaultTolerance]),
                    usage(),
                    halt(1);
                true ->
                    QuorumSize = FaultTolerance + 1,
                    MinCommitLat = compute_commit_latency(Region, Leader, Latencies, QuorumSize),
                    MinDeliveryLat = to_leader_latency(Region, Leader, Latencies),
                    MinUniformityLat = compute_minimal_quorum_rtt(Region, Latencies, QuorumSize),
                    io:format("Minimal strong commit latency: ~p ms~n", [MinCommitLat]),
                    io:format("Minimal delivery latency: ~p ms~n", [MinDeliveryLat * 2]),
                    io:format("Minimal barrier latency: ~p ms~n", [MinUniformityLat])
            end
    end.

-spec compute_commit_latency(atom(), atom(), latencies(), non_neg_integer()) -> non_neg_integer().
compute_commit_latency(Leader, Leader, _, 1) ->
    0;
compute_commit_latency(Leader, Leader, Latencies, N) ->
    compute_minimal_quorum_rtt(Leader, Latencies, N);
compute_commit_latency(Region, LeaderRegion, Latencies, N) when Region =/= LeaderRegion ->
    compute_follower_latency(Region, LeaderRegion, Latencies, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec compute_minimal_quorum_rtt(
    At :: atom(),
    Latencies :: latencies(),
    QuorumSize :: non_neg_integer()
) -> non_neg_integer().

compute_minimal_quorum_rtt(At, Latencies, N) ->
    compute_minimal_quorum_rtt(At, Latencies, N - 1, min_half_rtt(At, Latencies, 0)).

-spec compute_minimal_quorum_rtt(
    At :: atom(),
    Latencies :: latencies(),
    N :: non_neg_integer(),
    MinLinks :: {non_neg_integer(), non_neg_integer()}
) -> non_neg_integer().

compute_minimal_quorum_rtt(At, Latencies, N, {MinCount, MinSoFar}) ->
    case N of
        1 ->
            MinSoFar * 2;
        M when M =< MinCount ->
            %% There's less rounds remaining than paths with the same latency, exhaust with that latency
            MinSoFar * 2;
        _ ->
            %% We can skip MinCount rounds, they all have the same latency.
            %% We know that there's at least one round left.
            compute_minimal_quorum_rtt(
                At,
                Latencies,
                (N - MinCount),
                min_half_rtt(At, Latencies, MinSoFar)
            )
    end.

-spec compute_follower_latency(atom(), atom(), latencies(), non_neg_integer()) -> non_neg_integer().
compute_follower_latency(At, Leader, Latencies, N) ->
    G = config_to_digraph(Latencies),
    ToLeader = to_leader_latency(At, Leader, Latencies),
    compute_follower_latency(At, Leader, G, N - 1, ToLeader, shortest_latency(Leader, At, G, 0)).

-spec compute_follower_latency(
    At :: atom(),
    Leader :: atom(),
    LatGraph :: latencies_graph(),
    QuorumSize :: non_neg_integer(),
    ToLeaderLatency :: non_neg_integer(),
    MinLinks :: {non_neg_integer(), non_neg_integer()}
) -> non_neg_integer().

compute_follower_latency(At, Leader, G, N, ToLeader, {MinCount, MinSoFar}) ->
    case N of
        1 ->
            MinSoFar + ToLeader;
        M when M =< MinCount ->
            MinSoFar + ToLeader;
        _ ->
            compute_follower_latency(
                At,
                Leader,
                G,
                (N - MinCount),
                ToLeader,
                shortest_latency(Leader, At, G, MinSoFar)
            )
    end.

% @doc Get the latency of the min path from `At` to any other site, but bigger than `Min`.
%  If X paths have the same latency M, return {X, M}
-spec min_half_rtt(atom(), latencies(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
min_half_rtt(At, Latencies, Min) ->
    lists:foldl(
        fun
            ({_, RTT}, {Count, MinRTT}) when RTT =< MinRTT andalso RTT > Min ->
                {Count + 1, RTT};
            (_, Acc) ->
                Acc
        end,
        {0, undefined},
        maps:get(At, Latencies)
    ).

-spec to_leader_latency(atom(), atom(), latencies()) -> non_neg_integer().
to_leader_latency(Leader, Leader, _) ->
    0;
to_leader_latency(Region, Leader, Latencies) ->
    #{Region := Targets} = Latencies,
    {Leader, LatencyToLeader} = lists:keyfind(Leader, 1, Targets),
    LatencyToLeader.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% graph utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec config_to_digraph(latencies()) -> latencies_graph().
config_to_digraph(Latencies) ->
    G = digraph:new(),
    _ = [digraph:add_vertex(G, K) || K <- maps:keys(Latencies)],
    maps:fold(
        fun(K, Targets, Acc) ->
            [digraph:add_edge(G, K, T, L) || {T, L} <- Targets],
            Acc
        end,
        G,
        Latencies
    ).

-spec shortest_latency(
    From :: atom(),
    To :: atom(),
    Digraph :: latencies_graph(),
    Min :: non_neg_integer()
) -> {non_neg_integer(), non_neg_integer()}.

shortest_latency(From, To, Digraph, Min) ->
    shortest_latency(From, To, Digraph, #{From => []}, 0, Min).

-spec shortest_latency(
    From :: atom(),
    To :: atom(),
    Digraph :: latencies_graph(),
    Visited :: #{atom() => []},
    Cost :: non_neg_integer(),
    Min :: non_neg_integer()
) -> non_neg_integer().

shortest_latency(From, To, Digraph, Visited, Cost, Min) ->
    lists:foldl(
        fun(E, {Count, Acc}) ->
            {ChildCount, Total} =
                case digraph:edge(Digraph, E) of
                    {_, From, To, Lat} ->
                        {1, Cost + Lat};
                    {_, From, Neigh, Lat} when not is_map_key(Neigh, Visited) ->
                        shortest_latency(
                            Neigh,
                            To,
                            Digraph,
                            Visited#{Neigh => []},
                            Cost + Lat,
                            Min
                        );
                    _ ->
                        {0, Acc}
                end,
            if
                Total =< Acc andalso Total > Min ->
                    {Count + ChildCount, Total};
                true ->
                    {Count, Acc}
            end
        end,
        {0, undefined},
        digraph:out_edges(Digraph, From)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_config_key(term(), [tuple()]) -> {ok, term()} | no_return().
get_config_key(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            io:fwrite(standard_error, "config file doesn't contain key ~p~n", [Key]),
            usage(),
            halt(1);
        {Key, Value} ->
            {ok, Value}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_args([term()]) -> {ok, #{}} | error.
parse_args([]) ->
    error;
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

-spec parse_args([term()], #{}) -> {ok, #{}} | error.
parse_args([], Acc) ->
    {ok, Acc};
parse_args([[$- | Flag] | Args], Acc) ->
    case Flag of
        [$c] ->
            parse_flag(Args, fun(Arg) -> Acc#{config => Arg} end);
        [$r] ->
            parse_flag(Args, fun(Arg) -> Acc#{region => Arg} end);
        [$f] ->
            parse_flag(Args, fun(Arg) -> Acc#{f_factor => Arg} end);
        _ ->
            error
    end;
parse_args(_, _) ->
    error.

-spec parse_flag([term()], fun((term()) -> #{})) -> {ok, #{}} | error.
parse_flag(Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args(Rest, Fun(FlagArg));
        _ -> error
    end.

-spec required(#{}) -> {ok, #{}} | error.
required(Opts) ->
    Required = [config, region, f_factor],
    Valid = lists:all(fun(F) -> maps:is_key(F, Opts) end, Required),
    case Valid of
        false ->
            error;
        true ->
            {ok, Opts}
    end.
