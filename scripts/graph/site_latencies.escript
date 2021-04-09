#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enabled -name some_other_node@127.0.0.1 -setcookie grb_cookie

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/graph").
-define(CONF, configuration).

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(
        standard_error,
        "Usage: ~s /path/to/results [-r] [-m | --measurements] [-f <config-file>] [-s --server-reports]~n",
        [Name]
    ).

ip_for_node('apollo-1-1.imdea') -> "10.10.5.31";
ip_for_node('apollo-1-2.imdea') -> "10.10.5.32";
ip_for_node('apollo-1-3.imdea') -> "10.10.5.33";
ip_for_node('apollo-1-4.imdea') -> "10.10.5.34";
ip_for_node('apollo-1-5.imdea') -> "10.10.5.35";
ip_for_node('apollo-1-6.imdea') -> "10.10.5.36";
ip_for_node('apollo-1-7.imdea') -> "10.10.5.37";
ip_for_node('apollo-1-8.imdea') -> "10.10.5.38";
ip_for_node('apollo-1-9.imdea') -> "10.10.5.39";
ip_for_node('apollo-1-10.imdea') -> "10.10.5.40";
ip_for_node('apollo-1-11.imdea') -> "10.10.5.41";
ip_for_node('apollo-1-12.imdea') -> "10.10.5.42";
ip_for_node('apollo-2-1.imdea') -> "10.10.5.61";
ip_for_node('apollo-2-2.imdea') -> "10.10.5.62";
ip_for_node('apollo-2-3.imdea') -> "10.10.5.63";
ip_for_node('apollo-2-4.imdea') -> "10.10.5.64";
ip_for_node('apollo-2-5.imdea') -> "10.10.5.65";
ip_for_node('apollo-2-6.imdea') -> "10.10.5.66";
ip_for_node('apollo-2-7.imdea') -> "10.10.5.67";
ip_for_node('apollo-2-8.imdea') -> "10.10.5.68";
ip_for_node('apollo-2-9.imdea') -> "10.10.5.69";
ip_for_node('apollo-2-10.imdea') -> "10.10.5.70";
ip_for_node('apollo-2-11.imdea') -> "10.10.5.71";
ip_for_node('apollo-2-12.imdea') -> "10.10.5.72".

main(Args) ->
    case parse_args(Args, []) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~p~n", [Reason]),
            usage(),
            halt(1);
        {ok, Opt=#{rest := ResultPath}} ->
            NumClients = client_threads(ResultPath),
            {ok, Terms} = file:consult(config_file(ResultPath)),
            {clusters, ClusterMap} = lists:keyfind(clusters, 1, Terms),
            CheckServers = maps:get(server_reports, Opt, false),

            IsRubis = maps:get(rubis, Opt, false),
            _ = ets:new(?CONF, [set, named_table]),
            true = ets:insert(?CONF, {rubis, IsRubis}),

            Reports = pmap(
                fun({Cluster, #{servers := ServerNodes, clients := ClientNodes}}) ->
                    ClusterStr = atom_to_list(Cluster),
                    Latencies = parse_latencies(ResultPath, ClusterStr, ClientNodes),
                    case CheckServers of
                        true ->
                            AppReports = server_reports(ResultPath, ClusterStr, ServerNodes),
                            {ClusterStr, AppReports, Latencies};
                        false ->
                            {ClusterStr, Latencies}
                    end
                end,
                maps:to_list(ClusterMap)
            ),

            GlobalResults = parse_global_latencies(ResultPath),
            io:format("~s~n~n", [GlobalResults]),

            lists:foreach(
                fun
                    ({ClusterStr, Latencies}) ->
                        Formatted = string:join(
                            string:replace(Latencies, "NA", NumClients, all),
                            ""
                        ),
                        io:format("~s,~s~n", [string:to_upper(ClusterStr),Formatted]);
                    ({ClusterStr, ServerReports, Latencies}) ->
                        Formatted = string:join(
                            string:replace(Latencies, "NA", NumClients, all),
                            ""
                        ),
                        io:format("~p~n~s,~s~n", [
                            ServerReports,
                            string:to_upper(ClusterStr),
                            Formatted
                        ])
                end,
                Reports
            ),

            %% Report on measurements / visibility. Requires results to be present locally.
            ok = maybe_print_measurements(ClusterMap, ResultPath, Opt),
            true = ets:delete(?CONF)
    end.

client_threads(Path) ->
    try
        {match, [ {Start0, Len0} ]} = re:run(Path, "t[_=][0-9]+"),
        Match = string:slice(Path, Start0, Len0),
        {match, [ {Start1, Len1} ]} = re:run(Match, "[0-9]+"),
        string:slice(Match, Start1, Len1)
    catch
        _:_ -> "NA"
    end.

config_file(Path) ->
    FindConfig = io_lib:format("find ~s -type f -name cluster.config -print", [Path]),
    Matches = nonl(os:cmd(FindConfig)),
    hd(string:split(Matches, "\n", all)).

maybe_print_measurements(ClusterMap, ResultPath, #{measurements := true}) ->
    VisibilityReports = pmap(
        fun(Cluster) ->
            ClusterStr = atom_to_list(Cluster),
            Rep = parse_measurements(ResultPath, ClusterStr),
            {ClusterStr, Rep}
        end,
        maps:keys(ClusterMap)
    ),
    FoldFun =
        fun
            ({StatName, Avg, Max}, Acc) ->
                io_lib:format(
                    "~w,~f,~p~n~s",
                    [StatName, Avg, Max, Acc]
                );
            ({StatName, Total}, Acc) ->
                io_lib:format(
                    "~w,~p~n~s",
                    [StatName, Total, Acc]
                )
        end,
    io:format("================================~n"),
    lists:foreach(
        fun({ClusterStr, Values}) ->
            Content = lists:foldl(FoldFun, "", Values),
            Header = "stat,avg,max",
            io:format("~s,~s~n~s~n", [string:to_upper(ClusterStr), Header, Content])
        end,
        VisibilityReports
    ),
    io:format("================================~n");
maybe_print_measurements(_, _, _) ->
    ok.

parse_global_latencies(ResultPath) ->
    MergeAll = io_lib:format("~s ~s", [
        filename:join([?SELF_DIR, "merge.sh"]),
        ResultPath
    ]),
    _ = os:cmd(MergeAll),
    ReadResult = io_lib:format("~s ~s -i ~s 2>/dev/null", [
        filename:join([?SELF_DIR, "read_data.r"]),
        is_rubis_flag(),
        ResultPath
    ]),

    nonl(os:cmd(ReadResult)).

is_rubis_flag() ->
    try
        true = ets:lookup_element(?CONF, rubis, 2),
        "-r"
    catch _:_ ->
        ""
    end.

server_reports(ResultPath, ClusterStr, ServerNodes) ->
    ReportFile = filename:join([ResultPath, io_lib:format("~s_server_reports.bin", [ClusterStr])]),
    case filelib:is_file(ReportFile) of
        true ->
            {ok, Bin} = file:read_file(ReportFile),
            binary_to_term(Bin);
        false ->
            AppNodes = [list_to_atom("grb@" ++ ip_for_node(N)) || N <- ServerNodes],
            Contents = lists:zip(
                AppNodes,
                [Res || {ok, Res} <- erpc:multicall(AppNodes, grb_measurements, report_stats, [])]
            ),
            file:write_file(ReportFile, term_to_binary(Contents)),
            Contents
    end.

parse_latencies(ResultPath, ClusterStr, BenchNodes) ->
    ClientNodes = lists:map(fun erlang:atom_to_list/1, BenchNodes),
    Paths = fun(File) ->
        lists:foldl(
            fun(N, Acc) ->
                Joined = filename:join([ResultPath, N, File]),
                case Acc of
                    {ignore, ignore} -> {Joined, Joined};
                    {Head, Other} -> {Head, Other ++ " " ++ Joined}
                end
            end,
            {ignore, ignore},
            ClientNodes
        )
    end,

    Mktemp = io_lib:format("mktemp -d -t ~s", [ClusterStr]),
    TmpPath = nonl(os:cmd(Mktemp)),

    MergeSummary =
        io_lib:format("~s ~s > ~s", [
            filename:join([?SELF_DIR, "mergeSummary.awk"]),
            element(2, Paths("summary.csv")),
            filename:join([TmpPath, "summary.csv"])
        ]),

    _ = os:cmd(MergeSummary),

    MergeLatencies = fun(File) ->
        {TokenPath, TargetPaths} = Paths(File),
        Filename = filename:join([?SELF_DIR, "mergeLatencies.awk"]),
        Command = io_lib:format("~s ~s > ~s", [
            Filename,
            TargetPaths,
            filename:join([TmpPath, File])
        ]),
        case filelib:is_file(TokenPath) of
            true -> os:cmd(Command);
            false -> ok
        end
    end,

    _ = MergeLatencies("readonly-blue_latencies.csv"),
    _ = MergeLatencies("writeonly-blue_latencies.csv"),
    _ = MergeLatencies("read-write-blue_latencies.csv"),
    _ = MergeLatencies("readonly-red_latencies.csv"),
    _ = MergeLatencies("writeonly-red_latencies.csv"),

    % Extra latencies (blue)
    _ = MergeLatencies("read-write-blue-track_latencies.csv"),
    _ = MergeLatencies("read-write-blue-track_start_latencies.csv"),
    _ = MergeLatencies("read-write-blue-track_read_latencies.csv"),
    _ = MergeLatencies("read-write-blue-track_update_latencies.csv"),
    _ = MergeLatencies("read-write-blue-track_commit_latencies.csv"),

    _ = MergeLatencies("readonly-blue-bypass_latencies.csv"),
    _ = MergeLatencies("readonly-red-bypass_latencies.csv"),
    _ = MergeLatencies("read-start-red_latencies.csv"),
    _ = MergeLatencies("writeonly-blue-bypass_latencies.csv"),

    % Extra latencies (red)
    _ = MergeLatencies("readonly-red-track_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_start_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_read_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_commit_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_prepare_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_accept_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_coordinator_commit_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_coordinator_commit_barrier_latencies.csv"),

    _ = MergeLatencies("writeonly-red-time_latencies.csv"),
    _ = MergeLatencies("writeonly-red-time_start_latencies.csv"),
    _ = MergeLatencies("writeonly-red-time_update_latencies.csv"),
    _ = MergeLatencies("writeonly-red-time_commit_latencies.csv"),

    % Rubis latencies
    _ = MergeLatencies("register-user_latencies.csv"),
    _ = MergeLatencies("browse-categories_latencies.csv"),
    _ = MergeLatencies("search-items-in-category_latencies.csv"),
    _ = MergeLatencies("browse-regions_latencies.csv"),
    _ = MergeLatencies("browse-categories-in-region_latencies.csv"),
    _ = MergeLatencies("search-items-in-region_latencies.csv"),
    _ = MergeLatencies("view-item_latencies.csv"),
    _ = MergeLatencies("view-user-info_latencies.csv"),
    _ = MergeLatencies("view-bid-history_latencies.csv"),
    _ = MergeLatencies("buy-now_latencies.csv"),
    _ = MergeLatencies("store-buy-now_latencies.csv"),
    _ = MergeLatencies("put-bid_latencies.csv"),
    _ = MergeLatencies("store-bid_latencies.csv"),
    _ = MergeLatencies("put-comment_latencies.csv"),
    _ = MergeLatencies("store-comment_latencies.csv"),
    _ = MergeLatencies("select-category-to-sell-item_latencies.csv"),
    _ = MergeLatencies("register-item_latencies.csv"),
    _ = MergeLatencies("about-me_latencies.csv"),
    _ = MergeLatencies("get-auctions-ready-for-close_latencies.csv"),
    _ = MergeLatencies("close-auction_latencies.csv"),

    ReadResult =
        io_lib:format("~s ~s -p -i ~s 2>/dev/null", [
            filename:join([?SELF_DIR, "read_data.r"]),
            is_rubis_flag(),
            TmpPath
        ]),

    nonl(os:cmd(ReadResult)).

parse_measurements(ResultPath, Cluster) ->
    Path =
        unicode:characters_to_list(io_lib:format("measurements-apollo-~s.bin", [Cluster])),
    case file:read_file(filename:join(ResultPath, Path)) of
        {error, _} ->
            [];
        {ok, Bin} ->
            FoldFun =
                fun(_Node, Values, Acc) ->
                    lists:foldl(
                        fun
                            %% Rolling max, plus accumulate ops for weighted mean later
                            ({stat, Name, #{ops := Ops, avg := Avg, max := Max}}, InnerAcc)
                                when Ops > 0 ->
                                    maps:update_with(
                                        {stat, Name},
                                        fun({Operations, Rollmax}) ->
                                            {[{Avg, Ops} | Operations], max(Rollmax, Max)}
                                        end,
                                        {[{Avg, Ops}], Max},
                                        InnerAcc
                                    );

                            ({counter, Name, Total}, InnerAcc) ->
                                maps:update_with(
                                    {counter, Name},
                                    fun(Old) -> Old + Total end,
                                    Total,
                                    InnerAcc
                                );

                            %% Ignore everything else for now
                            (_, InnerAcc) ->
                                InnerAcc
                        end,
                        Acc,
                        Values
                    )
                end,
            lists:reverse(lists:sort(maps:fold(
                fun
                    ({counter, Stat}, Total, Acc) ->
                        case Stat of
                            {grb_red_coordinator, _, _, _} ->
                                Acc;
                            _ ->
                                [ {{counter, Stat}, Total} | Acc ]
                        end;

                    ({stat, Stat}, {Ops, Max}, Acc) ->
                        %% Make weighted average
                        {Top, Bot} = lists:foldl(
                            fun({Avg, N}, {T, B}) ->
                                {(Avg * N) + T, B + N}
                            end,
                            {0, 0},
                            Ops
                        ),
                        case Stat of
                            {grb_red_coordinator, _} ->
                                [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            {grb_red_coordinator, _, _} ->
                                [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            {grb_red_coordinator, _, _, sent_to_ack} ->
                                [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            {grb_red_coordinator, _, _, ack_in_flight} ->
                                [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            {grb_paxos_vnode, _, Attr} ->
                                case
                                    lists:member(
                                        Attr,
                                        [message_queue_len,
                                         deliver_updates_called]
                                    )
                                of
                                    false ->
                                        [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                                    true ->
                                        [ {Stat, Top / Bot, Max} | Acc]
                                end;
                            {grb_dc_messages, _, _, Attr}
                                when Attr =/= message_queue_len ->
                                    [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            {grb_dc_connection_sender_socket, _, _, Attr} ->
                                case
                                    lists:member(
                                        Attr,
                                        [message_queue_len,
                                         pending_queue_len,
                                         pending_queue_bytes]
                                    )
                                of
                                    false ->
                                        [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                                    true ->
                                        [ {Stat, Top / Bot, Max} | Acc]
                                end;
                            _ ->
                                [ {Stat, Top / Bot, Max} | Acc]
                        end
                end,
                [],
                maps:fold(FoldFun, #{}, binary_to_term(Bin))
            )))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

nonl(S) -> string:trim(S, trailing, "$\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args([], _) ->
    {error, noargs};
parse_args(Args, Required) ->
    case parse_args_inner(Args, #{}) of
        {ok, Opts} -> required(Required, Opts);
        Err -> Err
    end.

parse_args_inner([], Acc) ->
    {ok, Acc};
parse_args_inner([[$- | Flag] | Args], Acc) ->
    case Flag of
        [$f] ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{config => Arg} end);
        "-file" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{config => Arg} end);
        [$s] ->
            parse_args_inner(Args, Acc#{server_reports => true});
        "-server_reports" ->
            parse_args_inner(Args, Acc#{server_reports => true});
        [$r] ->
            parse_args_inner(Args, Acc#{rubis => true});
        [$m] ->
            parse_args_inner(Args, Acc#{measurements => true});
        "-measurements" ->
            parse_args_inner(Args, Acc#{measurements => true});
        [$h] ->
            usage(),
            halt(0);
        _ ->
            {error, {badarg, Flag}}
    end;
parse_args_inner(Words, Acc) ->
    {ok, Acc#{rest => Words}}.

parse_flag(Flag, Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args_inner(Rest, Fun(FlagArg));
        _ -> {error, {noarg, Flag}}
    end.

required(Required, Opts) ->
    Valid = lists:all(fun(F) -> maps:is_key(F, Opts) end, Required),
    case Valid of
        true -> {ok, Opts};
        false -> {error, "Missing required fields"}
    end.
