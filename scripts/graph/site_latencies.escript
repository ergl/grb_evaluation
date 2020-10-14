#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enabled -name some_other_node@127.0.0.1 -setcookie grb_cookie

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/graph").

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(standard_error, "Usage: ~s -f <config-file> /path/to/results~n", [Name]).

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
            io:fwrite(standard_error, "Wrong option: reason ~s~n", [Reason]),
            usage(),
            halt(1);
        {ok, Opt=#{rest := ResultPath}} ->
            {ok, NumClients} = client_threads(ResultPath),
            {ok, Terms} = file:consult(config_file(ResultPath)),
            {clusters, ClusterMap} = lists:keyfind(clusters, 1, Terms),
            CheckServers = maps:get(server_reports, Opt, false),

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
            io:format("~s~n", [GlobalResults]),

            io:format("================================"),
            lists:foreach(
                fun
                    ({ClusterStr, Latencies}) ->
                        Formatted = string:join(string:replace(Latencies, "NA", NumClients, all), ""),
                        io:format("~n~s~n~s~n", [
                            string:to_upper(ClusterStr),
                            Formatted
                        ]);
                    ({ClusterStr, ServerReports, Latencies}) ->
                        Formatted = string:join(string:replace(Latencies, "NA", NumClients, all), ""),
                        io:format("~n~s~n~p~n~s~n", [
                            string:to_upper(ClusterStr),
                            ServerReports,
                            Formatted
                        ])
                end,
                Reports
            ),
            io:format("================================~n")
    end.

client_threads(Path) ->
    try
        [_, Match | _] = re:split(Path, "t[_=]"),
        {match, [{Start, Len}]} = re:run(Match, "[0-9]+"),
        {ok, binary_to_list(string:slice(Match, Start, Len))}
    catch
        _:_ ->
            {error, no_match}
    end.

config_file(Path) ->
    FindConfig = io_lib:format("find ~s -type f -name cluster.config -print", [Path]),
    Matches = nonl(os:cmd(FindConfig)),
    hd(string:split(Matches, "\n", all)).


parse_global_latencies(ResultPath) ->
    MergeAll = io_lib:format("~s ~s", [
        filename:join([?SELF_DIR, "merge.sh"]),
        ResultPath
    ]),
    _ = os:cmd(MergeAll),
    ReadResult = io_lib:format("~s -i ~s 2>/dev/null", [
        filename:join([?SELF_DIR, "read_data.r"]),
        ResultPath
    ]),

    nonl(os:cmd(ReadResult)).

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
    _ = MergeLatencies("readonly-red_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_start_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_read_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_commit_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_prepare_latencies.csv"),
    _ = MergeLatencies("readonly-red-track_accept_latencies.csv"),
    _ = MergeLatencies("writeonly-red_latencies.csv"),

    ReadResult =
        io_lib:format("~s -p -i ~s 2>/dev/null", [
            filename:join([?SELF_DIR, "read_data.r"]),
            TmpPath
        ]),

    nonl(os:cmd(ReadResult)).

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
