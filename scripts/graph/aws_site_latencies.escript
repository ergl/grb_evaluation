#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/graph").

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(
        standard_error,
        "Usage: ~s /path/to/results [-f <config-file>]~n",
        [Name]
    ).

main(Args) ->
    case parse_args(Args, []) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~p~n", [Reason]),
            usage(),
            halt(1);
        {ok, #{rest := ResultPath}} ->
            NumClients = client_threads(ResultPath),
            {ok, Terms} = file:consult(config_file(ResultPath)),
            {clusters, ClusterMap} = lists:keyfind(clusters, 1, Terms),

            Reports = pmap(
                fun(Cluster) ->
                    ClusterStr = atom_to_list(Cluster),
                    Latencies = parse_latencies(ResultPath, ClusterStr),
                    {ClusterStr, Latencies}
                end,
                maps:keys(ClusterMap)
            ),

            GlobalResults = parse_global_latencies(ResultPath),
            io:format("~s~n~n", [GlobalResults]),

            lists:foreach(
                fun({ClusterStr, Latencies}) ->
                    Formatted = string:join(
                        string:replace(Latencies, "NA", NumClients, all),
                        ""
                    ),
                    io:format("~s,~s~n", [string:to_upper(ClusterStr),Formatted])
                end,
                Reports
            ),
            io:format("================================~n")
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


parse_global_latencies(ResultPath) ->
    MergeAll = io_lib:format(
        "~s -a ~s",
        [filename:join([?SELF_DIR, "merge.sh"]), ResultPath]
    ),
    _ = os:cmd(MergeAll),
    ReadResult = io_lib:format("~s -i ~s 2>/dev/null", [
        filename:join([?SELF_DIR, "read_data.r"]),
        ResultPath
    ]),

    nonl(os:cmd(ReadResult)).

parse_latencies(ResultPath, ClusterStr) ->
    WithPrefix = io_lib:format("aws-~s", [ClusterStr]),
    %% Find all files with prefix ResultPath/ClusterStr*
    PathList = filelib:wildcard(unicode:characters_to_list(
        io_lib:format("~s-*", [filename:join(ResultPath, WithPrefix)])
    )),

    %% Given the file, return the path of that file in all paths in PathList
    %% Also return a string with all the results in a single line, so we can
    %% pass that to a unix utility (like mergeSummary.awk)
    Paths = fun(File) ->
        lists:foldl(
            fun(Folder, Acc) ->
                Joined = filename:join(Folder, File),
                case Acc of
                    {ignore, ignore} -> {Joined, Joined};
                    {Head, Other} -> {Head, Other ++ " " ++ Joined}
                end
            end,
            {ignore, ignore},
            PathList
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

    % Normal Latencies
    _ = MergeLatencies("readonly-blue_latencies.csv"),
    _ = MergeLatencies("writeonly-blue_latencies.csv"),
    _ = MergeLatencies("read-write-blue_latencies.csv"),
    _ = MergeLatencies("readonly-red_latencies.csv"),
    _ = MergeLatencies("writeonly-red_latencies.csv"),

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
