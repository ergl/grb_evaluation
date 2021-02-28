#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/graph").
-define(CONF, configuration).

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(
        standard_error,
        "Usage: ~s [-ra] [--visibility] [-m | --measurements] /path/to/results [-f <config-file>]~n",
        [Name]
    ).

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

            IsRubis = maps:get(rubis, Opt, false),
            _ = ets:new(?CONF, [set, named_table]),
            true = ets:insert(?CONF, {rubis, IsRubis}),

            Reports = pmap(
                fun(Cluster) ->
                    ClusterStr = atom_to_list(Cluster),
                    {Latencies, Errors} = parse_latencies(ResultPath, ClusterStr),
                    {ClusterStr, Latencies, Errors}
                end,
                maps:keys(ClusterMap)
            ),

            {GlobalResults, GlobalErrors} = parse_global_latencies(ResultPath),
            io:format("~s~n~n", [GlobalResults]),

            lists:foreach(
                fun({ClusterStr, Latencies, _}) ->
                    Formatted = string:join(
                        string:replace(Latencies, "NA", NumClients, all),
                        ""
                    ),
                    io:format("~s,~s~n", [string:to_upper(ClusterStr),Formatted])
                end,
                Reports
            ),

            ok = maybe_print_abort_ratio(Reports, GlobalErrors, Opt),

            %% Report on measurements / visibility. Requires results to be present locally.
            ok = maybe_print_measurements(ClusterMap, ResultPath, Opt),
            ok = maybe_print_visibility(ClusterMap, ResultPath, Opt),

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

-spec maybe_print_measurements(_, _, _) -> ok.
maybe_print_measurements(ClusterMap, ResultPath, _Opt=#{measurements := true}) ->
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
                    "~w,~p,~p~n~s",
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

-spec maybe_print_visibility(_, _, _) -> ok.
maybe_print_visibility(ClusterMap, ResultPath, _Opt=#{visibility := true}) ->
    VisibilityReports = pmap(
        fun(Cluster) ->
            ClusterStr = atom_to_list(Cluster),
            Rep = parse_visibility(ResultPath, ClusterStr),
            {ClusterStr, Rep}
        end,
        maps:keys(ClusterMap)
    ),

    % FoldFun =
    %     fun({Remote, Min, Max, Avg, Med}, Acc) ->
    %         io_lib:format(
    %             "~p,~b,~b,~b,~b~n~s",
    %             [element(1, Remote)
    %              , Min div 1000
    %              , Max div 1000
    %              , Avg div 1000
    %              , Med div 1000
    %              , Acc]
    %         )
    % end,

    FoldFun =
        fun({Remote, RemValues}, Acc) ->
            io_lib:format(
                "~p,~s~n~s",
                [element(1, Remote),RemValues,Acc]
            )
        end,

    io:format("================================~n"),

    lists:foreach(
        fun({ClusterStr, Values}) ->
            Content = lists:foldl(FoldFun, "", Values),
            Header = "remote,min,max,avg,med",
            io:format("~s~n~s~n~s~n", [string:to_upper(ClusterStr), Header, Content])
        end,
        VisibilityReports
    ),

    io:format("================================~n");

maybe_print_visibility(_, _, _) ->
    ok.

-spec maybe_print_abort_ratio(_, _, _) -> ok.
maybe_print_abort_ratio(Reports, GlobalErrors, _Opt=#{abort_ratio := true}) ->
    io:format("================================~n"),
    io:format("~s~n~s~n", ["OVERALL", GlobalErrors]),
    lists:foreach(
        fun({ClusterStr, _, Errors}) ->
            io:format("~n~s~n~s~n",
                        [string:to_upper(ClusterStr), Errors])
        end,
        Reports
    ),
    io:format("================================~n");

maybe_print_abort_ratio(_, _, _) ->
    ok.

parse_global_latencies(ResultPath) ->
    MergeAll = io_lib:format(
        "~s -a ~s",
        [filename:join([?SELF_DIR, "merge.sh"]), ResultPath]
    ),
    _ = os:cmd(MergeAll),
    ReadResult = io_lib:format("~s ~s -i ~s 2>/dev/null", [
        filename:join([?SELF_DIR, "read_data.r"]),
        is_rubis_flag(),
        ResultPath
    ]),

    ErrorResult =
        io_lib:format("~s -i ~s 2>/dev/null", [
            filename:join([?SELF_DIR, "report_errors.r"]),
            ResultPath
        ]),

    {nonl(os:cmd(ReadResult)), nonl(os:cmd(ErrorResult))}.

parse_visibility(ResultPath, Region) ->
    Path = unicode:characters_to_list(io_lib:format("visibility-aws-~s.bin", [Region])),
    case file:read_file(filename:join(ResultPath, Path)) of
        {error, _} ->
            [];
        {ok, Bin} ->
            % FoldFun =
            %     fun(Remote, Values, Acc) ->
            %         [Min | _] = Sort = lists:sort(Values),
            %         Length = length(Sort),
            %         Max = lists:max(Sort),
            %         Med = lists:nth((length(Sort) div 2), Sort),
            %         Avg = lists:sum(Sort) div Length,
            %         [ {Remote, Min, Max, Avg, Med} | Acc ]
            %     end,

            FoldFun =
                fun(Remote, Values, Acc) ->
                    ValueStr = string:join([integer_to_list(X) || X <- Values], ","),
                    [ {Remote, ValueStr} | Acc ]
                end,

            maps:fold(FoldFun, [], binary_to_term(Bin))
    end.

parse_measurements(ResultPath, Region) ->
    Path = unicode:characters_to_list(io_lib:format("measurements-aws-~s.bin", [Region])),
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
                        [ {{counter, Stat}, Total} | Acc ];

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
                            {grb_paxos_vnode, _, Attr} when Attr =/= message_queue_len ->
                                [ {Stat, (Top / Bot) / 1000, Max / 1000} | Acc];
                            _ ->
                                [ {Stat, Top / Bot, Max} | Acc]
                        end
                end,
                [],
                maps:fold(FoldFun, #{}, binary_to_term(Bin))
            )))
    end.

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

    MergeErrors =
        io_lib:format("~s ~s > ~s", [
            filename:join([?SELF_DIR, "mergeErrors.awk"]),
            element(2, Paths("errors.csv")),
            filename:join([TmpPath, "errors.csv"])
        ]),

    _ = os:cmd(MergeErrors),

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
        io_lib:format("~s ~s -p -i ~s 2>/dev/null", [
            filename:join([?SELF_DIR, "read_data.r"]),
            is_rubis_flag(),
            TmpPath
        ]),

    ErrorResult =
        io_lib:format("~s -i ~s 2>/dev/null", [
            filename:join([?SELF_DIR, "report_errors.r"]),
            TmpPath
        ]),

    {nonl(os:cmd(ReadResult)), nonl(os:cmd(ErrorResult))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_rubis_flag() ->
    try
        true = ets:lookup_element(?CONF, rubis, 2),
        "-r"
    catch _:_ ->
        ""
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
        [$a] ->
            parse_args_inner(Args, Acc#{abort_ratio => true});
        [$r] ->
            parse_args_inner(Args, Acc#{rubis => true});
        "-visibility" ->
            parse_args_inner(Args, Acc#{visibility => true});
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
