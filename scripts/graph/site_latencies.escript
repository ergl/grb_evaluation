#!/usr/bin/env escript

-mode(compile).

-export([main/1]).

-define(SELF_DIR, "/Users/ryan/dev/imdea/code/grb_evaluation/scripts/graph").

usage() ->
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(standard_error, "Usage: ~s -f <config-file> /path/to/results~n", [Name]).

main(Args) ->
    case parse_args(Args, []) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~s~n", [Reason]),
            usage(),
            halt(1);
        {ok, #{rest := ResultPath}} ->
            ConfigFile = config_file(ResultPath),
            {ok, Terms} = file:consult(ConfigFile),
            {clusters, ClusterMap} = lists:keyfind(clusters, 1, Terms),
            Results = pmap(fun({Cluster, #{clients := ClientNodes}}) ->
                Nodes = lists:map(fun erlang:atom_to_list/1, ClientNodes),
                parse_latencies(ResultPath, Cluster, Nodes)
            end, maps:to_list(ClusterMap)),
            lists:foreach(fun({ClusterStr, ClusterResults}) ->
                io:format("~n~s~n~s~n", [ClusterStr, ClusterResults])
            end, Results)
    end.

config_file(Path) ->
    FindConfig = io_lib:format("find ~s -type f -name cluster.config -print", [Path]),
    Matches = nonl(os:cmd(FindConfig)),
    hd(string:split(Matches, "\n", all)).

parse_latencies(ResultPath, Cluster, ClientNodes) ->
    ClusterStr = atom_to_list(Cluster),
    Paths = fun(File) ->
        lists:foldl(fun(N, Acc) ->
            Joined = filename:join([ResultPath, N, File]),
            case Acc of
                {ignore, ignore} -> {Joined, Joined};
                {Head, Other} ->
                    {Head, Other ++ " " ++ Joined}
            end
        end, {ignore, ignore}, ClientNodes)
    end,

    Mktemp = io_lib:format("mktemp -d -t ~s", [ClusterStr]),
    TmpPath = nonl(os:cmd(Mktemp)),

    MergeSummary =
        io_lib:format("~s ~s > ~s", [filename:join([?SELF_DIR, "mergeSummary.awk"]),
                                     element(2, Paths("summary.csv")),
                                     filename:join([TmpPath, "summary.csv"])]),

    _ = os:cmd(MergeSummary),

    MergeLatencies = fun(File) ->
        {TokenPath, TargetPaths} = Paths(File),
        Filename = filename:join([?SELF_DIR, "mergeLatencies.awk"]),
        Command = io_lib:format("~s ~s > ~s", [Filename,
                                               TargetPaths,
                                               filename:join([TmpPath, File])]),
        case filelib:is_file(TokenPath) of
            true -> os:cmd(Command);
            false -> ok
        end
    end,

    _ = MergeLatencies("readonly-red_latencies.csv"),
    _ = MergeLatencies("writeonly-red_latencies.csv"),

    ReadResult =
        io_lib:format("~s -i ~s 2>/dev/null", [filename:join([?SELF_DIR, "read_data.r"]),
                                               TmpPath]),

    ClusterResult = nonl(os:cmd(ReadResult)),
    {string:to_upper(ClusterStr), ClusterResult}.

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

parse_args([], _) -> {error, noargs};
parse_args(Args, Required) ->
    case parse_args_inner(Args, #{}) of
        {ok, Opts} -> required(Required, Opts);
        Err -> Err
    end.

parse_args_inner([], Acc) -> {ok, Acc};
parse_args_inner([ [$- | Flag] | Args], Acc) ->
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
