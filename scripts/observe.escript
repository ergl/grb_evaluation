#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -hidden -name health@127.0.0.1 -setcookie grb_cookie

-mode(compile).
-export([main/1]).

-spec usage() -> ok.
usage() ->
    Name = filename:basename(escript:script_name()),
    io:fwrite(standard_error, "Usage: ~s <erlang-node>~n", [Name]).

main([NodeString]) ->
    case inet:getaddr(NodeString, inet) of
        {error, Reason} ->
            io:fwrite(standard_error, "Bad node: ~p~n", [Reason]),
            usage(),
            halt(1);

        {ok, IPAddr} ->
            NodeName = list_to_atom("grb@" ++ inet:ntoa(IPAddr)),
            true = net_kernel:connect_node(NodeName),
            ok = observer:start(),
            process_flag(trap_exit, true),
            io:format("Press ^C to quit~n"),
            receive
                {'EXIT', _, _} -> ok
            end
    end;

main(_) ->
    usage(),
    halt(1).
