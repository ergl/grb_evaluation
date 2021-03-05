#!/usr/bin/env escript
%%! -pa ../_build/default/lib/pvc_proto/ebin -Wall

%% A grep-like utility to search through pcap files (https://wiki.wireshark.org/Development/LibpcapFileFormat, used in
%% tcpdump and other libpcap-dependent utilities.
%%
%% The pcap file will contain a set of binary packets as captured by tcpdump. It contains bare ethernet, IP, and TCP
%% packets.
%%
%% Since our program uses a mix of protocol buffers and home-grown encoding, searching through the packets can be hard.
%% This tool can parse application-level messages and match on them, as well.

-mode(compile).

-define(REST_TABLE, rest_data_table).
-define(DEFAULT_PACKET_HEADER, 4).

-define(MSG_KINDS, #{
    0 => "BLUE_HB_KIND",
    1 => "REPL_TX_KIND",
    2 => "UPDATE_CLOCK_KIND",
    3 => "UPDATE_CLOCK_HEARTBEAT_KIND",
    4 => "RED_PREPARE_KIND",
    5 => "RED_ACCEPT_KIND",
    6 => "RED_ACCEPT_ACK_KIND",
    7 => "RED_DECIDE_KIND",
    8 => "RED_ALREADY_DECIDED_KIND",
    9 => "RED_HB_KIND",
    10 => "RED_HB_ACK_KIND",
    11 => "RED_DELIVER_KIND",
    12 => "FWD_BLUE_HB_KIND",
    13 => "FWD_BLUE_TX_KIND",
    14 => "REPL_TX_4_KIND",
    15 => "REPL_TX_8_KIND",
    16 => "DC_PING",
    17 => "DC_CREATE",
    18 => "DC_GET_DESCRIPTOR",
    19 => "DC_CONNECT_TO_DESCR",
    20 => "DC_START_BLUE_PROCESSES",
    21 => "DC_START_RED_FOLLOWER",
    22 => "UPDATE_CLOCK_CURE_KIND",
    23 => "UPDATE_CLOCK_CURE_HEARTBEAT_KIND",
    24 => "RED_LEARN_ABORT_KIND"
}).

-record(pcap_header, {
    % Endianess of header fields
    endianess :: big | little,
    % Does the file have nanosecond-level precision
    nano :: boolean(),
    % major-minor version
    major :: non_neg_integer(),
    minor :: non_neg_integer(),
    % time correction in seconds since UTC. In practice all timestamps are in UTC
    gmt_correction :: non_neg_integer(),
    % Accuracy of timestamps. In practice all tools set this to zero.
    ts_accuracy :: non_neg_integer(),
    % Snapshot length: maximum package len captured.
    snap_len :: non_neg_integer(),
    % Type of the captured traffic, we're only interested in ethernet
    network :: ethernet
}).

-type header_encoding() :: {big, boolean()} | {little, boolean()}.

-type opts() :: #{
    file := string(),
    verbose => boolean(),
    print_data => boolean(),
    data_errors => boolean(),
    count => boolean(),

    src_ip => inet:ip4_address(),
    dst_ip => inet:ip4_address(),

    src_port => non_neg_integer(),
    dst_port => non_neg_integer(),

    msg_type => non_neg_integer(),

    %% How many matches to print
    match_limit => non_neg_integer(),

    %% Print only those that differ by this ms from capture time
    capture_diff_threshold_min => non_neg_integer(),
    capture_diff_threshold_max => non_neg_integer()
}.

-type packet_info() :: #{
    %% was the packet trimmed on capture?
    packet_trimmed := boolean(),
    %% actual UNIX timestamp (secs, microsecs offset)
    packet_timestamp := {non_neg_integer(), non_neg_integer()},
    %% timestamp as offset from the beginning of capture
    packet_timestamp_relative_secs := non_neg_integer(),

    %% taken from IP
    src_ip := inet:ip4_address(),
    dst_ip := inet:ip4_address(),

    %% taken from TCP
    tcp_flow_id => flow_id(),
    src_port := non_neg_integer(),
    dst_port := non_neg_integer(),

    %% Sequence number (relative w/ respect to capture init) and
    %% ACK number (rel w/ respect to capture init)
    seq_rel := non_neg_integer(),
    ack_rel := non_neg_integer(),

    %% window size of the TCP packet
    window_size := non_neg_integer(),

    %% Extra
    payload => {error, retransmission}
             | {error, incomplete}
             | {error, term()}
             | [packet()]
}.

-type packet() :: #{
    version := non_neg_integer(),
    %% If metrics, this contains the timestamp at send time
    sent_ts => non_neg_integer(),
    %% Kind of message
    msg_type := non_neg_integer(),
    %% Payload, if any
    data := term()
}.

-type flow_id() :: {inet:ip4_address(), non_neg_integer(), inet:ip4_address(), non_neg_integer()}.

-export([main/1,
         bin_to_hexstr/1]).

-spec format_msg_types() -> string().
format_msg_types() ->
    Types = lists:sort(maps:to_list(?MSG_KINDS)),
    lists:foldl(
        fun({N, Type}, Acc) ->
            io_lib:format("~s~n\t~b:\t~s", [Acc, N, Type])
        end,
        "",
        Types
    ).

usage() ->
    Fmt =
        begin
            "NAME~n" ++
            "\t~s - search in pcap files containing application messages~n" ++
            "~nUSAGE~n" ++
            "\t~s -f trace.pcap [ -vcde ]~n" ++
            "\t\t\t    [ -t --type MSG_TYPE ]~n" ++
            "\t\t\t    [ --src-ip IP ] [ --dst-ip IP ]~n" ++
            "\t\t\t    [ --src-port PORT ] [ --dst-port PORT ]~n" ++
            "\t\t\t    [ --threshold_min THRESHOLD ] [ --threshold_max THRESHOLD ]~n" ++
            "\t\t\t    [ --limit LIMIT ]~n" ++
            "~nFLAGS~n" ++
            "\t-v: verbose~n" ++
            "\t-c: count, don't print~n" ++
            "\t-d: show matching payload~n" ++
            "\t-e: show errors~n" ++
            "~nAVAILABLE MSG_TYPES~n" ++
            "~s~n~n"
        end,
    Name = filename:basename(escript:script_name()),
    ok = io:fwrite(standard_error, Fmt, [Name, Name, format_msg_types()]).

main(Args) ->
    case parse_args(Args) of
        {error, Reason} ->
            io:fwrite(standard_error, "Wrong option: reason ~p~n", [Reason]),
            usage(),
            halt(1);

        {ok, Opts=#{file := FilePath}} ->
            %% This table will hold any extra data carried over from previous packets
            %% The key is the flow id of the TCP connection
            ?REST_TABLE = ets:new(?REST_TABLE, [set, named_table]),
            {ok, Contents} = file:read_file(FilePath),
            case parse_header(Contents) of
                {ok, Header, Packets} ->
                    io:format("~s~n", [fmt_header(Header)]),
                    filter_pcap_packets(Opts, Packets, 0);
                {error, Reason} ->
                    io:fwrite(standard_error, "Wrong header: reason ~p~n", [Reason]),
                    halt(1)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Output functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec print_packet(packet_info(), boolean(), boolean()) -> non_neg_integer().
print_packet(PacketInfo, Verbose, ShowData) ->
    #{
        tcp_flow_id := {SrcIp, SrcPort, DstIp, DstPort},
        payload := Msgs
    } = PacketInfo,
    CaptureTSMicros = packet_capture_time_micros(PacketInfo),
    case Msgs of
        {error, Reason} ->
            io:format(
                "~b\t~s:~p => ~s:~p \tN/A \t~w~n",
                [
                    CaptureTSMicros,
                    inet:ntoa(SrcIp),
                    SrcPort,
                    inet:ntoa(DstIp),
                    DstPort,
                    {error, Reason}
                ]
            ),
            Verbose andalso print_extra(PacketInfo, ShowData, {error, Reason}),
            1;
        _ ->
            PrintFun = fun(Msg) ->
                #{msg_type := Type, data := Data} = Msg,
                PacketTsMicros = maps:get(sent_ts, Msg, CaptureTSMicros),
                CaptureDiff = capture_diff(
                    CaptureTSMicros,
                    PacketTsMicros
                ),
                io:format(
                    "~b ~b (~f) ~s:~p => ~s:~p \ttype=~s~n",
                    [CaptureTSMicros,
                     PacketTsMicros,
                     CaptureDiff,
                     inet:ntoa(SrcIp),
                     SrcPort,
                     inet:ntoa(DstIp),
                     DstPort,
                     translate_type(Type)]
                ),
                Verbose andalso print_extra(PacketInfo, ShowData, Msg),
                ShowData andalso io:format("~n~w~n~n", [Data])
            end,
            lists:foreach(
                PrintFun,
                Msgs
            ),
            length(Msgs)
    end.

-spec print_extra(packet_info(), boolean(), packet()) -> ok.
print_extra(PacketInfo, ShowData, Msg) ->
    #{
        seq_rel := Seq,
        ack_rel := Ack,
        window_size := WindowSize
    } = PacketInfo,
    CaptureTSMicros = packet_capture_time_micros(PacketInfo),
    CaptureDate = calendar:system_time_to_rfc3339(CaptureTSMicros, [{unit, microsecond}]),
    SentDate =
        if
            is_map(Msg) ->
                calendar:system_time_to_rfc3339(
                    maps:get(sent_ts, Msg, CaptureTSMicros),
                    [{unit, microsecond}]
                );
            true ->
                "N/A"
        end,
    io:format(
        "Capture Date := ~s | SentDate := ~s | Seq := ~b | ACK := ~b | Rcv Window := ~p bytes~n",
        [CaptureDate, SentDate, Seq, Ack, WindowSize]
    ),
    Trimmed = maps:get(packet_trimmed, PacketInfo, false),
    Trimmed andalso io:format("(message trimmed)~n"),
    FlowId = maps:get(tcp_flow_id, PacketInfo, ignore),
    FlowId =/= ignore andalso ShowData andalso begin
        Bytes = get_extra_bytes(FlowId),
        Bytes =/= <<>> andalso io:format("Extra bytes := ~p~n", [Bytes])
    end,
    io:format("~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PCAP Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_header(binary()) -> {ok, Header :: #pcap_header{}, Packets :: binary()} | {error, term()}.
parse_header(Contents) ->
    <<Magic:32, Rest/binary>> = Contents,
    case parse_ordering(Magic) of
        {ok, Order} ->
            parse_header(Order, Rest);
        Err ->
            Err
    end.

-spec parse_ordering(integer()) -> {ok, header_encoding()} | {error, term()}.
parse_ordering(16#A1B2C3D4) -> {ok, {big, false}};
parse_ordering(16#D4C3B2A1) -> {ok, {little, false}};
parse_ordering(16#A1B23C4D) -> {ok, {big, true}};
parse_ordering(16#4D2CB2A1) -> {ok, {little, true}};
parse_ordering(_) -> {error, bad_magic}.

-spec parse_header(header_encoding(), binary()) -> {ok, #pcap_header{}, binary()} | {error, term()}.
parse_header({big, Nano}, Binary) ->
    <<VsnMajor:16/big-unsigned, VsnMinor:16/big-unsigned,
      ThisZone:32/big-signed, SigFigs:32/big-unsigned,
      SnapLen:32/big-unsigned, Network:32/big-unsigned, Packets/binary>> = Binary,
    {
        ok,
        #pcap_header{endianess=big, nano=Nano,
                     major=VsnMajor, minor=VsnMinor,
                     gmt_correction=ThisZone, ts_accuracy=SigFigs,
                     snap_len=SnapLen, network=parse_network(Network)},
        Packets
    };

parse_header({little, Nano}, Binary) ->
    <<VsnMajor:16/little-unsigned, VsnMinor:16/little-unsigned,
      ThisZone:32/little-signed, SigFigs:32/little-unsigned,
      SnapLen:32/little-unsigned, Network:32/little-unsigned, Packets/binary>> = Binary,
    {
        ok,
        #pcap_header{endianess=little, nano=Nano,
                     major=VsnMajor, minor=VsnMinor,
                     gmt_correction=ThisZone, ts_accuracy=SigFigs,
                     snap_len=SnapLen, network=parse_network(Network)},
        Packets
    }.

-spec fmt_header(#pcap_header{}) -> string().
fmt_header(#pcap_header{nano=Nano, major=Major, minor=Minor, snap_len=Len}) ->
    unicode:characters_to_list(
        io_lib:format(
            "PCAP ~b.~b | nano support ~p | max_packet_len ~b bytes",
            [Major, Minor, Nano, Len]
        )
    ).

parse_network(1) -> ethernet;
parse_network(_) -> {error, unknown_network}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Packet Parsing (Ethernet, IP, TCP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec filter_pcap_packets(opts(), binary(), non_neg_integer()) -> ok.
filter_pcap_packets(Opts, <<>>, N) ->
    case Opts of
        #{count := true} ->
            io:format("~b~n", [N]);
        _ ->
        ok
    end;
filter_pcap_packets(Opts, Bin, N) ->
    {Packet, Rest} = parse_packet(Opts, Bin),
    case Packet of
        skip ->
            filter_pcap_packets(Opts, Rest, N);
        _ ->
            ShouldPrint = not (maps:get(count, Opts, false)),
            Limit = maps:get(match_limit, Opts, infinity),
            if
                N >= Limit ->
                    if
                        not ShouldPrint ->
                            io:format("~b~n", [N]);
                        true ->
                            ok
                    end;
                true ->
                    Printed = print_packet(
                        Packet,
                        maps:get(verbose, Opts, false),
                        maps:get(print_data, Opts, false)
                    ),
                    filter_pcap_packets(Opts, Rest, N + Printed)
            end
    end.

-spec parse_packet(opts(), binary()) -> {skip, binary()} | {packet_info(), binary()}.
parse_packet(Opts, Bin) ->
    <<
        % unix timestamp in seconds / microseconds (or nanos is nano is supported)
        StartSec:32/little-unsigned, MicroOfft:32/little-unsigned,
        % ByteLen length saved, OrigLen: original packet size
        ByteLen:32/little-unsigned, OrigLen:32/little-unsigned,
        Packet:ByteLen/binary,
        Rest/binary
    >> = Bin,
    PacketInfo = #{
        packet_trimmed => (OrigLen > ByteLen),
        packet_timestamp => {StartSec, MicroOfft},
        packet_timestamp_relative_secs => start_offset(StartSec)
    },
    ParsedPacket = parse_packet_data(
        Packet,
        Opts,
        PacketInfo
    ),
    {ParsedPacket, Rest}.

-spec parse_packet_data(binary(), opts(), packet_info()) -> packet_info() | skip.
parse_packet_data(Packet, Opts, PacketInfo) ->
    %% Packets are always TCP, so have Ethernet and IP headers
    <<
        %% Skip Ethernet frame (14 bytes => 112 octets) and IPv4 metadata (96 octets)
        %% Match on header length so we can skip options later.
        _:112, 4:4, IHL:4, _:88,
        RawSrcIp:32/bits, RawDstIp:32/bits,
        Rest/binary
    >> = Packet,
    SrcIp = to_addr(RawSrcIp),
    DstIp = to_addr(RawDstIp),
    case match_ip_level(Opts, SrcIp, DstIp) of
        false ->
            skip;
        true ->
            TCP_Packet = skip_options(IHL, Rest),
            parse_tcp_level(TCP_Packet, Opts, PacketInfo#{src_ip => SrcIp, dst_ip => DstIp})
    end.

% Skip IP-level options if IHL says so.
-spec skip_options(pos_integer(), binary()) -> binary().
skip_options(IHL, Bin) when IHL > 5 ->
    %% IP has extra options, skip them until we get to the data
    %% IHL * 32 => total IP header size, and we remove the 160 we've already parsed.
    <<_:((IHL bsl 5) - 160), Data/binary>> = Bin,
    Data;
skip_options(5, Bin) ->
    Bin.

-spec parse_tcp_level(binary(), opts(), packet_info()) -> skip | packet_info().
parse_tcp_level(Packet, Opts, PacketInfo=#{src_ip := SrcIp, dst_ip := DstIp}) ->
    <<
        SrcPort:16, DstPort:16,
        Seq:32,
        ACK:32,
        OffsetWords:4, _:3, Flags:9/bits, Window:16,
        _/bits
    >> = Packet,
    case match_tcp_level(Opts, SrcPort, DstPort) of
        false ->
            skip;
        true ->
            %% same as with IP, skip to the data.
            <<_:(OffsetWords bsl 5), RawData/binary>> = Packet,
            ConnId = make_tcp_flow(SrcIp, SrcPort, DstIp, DstPort),
            ACKSet = is_ack_set(Flags),
            NewPacketInfo = PacketInfo#{
                tcp_flow_id => ConnId,
                src_port => SrcPort,
                dst_port => DstPort,
                seq_rel => rel_seq(Seq, ConnId),
                ack_rel => rel_ack(ACKSet, ACK, ConnId),
                window_size => Window
            },
            parse_app_level(RawData, Opts, NewPacketInfo)
    end.

-spec parse_app_level(binary(), opts(), packet_info()) -> skip | packet_info().
parse_app_level(<<>>, _, _) ->
    skip;
parse_app_level(Packet, Opts, PacketInfo=#{tcp_flow_id := FlowId}) ->
    %% In case of fragmentation, we might have pending data from a previous packet
    ExtraBytes = get_extra_bytes(FlowId),
    NewPacketPayload = <<ExtraBytes/binary, Packet/binary>>,
    case parse_data(PacketInfo, NewPacketPayload) of
        {ok, Data, Rest} ->
            ok = set_extra_bytes(FlowId, Rest),
            case match_app_level(Opts, PacketInfo, Data) of
                [] ->
                    skip;
                FilterData ->
                    PacketInfo#{payload => FilterData}
            end;

        %% We got some bad data. It might be that this packet is a retransmission, in which case we
        %% can ignore the content (we have processed it before).
        %% TODO(borja): Have we?
        {error, {bad_data, _BadData}}=Err ->
            ok = clear_extra_bytes(FlowId),
            case reuse_ack(PacketInfo) of
                true ->
                    skip_on_error(Opts, PacketInfo, {error, retransmission});
                false ->
                    %% Give up, can't decide
                    Err
            end;

        %% If the entire packet doesn't contain enough data, stash it for later
        incomplete_data ->
            ok = set_extra_bytes(FlowId, NewPacketPayload),
            skip_on_error(Opts, PacketInfo, {error, incomplete});

        {error, Reason} ->
            ok = clear_extra_bytes(FlowId),
            skip_on_error(Opts, PacketInfo, {error, Reason})
    end.

match_ip_level(Opts, SrcIp, DstIp) ->
    MatchSrc = SrcIp =:= maps:get(src_ip, Opts, SrcIp),
    MatchDst = DstIp =:= maps:get(dst_ip, Opts, DstIp),
    MatchSrc andalso MatchDst.

match_tcp_level(Opts, SrcPort, DstPort) ->
    MatchSrc = SrcPort =:= maps:get(src_port, Opts, SrcPort),
    MatchDst = DstPort =:= maps:get(dst_port, Opts, DstPort),
    MatchSrc andalso MatchDst.

match_app_level(Opts, PacketInfo, Msgs) ->
    CaptureTS = packet_capture_time_micros(PacketInfo),
    CaptureDiffThMin = maps:get(capture_diff_threshold_min, Opts, 0),
    CaptureDiffThMax = maps:get(capture_diff_threshold_max, Opts, infinity),
    lists:filtermap(fun(Msg=#{msg_type := MsgType}) ->
        MatchedType = maps:get(msg_type, Opts, MsgType),
        CaptureDiff = capture_diff(CaptureTS, maps:get(sent_ts, Msg, CaptureTS)),
        if
            (MsgType =:= MatchedType) andalso
            (CaptureDiff >= CaptureDiffThMin) andalso
            (CaptureDiff =< CaptureDiffThMax) ->
                {true, Msg};
            true ->
                false
        end
    end, Msgs).

packet_capture_time_micros(#{packet_timestamp := {Secs, MicrosOffset}}) ->
    (Secs * 1_000_000) + MicrosOffset.

capture_diff(PacketCapture, PacketTs) ->
    (PacketCapture - PacketTs) / 1000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Protocol Parsing (Ethernet, IP, TCP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec valid_data([binary()]) -> boolean().
valid_data(Msgs) ->
    not lists:any(fun(<<>>) -> true; (_) -> false end, Msgs).

-spec decode_msg(packet_info(), binary()) -> packet().
decode_msg(#{packet_trimmed := Trimmed}, Msg) ->
    <<
        VSN:8,
        SentTimestamp:8/unit:8-integer-big-unsigned,
        Type:8,
        Payload/binary
    >> = Msg,
    Decoded =
        if
            Trimmed ->
                %% Be careful, we might be cut off
                try
                    binary_to_term(Payload)
                catch _:_ ->
                    {error, incomplete}
                end;
            true ->
                binary_to_term(Payload)
        end,
    #{
        version => VSN,
        sent_ts => SentTimestamp,
        msg_type => Type,
        data => Decoded
    }.

-spec parse_data(Info :: packet_info(),
                 Data :: binary()) -> {ok, packet(), binary()}
                                    | {error, {bad_data, binary()}}
                                    | incomplete_data
                                    | {error, term()}.

parse_data(Info, Data) ->
    case decode_data(Data) of
        {ok, Msgs, Rest} ->
            case valid_data(Msgs) of
                false ->
                    {error, {bad_data, Data}};
                true ->
                    MsgInfo = [ decode_msg(Info, M) || M <- Msgs ],
                    {ok, MsgInfo, Rest}
            end;

        Other ->
            Other
    end.

%% Decode client buffer
-spec decode_data(Data :: binary()) -> {ok, [binary()], binary()}
                                     | {error, atom()}
                                     | incomplete_data.
decode_data(Data) ->
    case erlang:decode_packet(?DEFAULT_PACKET_HEADER, Data, []) of
        {more, _} ->
            incomplete_data;

        {error, Reason} ->
            {error, Reason};

        {ok, Message, More} ->
            decode_data_inner(More, [Message])
    end.

decode_data_inner(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};

decode_data_inner(Data, Acc) ->
    case erlang:decode_packet(?DEFAULT_PACKET_HEADER, Data, []) of
        {ok, Message, More} ->
            decode_data_inner(More, [Message | Acc]);
        _ ->
            {ok, lists:reverse(Acc), Data}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getopt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec parse_args([term()]) -> {ok, opts()} | {error, term()}.
parse_args([]) ->
    {error, noargs};
parse_args(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} -> required(Opts);
        Err -> Err
    end.

-spec parse_args([term()], opts()) -> {ok, opts()} | {error, term()}.
parse_args([], Acc) -> {ok, Acc};
parse_args([ [$- | Flag] | Args], Acc) ->
    case Flag of
        [$v] ->
            parse_args(Args, Acc#{verbose => true});
        [$c] ->
            parse_args(Args, Acc#{count => true});
        [$e] ->
            parse_args(Args, Acc#{data_errors => true});
        [$d] ->
            parse_args(Args, Acc#{print_data => true});
        [$f] ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{file => Arg} end);
        [$t] ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{msg_type => list_to_integer(Arg, 10)} end);
        "-type" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{msg_type => list_to_integer(Arg, 10)} end);
        "-src-ip" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{src_ip => element(2, inet:parse_address(Arg))} end);
        "-dst-ip" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{dst_ip => element(2, inet:parse_address(Arg))} end);
        "-src-port" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{src_port => list_to_integer(Arg)} end);
        "-dst-port" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{dst_port => list_to_integer(Arg)} end);
        "-limit" ->
            parse_flag(Flag, Args, fun(Arg) -> Acc#{match_limit => list_to_integer(Arg)} end);
        "-threshold_min" ->
            parse_flag(
                Flag,
                Args,
                fun(Arg) ->
                    Conv = case catch list_to_integer(Arg) of
                        N when is_integer(N) -> N;
                        {'EXIT', _} ->
                            list_to_float(Arg)
                    end,
                    Acc#{capture_diff_threshold_min => Conv}
                end
            );
        "-threshold_max" ->
            parse_flag(
                Flag,
                Args,
                fun(Arg) ->
                    Conv = case catch list_to_integer(Arg) of
                        N when is_integer(N) -> N;
                        {'EXIT', _} ->
                            list_to_float(Arg)
                    end,
                    Acc#{capture_diff_threshold_max => Conv}
                end
            );
        [$h] ->
            usage(),
            halt(0);
        _ ->
            {error, {badarg, Flag}}
    end;

parse_args(_, _) ->
    {error, noargs}.

parse_flag(Flag, Args, Fun) ->
    case Args of
        [FlagArg | Rest] -> parse_args(Rest, Fun(FlagArg));
        _ -> {error, {noarg, Flag}}
    end.

required(Opts) ->
    Required = [file],
    Valid = lists:all(fun(F) -> maps:is_key(F, Opts) end, Required),
    case Valid of
        true -> {ok, Opts};
        false -> {error, "Missing required fields"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Get the packet start second as offset from the first packet time
-spec start_offset(non_neg_integer()) -> non_neg_integer().
start_offset(TimeS) ->
    case erlang:get(first_start) of
        undefined ->
            erlang:put(first_start, TimeS),
            0;
        StartS ->
            TimeS - StartS
    end.

%% @doc Get the relative sequence number for this TCP flow
-spec rel_seq(non_neg_integer(), flow_id()) -> non_neg_integer().
rel_seq(Seq, TCPConn) ->
    case erlang:get({TCPConn, seq}) of
        undefined ->
            erlang:put({TCPConn, seq}, Seq),
            Seq;
        FirstSeq ->
            Seq - FirstSeq
    end.

%% @doc Get the relative ack number for this TCP flow
-spec rel_ack(boolean(), non_neg_integer(), flow_id()) -> non_neg_integer().
rel_ack(false, _, _) -> ignore;
rel_ack(true, ACK, TCPConn) ->
    erlang:put({TCPConn, last_ack}, ACK),
    case erlang:get({TCPConn, ack}) of
        undefined ->
            erlang:put({TCPConn, ack}, ACK),
            erlang:put({TCPConn, last_ack}, 1),
            1;
        FirstACK ->
            %% ACKs start at 1, not 0
            OffsetACK = 1 + (ACK - FirstACK),
            erlang:put({TCPConn, last_ack}, OffsetACK),
            OffsetACK
    end.

%% @doc Is this TCP packet an ACK?
-spec is_ack_set(bitstring()) -> boolean().
is_ack_set(<<_:4, 1:1, _:4>>) -> true;
is_ack_set(<<_:4, 0:1, _:4>>) -> false.

-spec make_tcp_flow(inet:ip4_address(),
                    non_neg_integer(),
                    inet:ip4_address(),
                    non_neg_integer()) -> flow_id().

make_tcp_flow(SrcIp, SrcPort, DstIp, DstPort) ->
    {SrcIp, SrcPort, DstIp, DstPort}.

-spec to_addr(bitstring()) -> inet:ip4_address().
to_addr(<<A,B,C,D>>) -> {A,B,C,D}.

%% @doc Get any extra bytes from a previous packet in the flow
-spec get_extra_bytes(flow_id()) -> bitstring().
get_extra_bytes(FlowId) ->
    case ets:lookup(?REST_TABLE, FlowId) of
        [{FlowId, Binary}] -> Binary;
        [] -> <<>>
    end.

%% @doc Save any extra bytes from protocol parsing
-spec set_extra_bytes(flow_id(), bitstring()) -> ok.
set_extra_bytes(FlowId, Bytes) ->
    true = ets:insert(?REST_TABLE, {FlowId, Bytes}),
    ok.

-spec clear_extra_bytes(flow_id()) -> ok.
clear_extra_bytes(FlowId) ->
    true = ets:insert(?REST_TABLE, {FlowId, <<>>}),
    ok.

%% @doc Skip packet on error, unless user requested to see errors
-spec skip_on_error(opts(), packet_info(), {error, atom()}) -> skip | {error, atom()}.
skip_on_error(Opts, PacketInfo, Error) ->
    case maps:get(data_errors, Opts, false) of
        false -> skip;
        true -> PacketInfo#{payload => Error}
    end.

%% @doc Have we seen the ACK from this packet before?
-spec reuse_ack(packet_info()) -> boolean().
reuse_ack(#{tcp_flow_id := Id, ack_rel := CurrentACK}) ->
    case erlang:get({Id, last_ack}) of
        CurrentACK -> true;
        _ -> false
    end.

%% Hextream parsing
%% Taken from https://github.com/b/hex, and that itself
%% from http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

-spec translate_type(non_neg_integer()) -> string().
translate_type(Type)
    when is_map_key(Type, ?MSG_KINDS) ->
        maps:get(Type, ?MSG_KINDS);
translate_type(_) ->
    "UNKNOWN_KIND".
