-module(mqtt).
-export([build_connect/2,
		build_connack/1,
		build_publish/2,
		build_pingreq/0,
		build_pingresp/0,
		build_disconnect/0]).

-vsn("0.1.0").

%% implementation of MQTT_V3.1

-define(CONNECT,     2#00010000).
-define(CONNACK,     2#00100000).
-define(PUBLISH,     2#00110000).
-define(PUBACK,      2#01000000).
-define(PUBREC,      2#01010000).
-define(PUBREL,      2#01100000).
-define(PUBCOMP,     2#01110000).
-define(SUBSCRIBE,   2#10000000).
-define(SUBACK,      2#10010000).
-define(UNSUBSCRIBE, 2#10100000).
-define(UNSUBACK,    2#10110000).
-define(PINGREQ,     2#11000000).
-define(PINGRESP,    2#11010000).
-define(DISCONNECT,  2#11100000).

-define(DUP0,        2#00000000).
-define(DUP1,        2#00001000).

-define(QOS0,        2#00000000).
-define(QOS1,        2#00000010).
-define(QOS2,        2#00000100).

-define(RETAIN0,     2#00000000).
-define(RETAIN1,     2#00000001).


%% ===================================================================
%% API functions
%% ===================================================================

build_connect(ClientId, KeepAliveTimer) ->
	VariableHeader = get_connect_variable_header(KeepAliveTimer),

	Payload = get_connect_payload(ClientId),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = get_fixed_header(?CONNECT, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).


build_connack(ReturnCode) ->
	VariableHeader = [0, ReturnCode],

	Length = 2,
	FixedHeader = get_fixed_header(?CONNACK, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader]).


%% only Qos = 0 is supported
build_publish(Topic, Payload) ->
	VariableHeader = get_publish_variable_header(Topic),

	Length = erlang:size(VariableHeader) + erlang:size(Payload),
	FixedHeader = get_fixed_header(?PUBLISH, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader, VariableHeader, Payload]).


build_pingreq() ->
	Length = 0,
	FixedHeader = get_fixed_header(?PINGREQ, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader]).


build_pingresp() ->
	Length = 0,
	FixedHeader = get_fixed_header(?PINGRESP, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader]).


build_disconnect() ->
	Length = 0,
	FixedHeader = get_fixed_header(?DISCONNECT, ?DUP0, ?QOS0, ?RETAIN0, Length),

	erlang:list_to_binary([FixedHeader]).



%% ===================================================================
%% Local Functions
%% ===================================================================

%% Fixed header.
%% bit     |7  6  5  4   |3        |2    1    |0
%% byte 1  |Message Type |DUP flag |QoS level |RETAIN
%% byte 2  |Remaining Length
get_fixed_header(MsgType, Dup, Qos, Retain, Length) ->
	Header = MsgType bor Dup bor Qos bor Retain,
	RemainingLength = get_remaining_length(Length, []),
	[Header|RemainingLength].


get_remaining_length(0, Result) ->
	lists:reverse(Result);
get_remaining_length(Length, Result) ->
	Digit0 = Length rem 128,
	X = Length div 128,
	Digit = if
		X > 0 ->
			Digit0 bor 16#80;
		true ->
			Digit0
	end,
	get_remaining_length(X, [Digit|Result]).


get_connect_payload(ClientId) ->
	get_utf8(ClientId).


get_connect_variable_header(KeepAliveTimer) ->
	ProtocalName = [0, 6, <<"MQIsdp">>],
	ProtocalVer = 3,

	%% Connect flags.
	%% bit     |7    |6        |5           |4    3   |2         |1             |0
	%% byte 1  |User |Password |Will Retain |Will QoS |Will Flag |Clean Session |Reserved
	ConnectFlags = 2#00000001,

	KeepAliveTimerH = KeepAliveTimer div 256,
	KeepAliveTimerL = KeepAliveTimer rem 256,

	erlang:list_to_binary([ProtocalName, ProtocalVer, ConnectFlags, KeepAliveTimerH, KeepAliveTimerL]).


get_publish_variable_header(Topic) ->
	TopicBin = get_utf8(Topic),
	TopicBin.


get_utf8(Content) ->
    ContentBin = unicode:characters_to_binary(Content, latin1),

    ContentBinLen = erlang:size(ContentBin),
    ContentBinLenH = ContentBinLen div 256,
    ContentBinLenL = ContentBinLen rem 256,

    erlang:list_to_binary([ContentBinLenH, ContentBinLenL, ContentBinLen]).
