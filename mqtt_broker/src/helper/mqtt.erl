-module(mqtt).
-export([build_connect/1]).

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

-define(DUP0,  2#00000000).
-define(DUP1,  2#00001000).

-define(QOS0,  2#00000000).
-define(QOS1,  2#00000010).
-define(QOS2,  2#00000100).

-define(RETAIN0,  2#00000000).
-define(RETAIN1,  2#00000001).


%% ===================================================================
%% API functions
%% ===================================================================

build_connect(_ClientId) ->
	Length = 1,
	FixedHeader = get_fixed_header(?CONNECT, ?DUP0, ?QOS0, ?RETAIN0, Length),
	erlang:list_to_binary(FixedHeader).


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
