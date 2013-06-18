-module(mqtt_utils).
-export([is_connack_success/1,
		strip_fixed_header/1,
		get_qos/1,
		get_msg_length/1,
		extract_publish_msg/1]).

-vsn("0.1.0").

%% implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

is_connack_success(Data) ->
	case erlang:size(Data) of
		4 ->
			ReturnCode = binary:at(Data, 3),
			if
				ReturnCode =:= 0 ->
					true;
				true ->
					false
			end;
		_ ->
			false
	end.


strip_fixed_header(Msg) ->
	strip_fixed_header(Msg, true).


get_msg_length(Msg) ->
	{FixedHeaderLength, RestLength} = get_msg_length(Msg, 1, true, 0, 1),
	{FixedHeaderLength, RestLength}.


get_qos(Msg) ->
	Byte0 = binary:at(Msg, 0),
	(Byte0 bsr 1) band 2#00000011.


extract_publish_msg(Msg) ->
	{FixedHeaderLength, _} = get_msg_length(Msg),

	TopicLengthH = binary:at(Msg, FixedHeaderLength),
	TopicLengthL = binary:at(Msg, FixedHeaderLength + 1),
	TopicLength = TopicLengthH * 256 + TopicLengthL,
	Topic = binary:part(Msg, FixedHeaderLength + 2, TopicLength),

	Qos = get_qos(Msg),

	BytesBeforePayload = if
		Qos > 0 ->
			MsgIdLengthH = binary:at(Msg, FixedHeaderLength + TopicLength+ 2),
			MsgIdLengthL = binary:at(Msg, FixedHeaderLength + TopicLength + 3),
			MsgIdLength = MsgIdLengthH * 256 + MsgIdLengthL,

			FixedHeaderLength + TopicLength + MsgIdLength + 4;			
		true ->
			FixedHeaderLength + TopicLength + 2
	end,

	Payload = binary:part(Msg, BytesBeforePayload, erlang:byte_size(Msg) - BytesBeforePayload),

	{Topic, Payload}.


%% ===================================================================
%% Local Functions
%% ===================================================================

strip_fixed_header(Msg, Untouched) ->
	Msg2 = case Untouched of
		true ->
			<<_:1/binary, Msg0/binary>> = Msg,
			Msg0;
		false ->
			Msg
	end, 

	<<Len:8/integer, RestData/binary>> = Msg2,
	if 
		Len > 127 ->
			strip_fixed_header(RestData, false);
		true ->
			RestData
	end.


get_msg_length(Msg, Multiplier, Untouched, Length, Loop) ->
	Msg2 = case Untouched of
		true ->
			<<_:1/binary, Msg0/binary>> = Msg,
			Msg0;
		false ->
			Msg
	end,
	<<Len:8/integer, RestData/binary>> = Msg2,
	if
		Len > 127 ->
			Length2 = Length + (Len band 127) * Multiplier,
			Multiplier2 = Multiplier * 128,			
			get_msg_length(RestData, Multiplier2, false, Length2, Loop + 1);
		true ->
			Length2 = Length + (Len band 127) * Multiplier,
			{Loop + 1, Length2}
	end.
