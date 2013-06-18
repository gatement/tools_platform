-module(mqtt_utils).
-export([is_connack_success/1,
		strip_fixed_header/1,
		get_msg_length/1]).

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
	{FixedLength, RestLength} = get_msg_length(Msg, 1, true, 0, 1),
	{FixedLength, RestLength}.


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

	<<Len:1/integer, RestData/binary>> = Msg2,
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

	<<Len:1/integer, RestData/binary>> = Msg2,
	if
		Len > 127 ->
			Length2 = Length + (Len band 127) * Multiplier,
			Multiplier2 = Multiplier * 128,			
			get_msg_length(RestData, Multiplier2, false, Length2, Loop + 1);
		true ->
			{Loop + 1, Length}
	end.
