-module(mqtt_utils).
-export([is_connack_success/1]).

-vsn("0.1.0").

%% implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

is_connack_success(Data) ->
	case erlang:size(Data) of
		4 ->
			<<_:3/binary, ReturnCode:1/binary>> = Data,
			if
				ReturnCode =:= 0 ->
					true;
				true ->
					false
			end;
		_ ->
			false
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
