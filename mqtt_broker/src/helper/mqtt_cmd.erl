-module(mqtt_cmd).
-include("mqtt.hrl").
-export([online/1,
		offline/1,
		switch_status/2,
        switch_control/3,
        send_command/2]).


%% ===================================================================
%% API functions
%% ===================================================================

online(ClientId) ->
	Topic = lists:flatten(io_lib:format("/~s/online", [ClientId])),
    Payload = <<?CMD_ONLINE>>,
    mqtt:build_publish(Topic, Payload).


offline(ClientId) ->
	Topic = lists:flatten(io_lib:format("/~s/offline", [ClientId])),
    Payload = <<?CMD_OFFLINE>>,
    mqtt:build_publish(Topic, Payload).


switch_status(ClientId, Status) ->
    Topic = lists:flatten(io_lib:format("/~s/switch_status", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_SWITCH_STATUS, Status]),
    mqtt:build_publish(Topic, Payload).


switch_control(Topic, SwitchId, Status) ->
    Payload = erlang:list_to_binary([?CMD_SWITCH_CONTROL, SwitchId, Status]),
    mqtt:build_publish(Topic, Payload).


send_command(Topic, Cmd) ->
    Payload = erlang:list_to_binary([?CMD_SEND_COMMAND, Cmd]),
    mqtt:build_publish(Topic, Payload).


%% ===================================================================
%% Local Functions
%% ===================================================================
