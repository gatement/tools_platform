-module(mqtt_cmd).
-export([online/1,
		offline/1]).


%% ===================================================================
%% API functions
%% ===================================================================

online(ClientId) ->
	Topic = lists:flatten(io_lib:format("/~s/online", [ClientId])),
    Payload = <<1>>,
    mqtt:build_publish(Topic, Payload).


offline(ClientId) ->
	Topic = lists:flatten(io_lib:format("/~s/offline", [ClientId])),
    Payload = <<2>>,
    mqtt:build_publish(Topic, Payload).


%% ===================================================================
%% Local Functions
%% ===================================================================
