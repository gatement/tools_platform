-module(mqtt_broker).
%% API
-export([publish/5]).


%% ===================================================================
%% API functions
%% ===================================================================

publish(ExclusiveClientId, Topic, ClientId, UserId, PublishData) ->
	case model_mqtt_pub_permission:exist(ClientId, UserId, Topic) of
		false ->
			no_permission;
		true ->
		    SubscriptionPids = model_mqtt_subscription:get_online_subscription_client_pids(ExclusiveClientId, Topic),
		    [X ! {send_tcp_data, PublishData} || X <- SubscriptionPids]
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
