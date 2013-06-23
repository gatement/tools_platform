-module(mqtt_broker).
%% API
-export([publish/3]).


%% ===================================================================
%% API functions
%% ===================================================================

publish(ExclusiveClientId, Topic, PublishData) ->
    SubscriptionPids = model_mqtt_subscription:get_online_subscription_client_pids(ExclusiveClientId, Topic),
    [X ! {send_tcp_data, PublishData} || X <- SubscriptionPids].


%% ===================================================================
%% Local Functions
%% ===================================================================
