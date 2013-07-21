-module(mqtt_broker).
-include("../../platform_core/include/tools_platform.hrl").
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
			Subs = model_mqtt_subscription:get_subscription_client_ids(ExclusiveClientId, Topic),
			%error_logger:info_msg("[~p] subcriptions: ~p.~n", [?MODULE, Subs]),
			Fun = fun(Sub) ->
				Sent = case model_mqtt_session:get(Sub#mqtt_subscription.client_id) of
					undefined -> false;
					error -> false;
					Session -> 
						Pid = Session#mqtt_session.pid,
						case tools:is_pid_alive(Pid) of
							false ->
								false;
							true ->
								Pid ! {send_tcp_data, PublishData},
								true
						end
				end,

				case Sent of
					true ->
						good;
					false ->
						case Sub#mqtt_subscription.persistence of
							false ->
								no_persistence;
							true ->
								SubClientId = Sub#mqtt_subscription.client_id,
								Expired = tools:epoch_seconds() + Sub#mqtt_subscription.ttl, 
								SubQueueId = tools:generate_id(SubClientId),

								error_logger:info_msg("[~p] create sub_queue(topic: ~p): queueId: ~p|clientId: ~p|expired: ~p.~n", [?MODULE, Topic, SubQueueId, SubClientId, Expired]),

								model_mqtt_pub_queue:create(#mqtt_pub_queue{
									id = SubQueueId,
									client_id = SubClientId,
									expired = Expired,
									data = PublishData
								})
						end
				end
			end,

			lists:foreach(Fun, Subs)
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
