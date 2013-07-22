-module(mqtt_broker).
-include("mqtt.hrl").
-include("../../platform_core/include/tools_platform.hrl").
%% API
-export([publish/1]).


%% ===================================================================
%% API functions
%% ===================================================================

publish(#publish_msg{
	from_client_id = FromClientId,
	from_user_id = FromUserId,
	exclusive_client_id = ExclusiveClientId, 
	topic = Topic, 
	qos = Qos,
	data = Data}) ->

	case model_mqtt_pub_permission:exist(FromClientId, FromUserId, Topic) of
		false ->
			no_permission;

		true ->
			Subs = model_mqtt_subscription:get_subscription_client_ids(ExclusiveClientId, Topic),
			%error_logger:info_msg("[~p] subcriptions: ~p.~n", [?MODULE, Subs]),

			Fun = fun(Sub) ->
				case model_mqtt_session:get(Sub#mqtt_subscription.client_id) of
					undefined -> ignore;
					error -> ignore;
					Session -> 
						Pid = Session#mqtt_session.pid,
						case tools:is_pid_alive(Pid) of
							false -> ignore;
							true -> Pid ! {send_tcp_data, Data}
						end
				end,

				case Sub#mqtt_subscription.qos of
					1 ->
						case Qos of
							1 ->
								SubClientId = Sub#mqtt_subscription.client_id,
								Expired = tools:epoch_seconds() + Sub#mqtt_subscription.ttl, 
								SubQueueId = tools:generate_id(SubClientId),

								error_logger:info_msg("[~p] create sub_queue, topic: ~p, queueId: ~p, clientId: ~p, expired: ~p.~n", [?MODULE, Topic, SubQueueId, SubClientId, Expired]),

								model_mqtt_pub_queue:create(#mqtt_pub_queue{
									id = SubQueueId,
									client_id = SubClientId,
									expired = Expired,
									data = Data
								});

							_ -> do_nothing
						end;
					_ -> do_nothing
				end
			end,

			lists:foreach(Fun, Subs)
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
