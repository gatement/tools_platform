-module(rest_mqtt).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,	
	IsAdmin = model_usr_user:is_admin(UserId),
	case IsAdmin of
		true ->
			out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId);
		false -> 
			{status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(_Arg, ["subscription", "list"], _UserId) ->
	Subscriptions = model_mqtt_subscription:list(),	
	MapFunc = fun(Sub) ->
		Ttl = erlang:round(Sub#mqtt_subscription.ttl / 24 / 3600),
		
		{struct, [
			{"id", Sub#mqtt_subscription.id}, 
			{"client_id", Sub#mqtt_subscription.client_id}, 
			{"topic", Sub#mqtt_subscription.topic}, 
			{"qos", Sub#mqtt_subscription.qos}, 
			{"ttl", Ttl}, 
			{"desc", Sub#mqtt_subscription.desc}
		]}
	end,

    SubscriptionList = lists:map(MapFunc, Subscriptions),
    Result = [{"success", true}, {"data", {array, SubscriptionList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["subscription", "add"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ClientId = proplists:get_value("client_id", Vals),
	Topic = proplists:get_value("topic", Vals),
	Qos = erlang:list_to_integer(proplists:get_value("qos", Vals)),
	Ttl = erlang:list_to_integer(proplists:get_value("ttl", Vals)),
	Desc = proplists:get_value("desc", Vals),

	Ttl2 = Ttl * 24  * 3600, % days to seconds 

	case model_mqtt_subscription:exist(ClientId, Topic) of
		true ->
			do_nothing;
		false ->
			model_mqtt_subscription:create(#mqtt_subscription{
					id = uuid:to_string(uuid:uuid1()), 
					client_id = ClientId, 
					topic = Topic,
					qos = Qos,
					ttl = Ttl2,
					desc = Desc
				})
	end,

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["subscription", "delete"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	SubscriptionId = proplists:get_value("subscription_id", Vals),

	model_mqtt_subscription:delete(SubscriptionId),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, ["pub_permission", "list"], _UserId) ->
	PubPermissions = model_mqtt_pub_permission:list(),	
    PubPermissionList = [{struct, tools:record_to_list(PubPermission, record_info(fields, mqtt_pub_permission))} || PubPermission <- PubPermissions],
    Result = [{"success", true}, {"data", {array, PubPermissionList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["pub_permission", "add"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ClientId = proplists:get_value("client_id", Vals),
	UserId = proplists:get_value("user_id", Vals),
	Topic = proplists:get_value("topic", Vals),
	Desc = proplists:get_value("desc", Vals),

	case model_mqtt_pub_permission:exist(ClientId, UserId, Topic) of
		true ->
			do_nothing;
		false ->
			model_mqtt_pub_permission:create(#mqtt_pub_permission{
					id = uuid:to_string(uuid:uuid1()), 
					client_id = ClientId,  
					user_id = UserId, 
					topic = Topic,
					desc = Desc
				})
	end,

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["pub_permission", "delete"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	PubPermissionId = proplists:get_value("pub_permission_id", Vals),

	model_mqtt_pub_permission:delete(PubPermissionId),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["client", "is_online"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	ClientId = proplists:get_value("client_id", Vals),

	IsOnline = model_mqtt_session:is_online(ClientId),
    Result = [{"success", true}, {"data", IsOnline}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================
