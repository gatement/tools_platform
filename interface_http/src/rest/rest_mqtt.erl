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
    SubscriptionList = [{struct, tools:record_to_list(Subscription, record_info(fields, mqtt_subscription))} || Subscription <- Subscriptions],
    Result = [{"success", true}, {"data", {array, SubscriptionList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["subscription", "add"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	Name = proplists:get_value("name", Vals),
	ClientId = proplists:get_value("client_id", Vals),
	Topic = proplists:get_value("topic", Vals),
	Qos = erlang:list_to_integer(proplists:get_value("qos", Vals)),

	case model_mqtt_subscription:exist(ClientId, Topic) of
		true ->
			do_nothing;
		false ->
			model_mqtt_subscription:create(#mqtt_subscription{
					id = uuid:to_string(uuid:uuid1()), 
					name = Name,
					client_id = ClientId, 
					topic = Topic,
					qos = Qos
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
