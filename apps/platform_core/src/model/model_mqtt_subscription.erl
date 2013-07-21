-module(model_mqtt_subscription).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		exist/2,
		list/0,
		delete/1,
		get_online_subscription_client_pids/2,
		get_subscription_client_ids/2]).

%% ===================================================================
%% API functions
%% ===================================================================

create(Model) ->
	Fun = fun() ->
		mnesia:write(Model)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Model;
		_ -> error
	end.


exist(ClientId, Topic) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(mqtt_subscription), 
						X#mqtt_subscription.client_id =:= ClientId, 
						X#mqtt_subscription.topic =:= Topic]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Models} -> true
	end.


list() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(mqtt_subscription)]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({mqtt_subscription, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


get_online_subscription_client_pids(ExclusiveClientId, Topic) ->
	Topics = case Topic of
		"#" ->
			["#"];

		_ ->
			ClientId = lists:nth(1, string:tokens(Topic, "/")),
			TempTopic = lists:flatten(io_lib:format("/~s/+", [ClientId])),

			[Topic, "#", TempTopic]
	end,

	Fun = fun() -> 
		qlc:e(qlc:q([Session#mqtt_session.pid || Session <- mnesia:table(mqtt_session), 
						Sub <- mnesia:table(mqtt_subscription), 
						Sub#mqtt_subscription.client_id =/= ExclusiveClientId,
						Sub#mqtt_subscription.client_id =:= Session#mqtt_session.client_id,
						lists:any(fun(Elem) -> 
							Elem =:= Sub#mqtt_subscription.topic
						end, Topics)]))
	end,
	{atomic, Pids} = mnesia:transaction(Fun),

	Pids.


get_subscription_client_ids(ExclusiveClientId, Topic) ->
	Topics = case Topic of
		"#" ->
			["#"];

		_ ->
			ClientId = lists:nth(1, string:tokens(Topic, "/")),
			TempTopic = lists:flatten(io_lib:format("/~s/+", [ClientId])),

			[Topic, "#", TempTopic]
	end,

	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(mqtt_subscription), 
						X#mqtt_subscription.client_id =/= ExclusiveClientId,
						lists:any(fun(Elem) -> 
							Elem =:= X#mqtt_subscription.topic
						end, Topics)]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


%% ===================================================================
%% Local Functions
%% ===================================================================
