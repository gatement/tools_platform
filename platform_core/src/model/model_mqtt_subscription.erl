-module(model_mqtt_subscription).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		exist/2,
		list/0,
		delete/1,
		get_online_subscription_client_pids/2]).

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
	Fun = fun() -> 
		qlc:e(qlc:q([Session#mqtt_session.pid || Session <- mnesia:table(mqtt_session), 
						Sub <- mnesia:table(mqtt_subscription), 
						Sub#mqtt_subscription.client_id =/= ExclusiveClientId,
						Sub#mqtt_subscription.client_id =:= Session#mqtt_session.client_id, 
						Sub#mqtt_subscription.topic =:= Topic]))
	end,

	{atomic, Pids} = mnesia:transaction(Fun),

	Pids2 = if
		Topic =/= "#" ->
			Fun2 = fun() -> 
				qlc:e(qlc:q([Session#mqtt_session.pid || Session <- mnesia:table(mqtt_session), 
								Sub <- mnesia:table(mqtt_subscription), 
								Sub#mqtt_subscription.client_id =/= ExclusiveClientId,
								Sub#mqtt_subscription.client_id =:= Session#mqtt_session.client_id, 
								Sub#mqtt_subscription.topic =:= "#"]))
			end,
			{atomic, Pids0} = mnesia:transaction(Fun2),
			Pids0;
		true ->
			[]
	end,

	[Pids2 | Pids].
	

%% ===================================================================
%% Local Functions
%% ===================================================================
