-module(model_mqtt_subscription).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		exist/2,
		list/0,
		delete/1]).

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


%% ===================================================================
%% Local Functions
%% ===================================================================
