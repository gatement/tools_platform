-module(model_mqtt_pub_permission).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		exist/3,
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


exist(Topic, ClientId, UserId) ->
	Fun = fun() ->
		lists:append(
			qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_permission), 
						X#mqtt_pub_permission.topic =:= Topic, 
						X#mqtt_pub_permission.client_id =:= ClientId, 
						X#mqtt_pub_permission.user_id =:= UserId])),
			qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_permission), 
						X#mqtt_pub_permission.topic =:= "#", 
						X#mqtt_pub_permission.client_id =:= ClientId, 
						X#mqtt_pub_permission.user_id =:= UserId]))
		)
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Models} -> true
	end.


list() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_permission)]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({mqtt_pub_permission, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
