-module(model_mqtt_pub_queue).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		clear_old/0,
		get_by_clientId/1,
		delete_by_client_id_msg_id/2]).

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


clear_old() ->
	Now = tools:epoch_seconds(),

	DelFun = fun(Queue) -> 
		mnesia:delete({mqtt_pub_queue, Queue#mqtt_pub_queue.id}) 
	end,

	Fun = fun() -> 
		Queues = qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_queue), 
						X#mqtt_pub_queue.expired < Now])),

		lists:foreach(DelFun, Queues)
	end,

	mnesia:transaction(Fun).


get_by_clientId(ClientId) ->
	Fun = fun() -> 
		Models = qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_queue),
					X#mqtt_pub_queue.client_id =:= ClientId ])),
		[mnesia:delete({mqtt_pub_queue, X#mqtt_pub_queue.id}) || X <- Models],
		Models
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.

delete_by_client_id_msg_id (ClientId, MsgId) ->
	DelFun = fun(Queue) -> 
		mnesia:delete({mqtt_pub_queue, Queue#mqtt_pub_queue.id}) 
	end,

	Fun = fun() -> 
		Queues = qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_queue), 
						X#mqtt_pub_queue.client_id =:= ClientId,
					    X#mqtt_pub_queue.msg_id =:= MsgId])),
			
		lists:foreach(DelFun, Queues)
	end,

	mnesia:transaction(Fun).


%% ===================================================================
%% Local Functions
%% ===================================================================
