-module(model_mqtt_pub_queue).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		clear_old/0]).

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

	Fun = fun() -> 
		Queues = qlc:e(qlc:q([X || X <- mnesia:table(mqtt_pub_queue), 
						X#mqtt_pub_queue.expired < Now])),
		DelFun = fun(Queue) -> 
			mnesia:delete({mqtt_pub_queue, Queue#mqtt_pub_queue.id}) 
		end,
		lists:foreach(DelFun, Queues)
	end,

	mnesia:transaction(Fun),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
