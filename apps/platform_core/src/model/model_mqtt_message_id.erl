-module(model_mqtt_message_id).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get_msg_id/1]).

%% ===================================================================
%% API functions
%% ===================================================================

get_msg_id(ClientId) ->
	Fun = fun() ->
			mnesia:write_lock_table(mqtt_message_id),

			Models = mnesia:read(mqtt_message_id, ClientId),

			Model = case Models of
				[] -> #mqtt_message_id{
						client_id = ClientId,
						last_msg_id = 0
					};
				[Model0] -> Model0
			end,

			LastMsgId = Model#mqtt_message_id.last_msg_id,

			MsgId = case LastMsgId of
				65535 -> 1;
				_ -> LastMsgId + 1
			end,

			Model2 = Model#mqtt_message_id{last_msg_id = MsgId},
			mnesia:write(Model2)	  
	end,

	{atomic, MsgId} = mnesia:transaction(Fun),
	MsgId.


%% ===================================================================
%% Local Functions
%% ===================================================================
