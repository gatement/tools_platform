-module(model_mqtt_session).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
	get/1, 
	get_by_pid/1, 
	delete/1,
	delete_by_pid/1,
	get_count/0, 
	is_online/1]).

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


get(ClientId) ->
	Fun = fun() ->
		mnesia:read(mqtt_session, ClientId)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


get_by_pid(Pid) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(mqtt_session), 
						  X#mqtt_session.pid =:= Pid]))
	end,

	case mnesia:transaction(Fun) of
        {atomic, Models} -> Models;
		_ -> error
	end.


delete(ClientId) ->
	Fun = fun() ->
		mnesia:delete({mqtt_session, ClientId})
	end,

	mnesia:transaction(Fun).	


delete_by_pid(Pid) ->
	Fun = fun() ->
		Models = qlc:e(qlc:q([X || X <- mnesia:table(mqtt_session), 
						  			X#mqtt_session.pid =:= Pid])),
		[mnesia:delete({mqtt_session, X#mqtt_session.client_id}) || X <- Models]
	end,

	mnesia:transaction(Fun).


get_count() ->
	mnesia:table_info(mqtt_session, size).


is_online(ClientId) ->
    case ?MODULE:get(ClientId) of
    	error -> false;
        undefined -> false;
        _ -> true
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
