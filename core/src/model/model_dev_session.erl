-module(model_dev_session).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
	get/1, 
	get_by_sn/1, 
	get_by_pid/1, 
	all_keys/0,
	delete/1,
	delete_by_pid/1,
	delete_by_sn/1,  
	get_count/0, 
	update_pid/2,
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


get(Id) ->
	Fun = fun() ->
		mnesia:read(dev_session, Id)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


get_by_sn(Sn) ->
    Fun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_session), 
                          X#dev_session.sn =:= Sn]))
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Models} -> Models;
        _ -> error
    end.


get_by_pid(Pid) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(dev_session), 
						  X#dev_session.pid =:= Pid]))
	end,

	case mnesia:transaction(Fun) of
        {atomic, Models} -> Models;
		_ -> error
	end.


all_keys() ->
	Fun = fun() ->
		mnesia:all_keys(dev_session)
	end,

	case mnesia:transaction(Fun) of
		{atomic, Keys} -> Keys;
		_ -> error
	end.


delete(Id) ->
	Fun = fun() ->
		mnesia:delete({dev_session, Id})
	end,

	mnesia:transaction(Fun).	


delete_by_pid(Pid) ->
	Fun = fun() ->
		Models = qlc:e(qlc:q([X || X <- mnesia:table(dev_session), 
						  			X#dev_session.pid =:= Pid])),
		[mnesia:delete({dev_session, X#dev_session.id}) || X <- Models]
	end,

	mnesia:transaction(Fun).


delete_by_sn(Sn) ->
	Fun = fun() ->
		Models = qlc:e(qlc:q([X || X <- mnesia:table(dev_session), 
						  			X#dev_session.sn =:= Sn])),
		[mnesia:delete({dev_session, X#dev_session.id}) || X <- Models]
	end,

	mnesia:transaction(Fun).


get_count() ->
	mnesia:table_info(dev_session, size).


update_pid(DeviceId, Pid) -> 
	case ?MODULE:get(DeviceId) of
		undefined ->
			ignore;
		Model ->
			Model2 = Model#dev_session{pid = Pid},

			Fun = fun() ->
				mnesia:write(Model2)	  
			end,

			case mnesia:transaction(Fun) of
				{atomic, ok} -> Model2;
				_ -> error
			end
	end.


is_online(Sn) ->
    case get_by_sn(Sn) of
    	error -> error;
        [] -> false;
        _ -> true
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
