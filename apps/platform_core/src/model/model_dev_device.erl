-module(model_dev_device).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
		update/4,
		get/1,
		get/2,
		get_info/1,
		list/0,
		all_keys/0,
		update_user_id/2,
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


update(DeviceId, Name, UserId, Type) ->
	Model = ?MODULE:get(DeviceId),
	case Model of
		error ->
			erlang:exit(mnesia_error);
		undefined ->
			do_nothing;
		_ ->
			Model2 = Model#dev_device{name = Name, user_id = UserId, type = Type},

			Fun = fun() ->
				mnesia:write(Model2)	  
			end,

			case mnesia:transaction(Fun) of
				{atomic, ok} -> Model2;
				_ -> error
			end
	end.


get(DeviceId) ->
	Fun = fun() ->
		mnesia:read(dev_device, DeviceId)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


get(DeviceId, UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(dev_device), 
						X#dev_device.user_id =:= UserId,
						X#dev_device.device_id =:= DeviceId]))
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


get_info(DeviceId) ->
	Fun = fun() ->
		mnesia:read(dev_device, DeviceId)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> {undefined, non_exist};
		{atomic, [Model]} -> {Model#dev_device.type, Model#dev_device.name};
		_ -> {undefined, non_exist}
	end.


list() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(dev_device)]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


all_keys() ->
	Fun = fun() ->
		mnesia:all_keys(dev_device)
	end,

	case mnesia:transaction(Fun) of
		{atomic, Keys} -> Keys;
		_ -> error
	end.


update_user_id(DeviceId, UserId) ->
	Model = ?MODULE:get(DeviceId),
	Model2 = Model#dev_device{user_id = UserId},

	Fun = fun() -> 
		mnesia:write(Model2) 
	end,

	mnesia:transaction(Fun).


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({dev_device, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
