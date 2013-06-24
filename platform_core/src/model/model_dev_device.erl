-module(model_dev_device).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
		get/1,
		all_keys/0,
		update_user_id/2]).

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


get(DeviceId) ->
	Fun = fun() ->
		mnesia:read(dev_device, DeviceId)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


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


%% ===================================================================
%% Local Functions
%% ===================================================================
