-module(model_dev_device_user).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/2, 
		get/1,
		list/2,
		delete/2]).

%% ===================================================================
%% API functions
%% ===================================================================

create(Model, OwnerUserId) ->
	case model_dev_device:get(Model#dev_device_user.device_id, OwnerUserId) of
		undefined ->
			[];
		error ->
			[];
		_ ->
			Fun = fun() ->
				mnesia:write(Model)	  
			end,

			case mnesia:transaction(Fun) of
				{atomic, ok} -> Model;
				_ -> error
			end
	end.


get(Id) ->
	Fun = fun() ->
		mnesia:read(dev_device_user, Id)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


list(DeviceId, OwnerUserId) ->
	case model_dev_device:get(DeviceId, OwnerUserId) of
		undefined ->
			[];
		error ->
			[];
		_ ->
			Fun = fun() -> 
				qlc:e(qlc:q([io_lib:format("~s(~s)", [U#usr_user.name, U#usr_user.id]) || X <- mnesia:table(dev_device_user),
																		 			  U <- mnesia:table(usr_user), 
																		 			  U#usr_user.id =:= X#dev_device_user.user_id,
																					  X#dev_device_user.device_id =:= DeviceId]))
			end,

			{atomic, Models} = mnesia:transaction(Fun),
			Models
	end.


delete(Id, OwnerUserId) ->
	Model = ?MODULE:get(Id),
	case model_dev_device:get(Model#dev_device_user.device_id, OwnerUserId) of
		undefined ->
			error;
		error ->
			error;
		_ ->
			Fun = fun() ->
				mnesia:delete({dev_device_user, Id})
			end,
			case mnesia:transaction(Fun) of
				{atomic, ok} -> ok;
				_ -> error
			end
	end.
	

%% ===================================================================
%% Local Functions
%% ===================================================================
