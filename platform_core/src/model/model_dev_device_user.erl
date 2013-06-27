-module(model_dev_device_user).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/2, 
		get/1,
		list/2,
		exist/2,
		delete/2,
		is_have_permission/2]).

%% ===================================================================
%% API functions
%% ===================================================================

create(Model, OwnerUserId) ->
	DeviceId = Model#dev_device_user.device_id,
	UserId = Model#dev_device_user.user_id,
	
	case model_dev_device:get(DeviceId, OwnerUserId) of
		undefined ->
			error;
		error ->
			error;
		_ ->
			case model_usr_user:exist(UserId) of
				false ->
					user_not_exist;
				true ->
					case ?MODULE:exist(DeviceId, UserId) of
						true ->
							ok;
						false ->
							Fun = fun() ->
								mnesia:write(Model)	  
							end,
							{atomic, ok} = mnesia:transaction(Fun),
							ok
					end
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
				qlc:e(qlc:q([{struct, [
								{"id", X#dev_device_user.id},
								{"name", lists:flatten(io_lib:format("~s(~s)", [U#usr_user.name, U#usr_user.id]))}]}
						|| X <- mnesia:table(dev_device_user),
			 			  U <- mnesia:table(usr_user), 
			 			  U#usr_user.id =:= X#dev_device_user.user_id,
						  X#dev_device_user.device_id =:= DeviceId]))
			end,

			{atomic, Models} = mnesia:transaction(Fun),
			Models
	end.


exist(DeviceId, UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(dev_device_user), 
						X#dev_device_user.device_id =:= DeviceId, 
						X#dev_device_user.user_id =:= UserId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Models} -> true
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
	

is_have_permission(DeviceId, UserId) ->
	case model_dev_device:get(DeviceId, UserId) of
		error ->
			false;
		undefined ->		
			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(dev_device_user),
								  X#dev_device_user.device_id =:= DeviceId,
								  X#dev_device_user.user_id =:= UserId]))
			end,
	
			case mnesia:transaction(Fun) of
		        {atomic, []} -> false;
				{atomic, [_Model]} -> true;
				_ -> false
			end;
		_ ->
			true
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
