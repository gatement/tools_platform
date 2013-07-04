-module(rest_device_mgmt).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,	
	IsAdmin = model_usr_user:is_admin(UserId),
	case IsAdmin of
		true ->
			out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId);
		false -> 
			{status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(_Arg, ["list"], _UserId) ->
	Devices = model_dev_device:list(),	
    DeviceList = [{struct, tools:record_to_list(Device, record_info(fields, dev_device))} || Device <- Devices],
    Result = [{"success", true}, {"data", {array, DeviceList}}],

	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["update"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	DeviceId = proplists:get_value("device_id", Vals),
	Name = proplists:get_value("name", Vals),
	UserId = proplists:get_value("user_id", Vals),
	Type = proplists:get_value("type", Vals),

	model_dev_device:update(DeviceId, Name, UserId, Type),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["delete"], _UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	DeviceId = proplists:get_value("device_id", Vals),

	model_dev_device:delete(DeviceId),

    Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================
