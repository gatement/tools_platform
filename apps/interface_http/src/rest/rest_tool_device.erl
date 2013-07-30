-module(rest_tool_device).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-include("../../mqtt_broker/include/mqtt.hrl").
-export([out/1,
		get_device_list/1,
		update_switch_status/2,
		send_command/2]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case Arg#arg.pathinfo of
		undefined -> 
			device_html();
		_ -> 
			case string:tokens(Arg#arg.pathinfo, "/") of
				["socket"] ->
					web_socket(Arg);
				Paths ->
					Vals = yaws_api:parse_query(Arg),
					SessionId = proplists:get_value("sid", Vals),
					Response = case model_usr_session:get(SessionId) of
						error ->
							[{"success", false}, {"data", "Bad session, please re-login."}];
						[] ->
							[{"success", false}, {"data", "Bad session, please re-login."}];
						[UserSession] ->
							UserId = UserSession#usr_session.user_id,
							DeviceEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_DEVICE_ENABLED),
							case DeviceEnabled of
								true ->
									model_usr_session:update_last_active(SessionId),
									{ok, Data} = json2:decode_string(proplists:get_value("data", Vals)),
									out(Arg, Paths, Data, UserId);
								false -> 
									[{"success", false}, {"data", "user has no device subscription."}]
							end
					end,
					Callback = proplists:get_value("callback", Vals),
					Result = json2:encode({struct, Response}),
					Result2 = io_lib:format("~s(~s);", [Callback, Result]),
					{content, "application/json", Result2}
			end
	end.


%% ===================================================================
%% Web API
%% ===================================================================

web_socket(_Arg) ->
	%{_, _, _, _, HostWithProtocal} = lists:keyfind("Origin", 3, (Arg#arg.headers)#headers.other),
    %Opts = [{origin, HostWithProtocal}],
    Opts = [],
	CallbackMod = socket_device,    
    {websocket, CallbackMod, Opts}.


out(_Arg, ["device", "list", "jsonp"], _Data, UserId) ->
	get_device_list(UserId);

out(_Arg, ["switch", "update", "jsonp"], Data, UserId) ->
	update_switch_status(Data, UserId);

out(_Arg, ["command", "send", "jsonp"], Data, UserId) ->
	send_command(Data, UserId);

out(_Arg, _, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Common Functions
%% ===================================================================

get_device_list(UserId) ->
    DeviceIds = model_dev_device_user:get_device_ids(UserId),

    Fun = fun({Permission, DeviceId}) ->
		Device = model_dev_device:get(DeviceId),
		Values = model_dev_status:get_all_values_by_deviceId(DeviceId),

		{struct, [
				{"device_id", DeviceId}, 
				{"type", erlang:atom_to_list(Device#dev_device.type)}, 
				{"name", Device#dev_device.name},
				{"online", model_mqtt_session:is_online(DeviceId)},
				{"permission", Permission},
				{"values", {struct, Values}}
		]}
    end,

    Devices = [Fun(X) || X <- DeviceIds],

    [{"success", true}, {"data", {array, Devices}}].


update_switch_status(Data, UserId) ->
    {struct,[{"device_id", DeviceId}, {"switch_id", SwitchId}, {"status", Status}]} = Data,
    %io:format("~ndevice-id|switch|status: ~p|~p|~p~n~n", [DeviceId, Switch, Status]),

    %% publish it
    mqtt_broker:publish(#publish_msg{
		from_client_id = DeviceId,
		from_user_id = UserId,
		exclusive_client_id = "000000000001", 
		data = {switch_control, {DeviceId, erlang:list_to_integer(SwitchId), Status}}
	}),

    [{"success", true}, {"data", "ok."}].


send_command(Data, UserId) ->
    {struct,[{"device_id", DeviceId}, {"cmd", Cmd}]} = Data,
    %io:format("~ndevice-id|cmd: ~p|~p~n~n", [DeviceId, Cmd]),

    %% publish it
    mqtt_broker:publish(#publish_msg{
		from_client_id = DeviceId,
		from_user_id = UserId,
		exclusive_client_id = "000000000001", 
		data = {send_command, {DeviceId, Cmd}}
	}),

    [{"success", true}, {"data", "ok."}].


%% ===================================================================
%% Local Functions
%% ===================================================================

device_html() ->	
	{redirect_local, "/tools/device.html"}.
