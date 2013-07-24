-module(rest_tool_webchat).
-include("yaws_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-include("../../mqtt_broker/include/mqtt.hrl").
-export([out/1]).

%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case Arg#arg.pathinfo of
		undefined -> 
			{status, 404};
		_ -> 
			out(Arg, string:tokens(Arg#arg.pathinfo, "/"))
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
out(Arg, ["index"]) ->
	Body = erlang:binary_to_list(Arg#arg.clidata),
	%error_logger:info_msg("Body:~p~n", [Body]),

	{XmlEle, _} = xmerl_scan:string(Body),
	
	[ToUserNameEle] = xmerl_xpath:string("/xml/ToUserName", XmlEle),
	[ToUserNameText] = ToUserNameEle#xmlElement.content,
	ToUserName = ToUserNameText#xmlText.value,
	error_logger:info_msg("WebChat ToUserName: ~p~n", [ToUserName]),
	
	[FromUserNameEle] = xmerl_xpath:string("/xml/FromUserName", XmlEle),
	[FromUserNameText] = FromUserNameEle#xmlElement.content,
	FromUserName = FromUserNameText#xmlText.value,
	error_logger:info_msg("WebChat FromUserName: ~p~n", [FromUserName]),

	[ContentEle] = xmerl_xpath:string("/xml/Content", XmlEle),
	[ContentText] = ContentEle#xmlElement.content,
	Content0 = ContentText#xmlText.value,
	error_logger:info_msg("WebChat Content: ~p~n", [Content0]),

	UserId = get_user_id(FromUserName),
	Content = string:to_lower(Content0),

	case Content of
		"do" ->
			DeviceId = "000000000010",
			SwitchId = 1,
			Val = 1,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"df" ->
			DeviceId = "000000000010",
			SwitchId = 1,
			Val = 0,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"aao" ->
			DeviceId = "000000000002",
			SwitchId = 1,
			Val = 1,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"aaf" ->
			DeviceId = "000000000002",
			SwitchId = 1,
			Val = 0,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"abo" ->
			DeviceId = "000000000002",
			SwitchId = 2,
			Val = 1,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"abf" ->
			DeviceId = "000000000002",
			SwitchId = 2,
			Val = 0,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"aco" ->
			DeviceId = "000000000002",
			SwitchId = 3,
			Val = 1,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"acf" ->
			DeviceId = "000000000002",
			SwitchId = 3,
			Val = 0,
			change_switch_status(UserId, DeviceId, SwitchId, Val);
		"wf" ->
			DeviceId = "000000000003",
			Cmd = "poweroff",
			send_command(UserId, DeviceId, Cmd);
		"wr" ->
			DeviceId = "000000000003",
			Cmd = "restart",
			send_command(UserId, DeviceId, Cmd);
		"lf" ->
			DeviceId = "000000000004",
			Cmd = "poweroff",
			send_command(UserId, DeviceId, Cmd);
		"lr" ->
			DeviceId = "000000000004",
			Cmd = "restart",
			send_command(UserId, DeviceId, Cmd);
		_ ->
			do_nothing
	end,

	Response = case Content of
		"s" ->
			get_status(UserId, FromUserName, ToUserName);
		_ ->
			get_command_list(UserId, FromUserName, ToUserName)
	end,

	{html, Response};
	
out(_Arg, _) ->
	{status, 404}.


change_switch_status(UserId, DeviceId, SwitchId, Val) ->
    mqtt_broker:publish(#publish_msg{
		from_client_id = DeviceId,
		from_user_id = UserId,
		exclusive_client_id = "000000000001", 
		data = {switch_control, {DeviceId, SwitchId, Val}}
	}).


send_command(UserId, DeviceId, Cmd) ->
    mqtt_broker:publish(#publish_msg{
		from_client_id = DeviceId,
		from_user_id = UserId,
		exclusive_client_id = "000000000001", 
		data = {send_command, {DeviceId, Cmd}}
	}).


get_status(UserId, FromUserName, ToUserName) ->
    DeviceIds = model_dev_device_user:get_device_ids(UserId),

	ValueJoinFun = fun({Key, Val}) ->
		Val2 = case erlang:is_integer(Val) of
			true ->
				erlang:integer_to_list(Val);
			false ->
				Val
		end,
		io_lib:format("~s/~s", [Key, Val2])		
	end,

    Fun = fun({_Permission, DeviceId}) ->
        case DeviceId of
            "000000000001" ->
                ignore;
            _ ->
                Device = model_dev_device:get(DeviceId),

                Values = model_dev_status:get_all_values_by_deviceId(DeviceId),
				Values2 = lists:keysort(1, Values),
				ValueList = [ValueJoinFun(X) || X <- Values2],
				ValuesString = string:join(ValueList, ","),

				DeviceName = Device#dev_device.name,
				OnOffLine = case model_mqtt_session:is_online(DeviceId) of
					true ->
						"online";
					false ->
						"offline"
				end,

				io_lib:format("~s[~s]: ~s", [DeviceName, OnOffLine, ValuesString])
        end
    end,

    Status = [Fun(X) || X <- DeviceIds],

	Content0 = ["Status:" | Status],
	Content = string:join(Content0, "\n"),

	build_text_response(FromUserName, ToUserName, Content).


get_command_list(UserId, FromUserName, ToUserName) ->
    DeviceIds = model_dev_device_user:get_device_ids(UserId),

    Fun = fun({_Permission, DeviceId}) ->
        case DeviceId of
            "000000000001" ->
                "";
            "000000000005" ->
                "";
            "000000000010" ->
                "[do] demo switch1 on\n[df] demo switch1 off";
            "000000000002" ->
                "[aao] windows pc on\n[aaf] windows pc off\n[abo] switch2 on\n[abf] switch2 off\n\[aco] switch3 on\n[acf] switch3 off";
            "000000000003" ->
                "[wf] windows pc poweroff\n[wr] windows pc reboot";
            "000000000004" ->
                "[lf] linux poweroff\n[lr] linux reboot"
        end
    end,

    Commands = [Fun(X) || X <- DeviceIds],

	Content0 = ["Command list:\n[s] get status" | Commands],
	Content = string:join(Content0, "\n"),

	build_text_response(FromUserName, ToUserName, Content).


build_text_response(ToUserName, FromUserName, Content) ->
	Xml = {xml, [
		{'ToUserName', [ToUserName]},
		{'FromUserName', [FromUserName]},
		{'CreateTime', [erlang:integer_to_list(tools:epoch_seconds())]},
		{'MsgType', ["text"]},
		{'Content', [Content]},
		{'FuncFlag', ["0"]}
	]},
	Response = xmerl:export_simple_element(Xml, xmerl_xml),
	%error_logger:info_msg("Response:~p~n", [Response]),
	
	Response.


get_user_id(WebChatUser) ->
	{ok, UserBindings} = application:get_env(webchat_user_binding),
	proplists:get_value(WebChatUser, UserBindings, "anonymous").
	
