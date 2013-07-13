-module(rest_tool_webchat).
-include("yaws_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../../platform_core/include/tools_platform.hrl").
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
	%io:format("Body:~p~n", [Body]),

	{XmlEle, _} = xmerl_scan:string(Body),
	
	[ToUserNameEle] = xmerl_xpath:string("/xml/ToUserName", XmlEle),
	[ToUserNameText] = ToUserNameEle#xmlElement.content,
	ToUserName = ToUserNameText#xmlText.value,
	io:format("ToUserName:~p~n", [ToUserName]),
	
	[FromUserNameEle] = xmerl_xpath:string("/xml/FromUserName", XmlEle),
	[FromUserNameText] = FromUserNameEle#xmlElement.content,
	FromUserName = FromUserNameText#xmlText.value,
	io:format("FromUserName:~p~n", [FromUserName]),

	[ContentEle] = xmerl_xpath:string("/xml/Content", XmlEle),
	[ContentText] = ContentEle#xmlElement.content,
	Content = ContentText#xmlText.value,
	io:format("Content:~p~n", [Content]),

	case Content of
		"1" ->
			DeviceId = "000000000010",
			SwitchId = 1,
			Val = 1,
			change_switch_status(DeviceId, SwitchId, Val);
		"2" ->
			DeviceId = "000000000010",
			SwitchId = 1,
			Val = 0,
			change_switch_status(DeviceId, SwitchId, Val);
		"5" ->
			DeviceId = "000000000002",
			SwitchId = 2,
			Val = 1,
			change_switch_status(DeviceId, SwitchId, Val);
		"6" ->
			DeviceId = "000000000002",
			SwitchId = 2,
			Val = 0,
			change_switch_status(DeviceId, SwitchId, Val);
		_ ->
			do_nothing
	end,

	Response = get_response_data(FromUserName, ToUserName),
	{html, Response};
	
out(_Arg, _) ->
	{status, 404}.


change_switch_status(DeviceId, SwitchId, Val) ->
	UserId = "admin",
	{Topic, PublishData} = mqtt_cmd:switch_control(DeviceId, SwitchId, Val),
	mqtt_broker:publish("000000000001", Topic, DeviceId, UserId, PublishData).


get_response_data(FromUserName, ToUserName) ->
	{ok, TrustList} = application:get_env(webchat_trust_list),
	Trusted = lists:any(fun(Ele)->  Ele =:= FromUserName end, TrustList),

	Content0 = "Conmand list:\n[0] get status\n[1] demo switch1 on\n[2] demo switch1 off",
	Content1 = case Trusted of
		true ->
			"\n[3] windows pc on\n[4] windows pc off\n[5] switch1 on\n[6] switch1 off\n\[7] switch2 on\n[8] switch2 off";
		false ->
			""
	end,	
	Content = Content0 ++ Content1,

	Xml = {xml, [
		{'ToUserName', [FromUserName]},
		{'FromUserName', [ToUserName]},
		{'CreateTime', [erlang:integer_to_list(tools:epoch_seconds())]},
		{'MsgType', ["text"]},
		{'Content', [Content]},
		{'FuncFlag', ["0"]}
	]},
	Response = xmerl:export_simple_element(Xml, xmerl_xml),
	%io:format("Response:~p~n", [Response]),
	
	Response.
