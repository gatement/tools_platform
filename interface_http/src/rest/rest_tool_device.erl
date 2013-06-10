-module(rest_tool_device).
-include("yaws_api.hrl").
-include("../../core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	DeviceEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_DEVICE_ENABLED),

	case DeviceEnabled of
		true ->
			case Arg#arg.pathinfo of
				undefined -> 
					device_html();
				_ -> 
					out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
			end;

		false -> 
			{status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(Arg, ["socket"], _UserId) -> 
	CallbackMod = socket_device,
    Opts = [{origin, "http://" ++ (Arg#arg.headers)#headers.host}],
    {websocket, CallbackMod, Opts};


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================

device_html() ->	
	{redirect_local, "/tools/device.html"}.
