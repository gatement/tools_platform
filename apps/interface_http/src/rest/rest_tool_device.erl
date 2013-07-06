-module(rest_tool_device).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-export([out/1]).


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
					UserId = (Arg#arg.state)#arg_state.user_id,
					DeviceEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_DEVICE_ENABLED),
					case DeviceEnabled of
						true ->
							out(Arg, Paths, UserId);
						false -> 
							{status, 404}
					end
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


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================

device_html() ->	
	{redirect_local, "/tools/device.html"}.
