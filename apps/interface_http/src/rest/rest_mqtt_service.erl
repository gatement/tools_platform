-module(rest_mqtt_service).
-include("yaws_api.hrl").
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
			UserId = (Arg#arg.state)#arg_state.user_id,		
			out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(Arg, ["send_msg"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	Msg = proplists:get_value("msg", Vals),
	error_logger:info_msg("[~p] send_msg: ~p~n", [?MODULE, Msg]),

    %% publish it
    mqtt_broker:send_msg(UserId, Msg),

	Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(Arg, ["send_persistence_msg"], UserId) -> 
	Vals = yaws_api:parse_post(Arg),
	Msg = proplists:get_value("msg", Vals),

    %% publish it
    mqtt_broker:send_persistence_msg(UserId, Msg),

	Result = [{"success", true}],
	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _, _UserId) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================
