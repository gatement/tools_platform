-module(rest_gbl_setting).
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
			case Arg#arg.pathinfo of
				undefined -> 
					{status, 404};
				_ -> 
					out(Arg, string:tokens(Arg#arg.pathinfo, "/"))
			end;

		false -> {status, 404}
	end.


%% ===================================================================
%% Web API
%% ===================================================================

out(Arg, ["update"]) ->
	Vals = yaws_api:parse_post(Arg),
	Email = proplists:get_value("email", Vals),
	EmailPassword = proplists:get_value("email_password", Vals),
	SiteUrl = proplists:get_value("site_url", Vals),

	ReturnFunc = fun(Msg) ->
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/admin/setting.yaws?msg=~s", [EncodedMsg])),
		{redirect_local, Url}
	end,
	
	model_gbl_setting:set(?GBL_SETTING_EMAIL, Email),
	model_gbl_setting:set(?GBL_SETTING_EMAIL_PASSWORD, EmailPassword),
	model_gbl_setting:set(?GBL_SETTING_SITE_URL, SiteUrl),

	Msg = "Save succeeded.",
	ReturnFunc(Msg);


out(_Arg, _) ->
	{status, 404}.


%% ===================================================================
%% Local Functions
%% ===================================================================
