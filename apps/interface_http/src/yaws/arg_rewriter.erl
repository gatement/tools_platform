-module(arg_rewriter).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-export([arg_rewrite/1]).


%% ===================================================================
%% API functions
%% ===================================================================

arg_rewrite(Arg) ->
	case get_session(Arg) of
		[] ->
			do_rewrite(Arg);

		[Session] ->
			UserId = Session#usr_session.user_id,
			UserName = Session#usr_session.user_name,
			SessionId = Session#usr_session.id,

			model_usr_session:update_last_active(SessionId),

			{abs_path, RequestPath} = (Arg#arg.req)#http_request.path,

			% if request to the admin path, must be an Administrator
			case string:str(string:to_lower(RequestPath), "/admin/") of
				1 -> 
					case model_usr_user:is_admin(UserId) of
						true -> 
							Arg#arg
							{
								state = #arg_state
								{
									session_id = SessionId,
									user_id = UserId, 
									user_name = UserName
								}
							};
						false ->  {status, 404}
					end;
				_ ->
					Arg#arg
					{
						state = #arg_state
						{
							session_id = SessionId,
							user_id = UserId, 
							user_name = UserName
						}
					}
			end
		end.


%% ===================================================================
%% Local Functions
%% ===================================================================
 
%% these pages must be shippable without a auth cookie
no_auth_paths() ->
	[
	"/js/lib/jquery-1.10.0.min.js",
	"/js/lib/jquery.validate.min.js", 
	"/login.yaws",
	"/css/login.css", 
	"/js/login.js",
	"/register.yaws",
	"/css/register.css",
	"/js/register.js",
	"/forgot_password.yaws",
	"/css/forgot_password.css",
	"/js/forgot_password.js",
	"/user/session",
	"/user/session/jsonp",
	"/user/login",
	"/user/add",
	"/user/password/reset",
	"/device/socket",
	"/device/device/list/jsonp",
	"/device/switch/update/jsonp",
	"/device/command/send/jsonp",
	"/webchat/index"
	 ].


do_rewrite(Arg) ->
	Req = Arg#arg.req,
	{abs_path, Path} = Req#http_request.path,

	Func = fun(AuthPath) ->
		LowerSubPath = string:to_lower(string:substr(Path, 1, erlang:length(AuthPath))),
		LowerSubPath =:= AuthPath
	end,

	case lists:any(Func, no_auth_paths()) of
		true ->
			% leave it if it is login or related urls
			Arg;
		false ->
			Arg#arg
			{
				req = Req#http_request
				{
					path = {abs_path, "/login.yaws?target_url=" ++ yaws_api:url_encode(Path)}
				}
			}
	end.


get_session(Arg) ->
	CookieName = ?USR_SESSION_COOKIE_ID,
	case yaws_api:find_cookie_val(CookieName, (Arg#arg.headers)#headers.cookie) of
		[] -> [];
		SessionId -> 
			case model_usr_session:get(SessionId) of
				error -> [];
				Session -> Session
			end
	end.
