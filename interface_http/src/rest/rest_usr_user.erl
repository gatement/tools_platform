-module(rest_usr_user).
-include("yaws_api.hrl").
-include("../../core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	out(Arg, string:tokens(Arg#arg.pathinfo, "/")).


%% ===================================================================
%% Local Functions
%% ===================================================================

out(Arg, ["logout", "noredirect"]) ->
	logout(Arg),
	{html, "ok"};


out(Arg, ["logout"]) ->
	logout(Arg),
	{redirect_local, "/login.yaws"};


out(Arg, ["login"]) ->
	PostVals = yaws_api:parse_post(Arg),
	Id = proplists:get_value("id", PostVals),
	Pwd = proplists:get_value("pwd", PostVals),
	TargetUrl = case proplists:get_value("target_url", PostVals) of
		undefined -> "/";
		Val -> Val
	end,

	ReturnFunc = fun(OldId, OldTargetUrl, Msg) ->
		EncodedId = yaws_api:url_encode(OldId),
		EncodedTargetUrl = yaws_api:url_encode(OldTargetUrl),
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/login.yaws?id=~s&target_url=~s&msg=~s", [EncodedId, EncodedTargetUrl, EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:get(Id, Pwd) of
		[] ->
			Msg = "Wrong username/password.",
			ReturnFunc(Id, TargetUrl, Msg);
		[User] when User#usr_user.enabled /= true ->
			Msg = "User is disabled, please contact administrator.",
			ReturnFunc(Id, TargetUrl, Msg);
		[User] ->
			% update login time
			model_usr_user:update(User#usr_user{last_login = erlang:universaltime()}),

			% now register the session to the session server
			SessionId = model_usr_session:create(#usr_session{user_id = Id, 
											           user_name=User#usr_user.name}),

			% set cookie and redirect
			[yaws_api:setcookie(?USR_SESSION_COOKIE_ID, SessionId, "/"), {redirect_local, TargetUrl}]
	end;


out(Arg, ["add"]) ->
	Vals = yaws_api:parse_post(Arg),
	Id = proplists:get_value("id", Vals),
	Pwd = proplists:get_value("pwd", Vals),
	Name = proplists:get_value("name", Vals),
	Email = proplists:get_value("email", Vals),

	ReturnFunc = fun(OldId, OldName, OldEmail, Msg) ->
		EncodedId = yaws_api:url_encode(OldId),
		EncodedName = yaws_api:url_encode(OldName),
		EncodedEmail = yaws_api:url_encode(OldEmail),
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/register.yaws?id=~s&name=~s&email=~s&msg=~s", [EncodedId, EncodedName, EncodedEmail, EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:exist(Id) of
		false ->
			case lists:any(fun(E) -> not (((48 =< E) and (E =< 59)) or ((65 =< E) and (E =< 90)) or ((97 =< E) and (E =< 122))  or (E =:=95)) end, Id) of
				true -> 
					Msg = "User id can only contain numbers(0-9), letters(a-z, A-Z) or underscope(_).",
					ReturnFunc(Id, Name, Email, Msg);
				false ->
					IdLength = erlang:length(Id),
					if 
						IdLength > 30 -> 
							Msg = "The max length of user id is 30 characters.",
							ReturnFunc(Id, Name, Email, Msg);
						true ->
							case model_usr_user:create(#usr_user{id = Id, 
								                            password = Pwd, 
								                            name = Name, 
								                            email = Email}) of
								ok ->
									set_user_default_preference(Id),
									{redirect_local, "/login.yaws"};
								error ->
									Msg = "Failed to register.",
									ReturnFunc(Id, Name, Email, Msg)
							end
					end
			end;
		true ->
			Msg = "User is already existing.",
			ReturnFunc(Id, Name, Email, Msg)
	end;


out(Arg, ["update"]) ->
	Vals = yaws_api:parse_post(Arg),
	Name = proplists:get_value("name", Vals),
	Email = proplists:get_value("email", Vals),
	UserId = (Arg#arg.state)#arg_state.user_id,

	ReturnFunc = fun(Msg) ->
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/change_profile.yaws?msg=~s", [EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:get(UserId) of
		error ->
			Msg = "User does not exist.",
			ReturnFunc(Msg);
		User ->
			model_usr_user:update(User#usr_user{name = Name, email = Email}),
			Msg = "Save succeeded.",
			ReturnFunc(Msg)
	end;


out(Arg, ["update_all"]) ->
	Vals = yaws_api:parse_post(Arg),
	UserId = proplists:get_value("id", Vals),
	Name = proplists:get_value("name", Vals),
	Email = proplists:get_value("email", Vals),
	Enabled = erlang:list_to_atom(proplists:get_value("enabled", Vals, "false")),
	Admin = erlang:list_to_atom(proplists:get_value("admin", Vals, "false")),
	Note = erlang:list_to_atom(proplists:get_value("note", Vals, "false")),
	Word = erlang:list_to_atom(proplists:get_value("word", Vals, "false")),
	Gallery = erlang:list_to_atom(proplists:get_value("gallery", Vals, "false")),
	Monitor = erlang:list_to_atom(proplists:get_value("monitor", Vals, "false")),

	ReturnFunc = fun(Msg) ->
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/admin/user_edit.yaws?id=~s&msg=~s", [UserId, EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:get(UserId) of
		error ->
			Msg = "User does not exist.",
			ReturnFunc(Msg);
		User ->
			model_usr_user:update(User#usr_user{name = Name, email = Email, enabled = Enabled, admin = Admin}),

			model_usr_preference:set(UserId, ?USR_PREFERENCE_NOTE_ENABLED, Note),
			model_usr_preference:set(UserId, ?USR_PREFERENCE_WORD_ENABLED, Word),
			model_usr_preference:set(UserId, ?USR_PREFERENCE_GALLERY_ENABLED, Gallery),
			model_usr_preference:set(UserId, ?USR_PREFERENCE_MONITOR_ENABLED, Monitor),

			Msg = "Save succeeded.",
			ReturnFunc(Msg)
	end;


out(Arg, ["password", "reset"]) ->
	Vals = yaws_api:parse_post(Arg),
	Id = proplists:get_value("id", Vals),
	Email = proplists:get_value("email", Vals),

	ReturnFunc = fun(OldId, OldEmail, Msg) ->
		EncodedId = yaws_api:url_encode(OldId),
		EncodedEmail = yaws_api:url_encode(OldEmail),
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/forgot_password.yaws?id=~s&email=~s&msg=~s", [EncodedId, EncodedEmail, EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:exist(Id, Email) of
		true ->
			% update password
			NewPwd = tools:random_string(6),
			model_usr_user:change_password(Id, NewPwd),
			
			% send email
            retrieve_password(Email, NewPwd),
		
		    Msg = "New password has been sent to your email.",
		    ReturnFunc(Id, Email, Msg);
		false ->
		    Msg = "No such an user/email being registered.",
		    ReturnFunc(Id, Email, Msg)
	end;


out(Arg, ["password", "update"]) ->
	Vals = yaws_api:parse_post(Arg),
	OldPwd = proplists:get_value("old_pwd", Vals),
	NewPwd = proplists:get_value("new_pwd", Vals),
	UserId = (Arg#arg.state)#arg_state.user_id,

	ReturnFunc = fun(Msg) ->
		EncodedMsg = yaws_api:url_encode(Msg),
		Url = lists:flatten(io_lib:format("/change_password.yaws?msg=~s", [EncodedMsg])),
		{redirect_local, Url}
	end,
	
	case model_usr_user:get(UserId, OldPwd) of
		[] ->
			Msg = "Old password is wrong.",
			ReturnFunc(Msg);
		[_User] ->
			case model_usr_user:change_password(UserId, OldPwd, NewPwd) of
				error ->
					Msg = "Failed to change password.",
					ReturnFunc(Msg);
				ok ->
					% remove current session
					model_usr_session:delete_by_user(UserId),

					{redirect_local, "/login.yaws"}
			end
	end;


out(Arg, ["list"]) ->
	UserId = (Arg#arg.state)#arg_state.user_id,		
	IsAdmin = model_usr_user:is_admin(UserId),
	case IsAdmin of
		true ->
			Vals = yaws_api:parse_query(Arg),	
			ContainStr = proplists:get_value("term", Vals),
			Max = 20,

			ExceptUserId = (Arg#arg.state)#arg_state.user_id,
			Users = model_usr_user:list(ContainStr, Max, ExceptUserId),

			ReturnUsers = [{struct, tools:record_to_list(User, record_info(fields, user_autocomplete_item))} || User <- Users],
			{content, "application/json", json2:encode({array, ReturnUsers})};

		false -> 
			{status, 404}
	end;


out(Arg, ["search"]) ->
	Vals = yaws_api:parse_post(Arg),
	ContainStr = proplists:get_value("term", Vals),

	Users = model_usr_user:search(ContainStr),

	ReturnUsers = [{struct, tools:record_to_list(User, record_info(fields, user_search_result))} || User <- Users],	
    {content, "application/json", json2:encode({array, ReturnUsers})};


out(Arg, ["session"]) ->
	Vals = yaws_api:parse_post(Arg),
	Id = proplists:get_value("id", Vals),
	Pwd = proplists:get_value("pwd", Vals),
	
	Result = case model_usr_user:get(Id, Pwd) of
		[] ->
			[{"success", false}, {"data", "Failed to create session."}];

		[User] when User#usr_user.enabled /= true ->
			[{"success", false}, {"data", "Failed to create session."}];

		[User] ->
			% update login time
			model_usr_user:update(User#usr_user{last_login = erlang:universaltime()}),

			% now register the session to the session server
			SessionId = model_usr_session:create(#usr_session{user_id = Id, 
											             user_name=User#usr_user.name}),
			
			[{"success", true}, {"data", SessionId}]
	end,

	{content, "application/json", json2:encode({struct, Result})};


out(_Arg, _) ->
	{status, 404}.



logout(Arg) ->
	SessionId = (Arg#arg.state)#arg_state.session_id,
	model_usr_session:delete(SessionId).



retrieve_password(To, NewPwd) ->
	From = model_gbl_setting:get(?GBL_SETTING_EMAIL),
	FromPwd = model_gbl_setting:get(?GBL_SETTING_EMAIL_PASSWORD),
	SiteUrl = model_gbl_setting:get(?GBL_SETTING_SITE_URL),

	Subject = "JohnsonTools - Forgot Password",
	Content = lists:flatten(io_lib:format("Your have asked for resetting your password at ~s. Your new password is: ~s", [SiteUrl, NewPwd])),

	smtp_client:send_email(From, FromPwd, [To], Subject, Content).



set_user_default_preference(UserId) -> 
	%% enable note
	model_usr_preference:set(UserId, ?USR_PREFERENCE_NOTE_ENABLED, true),

	ok.
