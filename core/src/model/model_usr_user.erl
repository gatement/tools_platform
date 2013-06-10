-module(model_usr_user).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
		update/1,
		get_users/0, 
		exist/1, 
		exist/2, 
		get/1, 
		get/2,
		get_name/1,
		change_password/2, 
		change_password/3, 
		list/3,
		search/1,
		is_admin/1]).


%% ===================================================================
%% API functions
%% ===================================================================

get_users() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usr_user)]))
		end,
	{atomic, Users} = mnesia:transaction(Fun),
    {erlang:length(Users), Users}.


exist(Id) ->
	Fun = fun() -> 
		mnesia:read({usr_user, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _User} -> true
	end.


exist(Id, Email) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usr_user), 
						X#usr_user.id =:= Id, 
						X#usr_user.email =:= Email]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Users} -> true
	end.


get(Id, Pwd) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usr_user), 
						X#usr_user.id =:= Id, 
						X#usr_user.password =:= crypto:md5(Pwd)]))
	end,
	{atomic, User} = mnesia:transaction(Fun),
	User.


get(Id) ->
	Fun = fun() -> 
		mnesia:read({usr_user, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [User]} -> User
	end.


get_name(Id) ->
	Fun = fun() -> 
		mnesia:read({usr_user, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [User]} -> User#usr_user.name
	end.


create(User) ->
	Pwd = crypto:md5(User#usr_user.password),

	% get default values	
	Enabled = case User#usr_user.enabled of
		undefined -> true;
		_ -> User#usr_user.enabled
	end,
	Admin = case User#usr_user.admin of
		undefined -> false;
		_ -> User#usr_user.admin
	end,

	Fun = fun() ->
			mnesia:write(User#usr_user{password = Pwd,
									  admin = Admin,
									  enabled = Enabled})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


change_password(UserId, NewPwd) ->
	Fun = fun() ->
			case mnesia:read({usr_user, UserId}) of
				[] -> 
					error;
				[User] -> 
					case mnesia:write(User#usr_user{password=crypto:md5(NewPwd)}) of
						ok -> ok;
						_ -> error
					end
			end
	end,
	{atomic, Result} = mnesia:transaction(Fun),
	Result.


change_password(UserId, OldPwd, NewPwd) ->
	Fun = fun() ->
			case mnesia:read({usr_user, UserId}) of
				[] -> 
					error;
				[User] -> 
					EncryptedOldPwd = crypto:md5(OldPwd),
					if
						User#usr_user.password /= EncryptedOldPwd -> 
							error;
						true ->
							case mnesia:write(User#usr_user{password=crypto:md5(NewPwd)}) of
								ok -> ok;
								_ -> error
							end
					end
			end
	end,
	{atomic, Result} = mnesia:transaction(Fun),
	Result.


update(User) ->
	Fun = fun() ->
			mnesia:write(User)
	end,
	mnesia:transaction(Fun).


list(ContainStr, Max, ExceptUserId) ->
	case ContainStr of
		[] -> 
			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(usr_user)]))
			end;
		_ ->
			ContainStrLower = string:to_lower(ContainStr),

			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(usr_user),
					(string:str(string:to_lower(X#usr_user.id), ContainStrLower) > 0) 
						or (string:str(string:to_lower(X#usr_user.name), ContainStrLower) > 0)
						or (string:str(string:to_lower(X#usr_user.email), ContainStrLower) > 0)]))
			end
	end,

	{atomic, Users} = mnesia:transaction(Fun),
	AutocompleteUsers = [#user_autocomplete_item{
		label=User#usr_user.id ++ "(" ++ User#usr_user.name ++ ")", 
		value=User#usr_user.id
		} || User <- Users, User#usr_user.id =/= ExceptUserId],

	lists:sublist(AutocompleteUsers, Max).


search(ContainStr) ->
	case ContainStr of
		[] -> 
			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(usr_user)]))
			end;
		_ ->
			ContainStrLower = string:to_lower(ContainStr),

			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(usr_user),
					(string:str(string:to_lower(X#usr_user.id), ContainStrLower) > 0) 
						or (string:str(string:to_lower(X#usr_user.name), ContainStrLower) > 0)
						or (string:str(string:to_lower(X#usr_user.email), ContainStrLower) > 0)]))
			end
	end,

	{atomic, Users} = mnesia:transaction(Fun),

	[#user_search_result{
			id=User#usr_user.id,
			name=User#usr_user.name,
			email=User#usr_user.email,
			enabled=User#usr_user.enabled,
			admin=User#usr_user.admin,
			last_login= case User#usr_user.last_login of undefined -> ""; LastLogin -> tools:datetime_string('yyyy-MM-dd hh:mm:ss', LastLogin) end,
			note = model_usr_preference:get(User#usr_user.id, ?USR_PREFERENCE_NOTE_ENABLED),
			word = model_usr_preference:get(User#usr_user.id, ?USR_PREFERENCE_WORD_ENABLED),
			gallery = model_usr_preference:get(User#usr_user.id, ?USR_PREFERENCE_GALLERY_ENABLED),
			monitor = model_usr_preference:get(User#usr_user.id, ?USR_PREFERENCE_MONITOR_ENABLED),
			device = model_usr_preference:get(User#usr_user.id, ?USR_PREFERENCE_DEVICE_ENABLED)
		} || User <- Users].


is_admin(UserId) ->
	User = model_usr_user:get(UserId),
	User#usr_user.admin.


%% ===================================================================
%% Local Functions
%% ===================================================================
