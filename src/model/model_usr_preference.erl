-module(model_usr_preference).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/2,
		set/3]).


%% ===================================================================
%% API functions
%% ===================================================================

get(UserId, Key) ->
	case get_user_preference(UserId, Key) of
		inexistence -> false;
		UserPreference -> UserPreference#usr_preference.value
	end.


set(UserId, Key, Value) ->
	UserPreference = case get_user_preference(UserId, Key) of
			inexistence -> #usr_preference{id = tools:generate_id("user_preference"), user_id = UserId, key = Key};
			Val -> Val
	end,

	Fun = fun() -> 
		mnesia:write(UserPreference#usr_preference{value = Value}) 
	end,

	mnesia:transaction(Fun).


%% ===================================================================
%% Local Functions
%% ===================================================================

get_user_preference(UserId, Key) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usr_preference), 
						X#usr_preference.user_id =:= UserId, 
						X#usr_preference.key =:= Key]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> inexistence;
		{atomic, [UserPreference]} -> UserPreference
	end.
