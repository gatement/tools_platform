-module(model_gbl_setting).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/1,
		set/2]).


%% ===================================================================
%% API functions
%% ===================================================================

get(Key) ->
	case get_setting(Key) of
		inexistence -> "";
		Setting -> Setting#gbl_setting.value
	end.


set(Key, Value) ->
	Setting = #gbl_setting{key = Key, value = Value},

	Fun = fun() -> 
		mnesia:write(Setting) 
	end,

	mnesia:transaction(Fun).


%% ===================================================================
%% Local Functions
%% ===================================================================

get_setting(Key) ->
	Fun = fun() ->
		mnesia:read({gbl_setting, Key})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> inexistence;
		{atomic, [Setting]} -> Setting
	end.
