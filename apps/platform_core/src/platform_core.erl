-module(platform_core).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->   
	application:start(sasl),
    mnesia:start(),
    
	ok = application:start(platform_core),

	case application:get_env(platform_core, enable_cleaner) of
		{ok, true} ->
			ok = cleaner:start();
		_ ->
			no_start
	end,

	case application:get_env(platform_core, enable_db_backup) of
		{ok, true} ->
			ok = db_backup:start();
		_ ->
			no_start
	end,

	case application:get_env(platform_core, enable_iptracker) of
		{ok, true} ->
			ok = iptracker:start();
		_ ->
			no_start
	end,

	ok = interface_http:start(),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
