-module(platform_core).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->   
	application:start(sasl),
    mnesia:start(),
    
	application:start(platform_core),

	case application:get_env(platform_core, enable_monitor) of
		{ok, true} ->
			monitor:start();
		_ ->
			no_start
	end,

	case application:get_env(platform_core, enable_cleaner) of
		{ok, true} ->
			cleaner:start();
		_ ->
			no_start
	end,

	case application:get_env(platform_core, enable_db_backup) of
		{ok, true} ->
			db_backup:start();
		_ ->
			no_start
	end,

	case application:get_env(platform_core, enable_iptracker) of
		{ok, true} ->
			iptracker:start();
		_ ->
			no_start
	end,

    application:start(mqtt_broker),
    timer:sleep(3000),

    application:start(mqtt_client),

	interface_http:start(),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
