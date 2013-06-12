-module(platform_core).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->   
	application:start(sasl),
    mnesia:start(),
    
	application:start(platform_core),
	cleaner:start(),
	monitor:start(),
	db_backup:start(),
    application:start(gen_tcp_server),
    application:start(interface_tcp),
	interface_http:start(),
	iptracker:start(),
	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
