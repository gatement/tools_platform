-module(core).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->   
	application:start(sasl),
    mnesia:start(),
    
	application:start(core),
	cleaner:start(),
	monitor:start(),
	db_backup:start(),
    application:start(gen_tcp_server),
    application:start(interface_tcp),
	interface_http:start(),
	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
