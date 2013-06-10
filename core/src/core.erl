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
	interface_http:start(),
	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
