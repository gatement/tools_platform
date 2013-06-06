-module(core).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->   
	application:start(sasl),
    mnesia:start(),
    
	application:start(core),
	application:start(cleaner),
	application:start(monitor),
	application:start(db_backup),
	application:start(interface_http).


%% ===================================================================
%% Local Functions
%% ===================================================================
