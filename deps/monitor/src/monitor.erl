-module(monitor).
-export([start/0, stop/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->
	application:start(sasl),
	timer:sleep(1000),
	application:start(os_mon),
	timer:sleep(1000),
	application:start(monitor).


stop() ->
	application:stop(os_mon),
	timer:sleep(1000),
	application:stop(monitor).


%% ===================================================================
%% Local Functions
%% ===================================================================
