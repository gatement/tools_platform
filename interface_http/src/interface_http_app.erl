-module(interface_http_app).
-behaviour(application).
%% application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    interface_http_sup:start_link().


stop(_State) ->
    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
