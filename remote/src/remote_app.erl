-module(remote_app).
-behaviour(application).
%% application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    remote_sup:start_link().


stop(_State) ->
    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
