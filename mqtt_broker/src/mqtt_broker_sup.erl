-module(mqtt_broker_sup).
-behaviour(supervisor).
%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = {mqtt_broker_server, {mqtt_broker_server, start_link, []},
              permanent, 1000, supervisor, [mqtt_broker_server]},
              
    Children = [Server],
    RestartStrategy = {one_for_one, 30, 10},
    {ok, {RestartStrategy, Children}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
