-module(interface_tcp_sup).
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
    ServerDevice = {interface_tcp_server_device, {interface_tcp_server_device, start_link, []},
              permanent, 1000, supervisor, [interface_tcp_server_device]},
              
    Children = [ServerDevice],
    RestartStrategy = {one_for_one, 10, 1},
    {ok, {RestartStrategy, Children}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
