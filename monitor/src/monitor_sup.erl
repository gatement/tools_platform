-module(monitor_sup).
-behaviour(supervisor).
%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    Server = {monitor_server, {monitor_server, start_link, []},
              permanent, 10000, worker, [monitor_server]},

    {ok, {{one_for_one, 30, 10}, [Server]}}.

%% ===================================================================
%% Local Functions
%% ===================================================================
