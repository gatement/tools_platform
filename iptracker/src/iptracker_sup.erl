-module(iptracker_sup).
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
    Server = {iptracker_server, {iptracker_server, start_link, []},
              permanent, 10000, worker, [iptracker_server]},

    {ok, {{one_for_one, 300, 100}, [Server]}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
