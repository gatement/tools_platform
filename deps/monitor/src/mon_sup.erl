-module(mon_sup).
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
    Server = {mon_server, {mon_server, start_link, []},
              permanent, 2000, worker, [mon_server]},

    {ok, {{one_for_one, 10, 1}, [Server]}}.


%% ===================================================================
%% Local Functions
%% ===================================================================