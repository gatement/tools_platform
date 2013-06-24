-module(remote_sup).
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
    Server = {remote_server, {remote_server, start_link, []},
              permanent, 10000, worker, [remote_server]},

    {ok, {{one_for_one, 1, 1}, [Server]}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
