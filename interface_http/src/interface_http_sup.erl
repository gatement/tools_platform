-module(interface_http_sup).
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
    Server = {interface_http_server, {interface_http_server, start, []},
              permanent, 2000, worker, [interface_http_server]},

    {ok, {{one_for_all, 0, 1}, [Server]}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
