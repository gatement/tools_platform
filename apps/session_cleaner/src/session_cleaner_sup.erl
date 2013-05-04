-module(session_cleaner_sup).
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
    Server = {session_cleaner_server, {session_cleaner_server, start_link, []},
              permanent, 10000, worker, [session_cleaner_server]},

    {ok, {{one_for_one, 30, 10}, [Server]}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
