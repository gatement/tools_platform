-module(tools_platform_sup).
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
	Servers = case application:get_env(tools_platform, yaws_embed_mode) of
		{ok, true} ->
		    Server = {tools_platform_server, {tools_platform_server, start, []},
		              permanent, 2000, worker, [tools_platform_server]},
		    [Server];
		_ ->
			[]
	end,

    {ok, {{one_for_all, 0, 1}, Servers}}.


%% ===================================================================
%% Local Functions
%% ===================================================================
