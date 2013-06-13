-module(interface_tcp_server_device).
%% API
-export([start_link/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Port} = application:get_env(tcp_port_device),
    {ok, HeartbeatCheckingInterval} = application:get_env(heartbeat_checking_interval_device),
    error_logger:info_msg("starting [~p] at port: ~p~n", [?MODULE, Port]),
    gen_tcp_server:start_link(tcp_sup_device, handler_tcp_device, Port, HeartbeatCheckingInterval, []).


%% ===================================================================
%% Local Functions
%% ===================================================================
