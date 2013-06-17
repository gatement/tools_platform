-module(mqtt_broker_server).
%% API
-export([start_link/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Port} = application:get_env(tcp_port),
    {ok, HeartbeatCheckingInterval} = application:get_env(heartbeat_checking_interval),
    error_logger:info_msg("starting [~p] at port: ~p~n", [?MODULE, Port]),
    gen_tcp_server:start_link(mqtt_gen_tcp_server, handler_tcp_device, Port, HeartbeatCheckingInterval, []).


%% ===================================================================
%% Local Functions
%% ===================================================================
