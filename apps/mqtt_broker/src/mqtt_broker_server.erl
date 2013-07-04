-module(mqtt_broker_server).
%% API
-export([start_link/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Port} = application:get_env(tcp_port),
    error_logger:info_msg("starting [~p] at port: ~p~n", [?MODULE, Port]),
    gen_tcp_server:start_link(mqtt_gen_tcp_server, mqtt_broker_handler, Port, undefined).


%% ===================================================================
%% Local Functions
%% ===================================================================
