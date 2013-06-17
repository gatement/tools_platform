-module(gen_tcp_server).
-export([start_link/5, start_link/6, tcp_reply/2]).
-export([behaviour_info/1]).

%% ===================================================================
%% behavior callbacks
%% ===================================================================

behaviour_info(callbacks) ->
    [{init,1},
     {handle_data, 5},
     {terminate, 2},
     {get_hearbeat_data, 0}];
behaviour_info(_Other) ->
    undefined.


%% ===================================================================
%% API functions
%% ===================================================================

start_link(SupName, Callback, Port, HeartbeatCheckingInterval, UserArgs) ->
    start_link(SupName, Callback, undefined, Port, HeartbeatCheckingInterval, UserArgs).


start_link(SupName, Callback, IP, Port, HeartbeatCheckingInterval, UserArgs) ->
    gen_tcp_server_connection_sup:start_link(SupName, Callback, IP, Port, HeartbeatCheckingInterval, UserArgs).


tcp_reply(Socket, Data) ->
    error_logger:info_msg("send to socket ~p with data: ~p~n", [Socket, Data]),
    gen_tcp:send(Socket, Data).


%% ===================================================================
%% Local Functions
%% ===================================================================
