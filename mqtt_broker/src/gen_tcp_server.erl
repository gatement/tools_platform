-module(gen_tcp_server).
-export([start_link/4, start_link/5, tcp_reply/2]).
-export([behaviour_info/1]).

%% ===================================================================
%% behavior callbacks
%% ===================================================================

behaviour_info(callbacks) ->
    [{init,1},
     {process_data_online, 4},
     {process_data_publish, 4},
     {terminate, 3}];
behaviour_info(_Other) ->
    undefined.


%% ===================================================================
%% API functions
%% ===================================================================

start_link(SupName, Callback, Port, UserArgs) ->
    start_link(SupName, Callback, undefined, Port, UserArgs).


start_link(SupName, Callback, IP, Port, UserArgs) ->
    gen_tcp_server_connection_sup:start_link(SupName, Callback, IP, Port, UserArgs).


tcp_reply(Socket, Data) ->
    %error_logger:info_msg("send to socket ~p with data: ~p~n", [Socket, Data]),
    gen_tcp:send(Socket, Data).


%% ===================================================================
%% Local Functions
%% ===================================================================
