-module(device).
-include("../../mqtt_broker/include/mqtt.hrl").
-export([start/0, start_client/3]).

-define(CONNECTION_TIMEOUT, 10000).
-define(KEEP_ALIVE_TIMER, 300).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    client:start(),

    {ok, ServerHost} = application:get_env(client, server_host),
    {ok, ServerPort} = application:get_env(client, server_port),
    {ok, ClientCountInit} = application:get_env(client, client_count_init),
    {ok, ClientCountTotal} = application:get_env(client, client_count_total),
    {ok, ClientCreatingInterval} = application:get_env(client, client_creating_interval),

    error_logger:info_msg("connecting to ~p server - ~s:~p~n", [?MODULE, ServerHost, ServerPort]),
    
    start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, 1).


start_client(ServerHost, ServerPort, ClientId) ->
    case gen_tcp:connect(ServerHost, ServerPort, [binary, {active, true}]) of
        {ok, Socket} ->
            error_logger:info_msg("client===============================> ~p~n", [ClientId]), 

            %% send CONNECT
            {ok, MacBase} = application:get_env(client, mac_base),
            ConnectData = get_connect_data(MacBase + ClientId),
            error_logger:info_msg("sending CONNECT: ~p~n", [ConnectData]),
            gen_tcp:send(Socket, ConnectData),

            %% waiting for CONNACK
            receive
                {tcp, Socket, Msg} -> 
                    error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

                    case is_connack_success(Msg) of
                        true ->            
                            case application:get_env(client, run_mode) of
                                {ok, once} ->
                                    DisconnectData = get_disconnect_data(),
                                    gen_tcp:send(Socket, DisconnectData),
                                    error_logger:info_msg("sent DISCONNECT data: ~p~n", [DisconnectData]),

                                    timer:sleep(500),
                                    gen_tcp:close(Socket),
                                    error_logger:info_msg("=================================================> pass~n", []),
                                    init:stop();
                                {ok, live} ->
                                    {ok, DataSendingInterval} = application:get_env(client, data_sending_interval),
                                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, false)
                            end;
                        false ->
                            gen_tcp:close(Socket),
                            Reason = "not a good CONNACK",
                            reconnect(ServerHost, ServerPort, ClientId, Reason)
                    end
            after
                ?CONNECTION_TIMEOUT ->
                    gen_tcp:close(Socket),
                    Reason = "no CONNACK",
                    reconnect(ServerHost, ServerPort, ClientId, Reason)
            end;
        _ ->
            Reason = "connection error",
            reconnect(ServerHost, ServerPort, ClientId, Reason)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

start_processes(_, _, _, ClientCountTotal, _, ClientId) when ClientCountTotal < ClientId ->
    ok;
start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, ClientId) ->
    erlang:spawn(?MODULE, start_client, [ServerHost, ServerPort, ClientId]),

    if
        ClientCountInit > ClientId ->
             timer:sleep(ClientCreatingInterval);
        true ->
            no_sleep
    end,

    start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, ClientId + 1).


client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, SendData) ->
    case SendData of
        true ->
            %% send StatusData
            StatusData =  get_status_data(ClientId),
            error_logger:info_msg("sending StatusData: ~p~n", [StatusData]),
            gen_tcp:send(Socket, StatusData);

            %% send PINGREQ
            %PingReqData =  get_pingreq_data(),
            %error_logger:info_msg("sending PINGREQ: ~p~n", [PingReqData]),
            %gen_tcp:send(Socket, PingReqData);
        _ ->
            do_nothing
    end,

    receive
        {tcp, Socket, Msg} -> 
            error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

            case handle_data(Socket, Msg) of
                true ->
                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, false);
                false ->
                    Reason = "received DISCONNECT",
                    reconnect(ServerHost, ServerPort, ClientId, Reason)
            end;

        {tcp_closed, _Socket} ->
            error_logger:info_msg("tcp_closed ~p~n", [erlang:self()]),

            Reason = "tcp_closed",
            reconnect(ServerHost, ServerPort, ClientId, Reason);

        AnyMsg ->
            error_logger:info_msg("received any data ~p: ~p~n", [erlang:self(), AnyMsg]),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, false)

    after
        DataSendingInterval ->
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, true)
    end.


reconnect(ServerHost, ServerPort, ClientId, Reason) ->
    {ok, ReconnectWaitTime} = application:get_env(client, reconnect_wait_time),
    timer:sleep(ReconnectWaitTime),

    error_logger:info_msg("reconnecting because of ~p ~p~n", [Reason, erlang:self()]),
    start_client(ServerHost, ServerPort, ClientId). %% reconnect


get_connect_data(ClientId) ->
    ClientIdStr = get_client_id_string(ClientId),
    mqtt:build_connect(ClientIdStr, ?KEEP_ALIVE_TIMER).


get_disconnect_data() ->
    mqtt:build_disconnect().


get_status_data(ClientId) ->
    ClientIdStr = get_client_id_string(ClientId),
    Topic = "/" ++ ClientIdStr ++ "/status",

    Payload = <<2#00000001>>,
    mqtt:build_publish(Topic, Payload).


get_pingreq_data() ->
    mqtt:build_pingreq().


is_connack_success(Data) ->
    mqtt_utils:is_connack_success(Data).


handle_data(Socket, RawData) ->
    <<TypeCode:8/integer, _/binary>> = RawData,
    TypeCode2 = TypeCode bsr 4,
    case TypeCode2 of
        ?DISCONNECT ->
            gen_tcp:close(Socket),
            false;
        ?PINGRESP ->
            true
    end.


get_client_id_string(ClientId) ->
    ClientIdStr = erlang:integer_to_list(ClientId, 16),
    tools:prefix_string(ClientIdStr, 12, "0").
