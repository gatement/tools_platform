-module(device).
-include("../../apps/mqtt_broker/include/mqtt.hrl").
-export([start/0, start_client/3]).

-define(CONNECTION_TIMEOUT, 10000).
-define(KEEP_ALIVE_TIMER, 10).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    test_int:start(),

    {ok, ServerHost} = application:get_env(test_int, server_host),
    {ok, ServerPort} = application:get_env(test_int, server_port),
    {ok, ClientCountInit} = application:get_env(test_int, client_count_init),
    {ok, ClientCountTotal} = application:get_env(test_int, client_count_total),
    {ok, ClientCreatingInterval} = application:get_env(test_int, client_creating_interval),
    {ok, MacBase} = application:get_env(test_int, mac_base),

    error_logger:info_msg("connecting to ~p server - ~s:~p~n", [?MODULE, ServerHost, ServerPort]),
    
    start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, MacBase, 1).


start_client(ServerHost, ServerPort, ClientId) ->
    case gen_tcp:connect(ServerHost, ServerPort, [binary, {active, true}]) of
        {ok, Socket} ->
            error_logger:info_msg("client===============================> ~p~n", [ClientId]), 

            %% send CONNECT
            {ok, UserName} = application:get_env(test_int, username),
            {ok, Password} = application:get_env(test_int, password),
            ConnectData = mqtt:build_connect(ClientId, ?KEEP_ALIVE_TIMER, UserName, Password),
            error_logger:info_msg("sending CONNECT: ~p~n", [ConnectData]),
            gen_tcp:send(Socket, ConnectData),

            %% waiting for CONNACK
            receive
                {tcp, Socket, Msg} -> 
                    error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

                    case mqtt_utils:is_connack_success(Msg) of
                        true ->            
                            case application:get_env(test_int, run_mode) of
                                {ok, once} ->
                                    DisconnectData = mqtt:build_disconnect(),
                                    gen_tcp:send(Socket, DisconnectData),
                                    error_logger:info_msg("sent DISCONNECT data: ~p~n", [DisconnectData]),

                                    timer:sleep(500),
                                    gen_tcp:close(Socket),
                                    error_logger:info_msg("=================================================> pass~n", []),
                                    init:stop();
                                {ok, live} ->
                                    {ok, DataSendingInterval} = application:get_env(test_int, data_sending_interval),
                                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false)
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

start_processes(_, _, _, ClientCountTotal, _, _, ClientNumber) when ClientCountTotal < ClientNumber ->
    ok;
start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, MacBase, ClientNumber) ->
    ClientId = generate_client_id(MacBase, ClientNumber),
    erlang:spawn(?MODULE, start_client, [ServerHost, ServerPort, ClientId]),

    if
        ClientCountInit > ClientNumber ->
             timer:sleep(ClientCreatingInterval);
        true ->
            no_sleep
    end,

    start_processes(ServerHost, ServerPort, ClientCountInit, ClientCountTotal, ClientCreatingInterval, MacBase, ClientNumber + 1).


client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, SendData) ->
    case SendData of
        true ->
            %% send StatusData
            StatusData = get_status_data(ClientId),
            error_logger:info_msg("sending StatusData: ~p~n", [StatusData]),
            gen_tcp:send(Socket, StatusData);

            %% send PINGREQ
            %PingReqData = mqtt:build_pingreq(),
            %error_logger:info_msg("sending PINGREQ: ~p~n", [PingReqData]),
            %gen_tcp:send(Socket, PingReqData);
        _ ->
            do_nothing
    end,

    receive
        {tcp, Socket, Msg} -> 
            error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

            handle_packages(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, Msg);

        {tcp_closed, _Socket} ->
            error_logger:info_msg("tcp_closed ~p~n", [erlang:self()]),

            Reason = "tcp_closed",
            reconnect(ServerHost, ServerPort, ClientId, Reason);

        AnyMsg ->
            error_logger:info_msg("received any data ~p: ~p~n", [erlang:self(), AnyMsg]),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false)

    after
        DataSendingInterval ->
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, true)
    end.


reconnect(ServerHost, ServerPort, ClientId, Reason) ->
    {ok, ReconnectWaitTime} = application:get_env(test_int, reconnect_wait_time),
    timer:sleep(ReconnectWaitTime),

    error_logger:info_msg("reconnecting because of ~p ~p~n", [Reason, erlang:self()]),
    start_client(ServerHost, ServerPort, ClientId). %% reconnect


get_status_data(ClientId) ->
    Status = 2#00000001,
    mqtt_cmd:switch_status(ClientId, Status).


handle_packages(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, <<>>) ->
    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false);
handle_packages(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, RawData) ->
    {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
    Data = binary:part(RawData, 0, FixedLength + RestLength), 
    <<TypeCode:4/integer, _:4/integer, _/binary>> = Data,

    Result = case TypeCode of
        ?PINGRESP ->
            error_logger:info_msg("received ~p~n", ["PINGRESP"]),
            ok;

        ?PUBLISH ->
            error_logger:info_msg("received ~p~n", ["PUBLISH"]),

            StatusData = get_status_data(ClientId),
            error_logger:info_msg("sending StatusData: ~p~n", [StatusData]),
            gen_tcp:send(Socket, StatusData),
            ok;

        ?DISCONNECT ->
            gen_tcp:close(Socket),
            error_logger:info_msg("received ~p~n", ["DISCONNECT"]),
            stop          
    end,

    case Result of
        stop ->
            Reason = "received DISCONNECT",
            reconnect(ServerHost, ServerPort, ClientId, Reason);
        ok ->
            RestRawData = binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength),
            handle_packages(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, RestRawData)
    end.


generate_client_id(MacBase, ClientNumber) ->
    ClientIdStr = erlang:integer_to_list(MacBase + ClientNumber, 16),
    string:right(ClientIdStr, 12, $0).
