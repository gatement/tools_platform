-module(device).
-export([start/0, start_client/3]).


%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    client:start(),

    erlang:put(pids, []),

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

            MsgId = 1,

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
                                    gen_tcp:close(Socket),
                                    error_logger:info_msg("=================================================> pass~n", []),
                                    init:stop();
                                {ok, live} ->
                                    {ok, DataSendingInterval} = application:get_env(client, data_sending_interval),
                                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, MsgId)
                            end;
                        false ->
                            gen_tcp:close(Socket),
                            Reason = "not a good CONNACK",
                            reconnect(ServerHost, ServerPort, ClientId, Reason)
                    end
            after
                10000 ->
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


client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, MsgId) ->
    %% send StatusData
    StatusData =  get_status_data(MsgId),
    error_logger:info_msg("sending StatusData: ~p~n", [StatusData]),
    gen_tcp:send(Socket, StatusData),

    MsgId2 = if
        MsgId > 65535 ->
            1;
        true ->
            MsgId 
    end,

    receive
        {tcp, Socket, Msg} -> 
            error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

            handle_data(Socket, Msg),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, MsgId2);

        {tcp_closed, _Socket} ->
            error_logger:info_msg("tcp_closed ~p~n", [erlang:self()]),

            Reason = "tcp_closed",
            reconnect(ServerHost, ServerPort, ClientId, Reason);

        AnyMsg ->
            error_logger:info_msg("received any data ~p: ~p~n", [erlang:self(), AnyMsg]),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, MsgId2)
            
    after
        DataSendingInterval ->
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, MacBase, MsgId2)
    end.


reconnect(ServerHost, ServerPort, ClientId, Reason) ->
    {ok, ReconnectWaitTime} = application:get_env(client, reconnect_wait_time),
    timer:sleep(ReconnectWaitTime),

    error_logger:info_msg("reconnecting because of ~p ~p~n", [Reason, erlang:self()]),
    start_client(ServerHost, ServerPort, ClientId). %% reconnect


get_connect_data(MacInt) ->
    MacString = erlang:integer_to_list(MacInt, 16),
    ClientId = tools:prefix_string(MacString, 12, "0"),

    KeepAliveTimer = 300000,
    mqtt:build_connect(ClientId, KeepAliveTimer).


get_status_data(_MsgId) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Temperature = random:uniform(1024),

    Temperature0 = Temperature rem 256,
    Temperature1 = erlang:round((Temperature - Temperature0)/256),
    
    Data = [16#42, Temperature1, Temperature0],

    erlang:list_to_binary(Data).


is_connack_success(Data) ->
    mqtt_utils:is_connack_success(Data).


handle_data(_, <<>>) ->
    ok;
handle_data(Socket, RawData) ->
    <<TypeCode:3/binary, _/binary>> = RawData,
    case TypeCode of
        <<$#, $0, $#>> ->
            %% heart beat request
            <<_:3/binary, RestRawData/binary>> = RawData,
            ResponseData = <<$D, $A, $A>>,    
            error_logger:info_msg("sending Heartbeat Response: ~p~n", [ResponseData]),
            gen_tcp:send(Socket, ResponseData);
        <<$#, $1, $#>> ->
            %% led control
            <<_:4/binary, RestRawData/binary>> = RawData,
            ResponseData = <<16#43, 0>>,    
            error_logger:info_msg("sending Led Status: ~p~n", [ResponseData]),
            gen_tcp:send(Socket, ResponseData)
    end,

    handle_data(Socket, RestRawData).
