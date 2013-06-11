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

            {ok, DataSendingInterval} = application:get_env(client, data_sending_interval),

            case application:get_env(client, run_mode) of
                {ok, test} ->
                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, true, true);
                {ok, press} ->
                    client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, true, false)
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


client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, SendOnlineData, SendStatusData) ->
    {ok, MacBase} = application:get_env(client, mac_base),

    case SendOnlineData of
        true ->
            OnlineData =  get_online_data(MacBase + ClientId),
            error_logger:info_msg("sending OnlineData: ~p~n", [OnlineData]),
            gen_tcp:send(Socket, OnlineData);
        _ ->
            no_send
    end,

    case SendStatusData of
        true ->
            StatusData =  get_status_data(MacBase + ClientId),
            error_logger:info_msg("sending StatusData: ~p~n", [StatusData]),
            gen_tcp:send(Socket, StatusData);
        _ ->
            no_send
    end,

    case application:get_env(client, run_mode) of
        {ok, test} ->
            error_logger:info_msg("=================================================> pass~n", []),
            init:stop();
        {ok, press} ->
            do_nothing
    end,

    receive
        {tcp, Socket, Msg} -> 
            error_logger:info_msg("received tcp data ~p: ~p~n", [erlang:self(), Msg]),

            handle_data(Socket, Msg),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false, false);

        {tcp_closed, _Socket} ->
            error_logger:info_msg("tcp_closed ~p~n", [erlang:self()]),

            Reason = "tcp_closed",
            reconnect(ServerHost, ServerPort, ClientId, Reason);

        AnyMsg ->
            error_logger:info_msg("received any data ~p: ~p~n", [erlang:self(), AnyMsg]),
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false, false)
            
    after
        DataSendingInterval ->
            client_loop(Socket, ServerHost, ServerPort, ClientId, DataSendingInterval, false, true)
    end.


reconnect(ServerHost, ServerPort, ClientId, Reason) ->
    {ok, ReconnectWaitTime} = application:get_env(client, reconnect_wait_time),
    timer:sleep(ReconnectWaitTime),

    error_logger:info_msg("reconnecting because of ~p ~p~n", [Reason, erlang:self()]),
    start_client(ServerHost, ServerPort, ClientId). %% reconnect


get_online_data(Count) ->
    MacString0 = erlang:integer_to_list(Count, 16),
    MacString = tools:prefix_string(MacString0, 12, "0"),

    Data = [16#41 | MacString],

    erlang:list_to_binary(Data).


get_status_data(_Count) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Temperature = random:uniform(1024),

    Temperature0 = Temperature rem 256,
    Temperature1 = erlang:round((Temperature - Temperature0)/256),
    
    Data = [16#42, Temperature1, Temperature0],

    erlang:list_to_binary(Data).
    

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
