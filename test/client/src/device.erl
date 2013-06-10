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
    Mac6 = Count div (256 * 256 * 256 * 256 * 256),
    Mac5 = (Count rem (256 * 256 * 256 * 256 * 256)) div (256 * 256 * 256 * 256),
    Mac4 = (Count rem (256 * 256 * 256 * 256)) div (256 * 256 * 256),
    Mac3 = (Count rem (256 * 256 * 256)) div (256 * 256),
    Mac2 = (Count rem (256 * 256)) div 256,
    Mac1 = Count rem 256,

    Data = [16#A1,Mac6,Mac5,Mac4,Mac3,Mac2,Mac1],

    erlang:list_to_binary(Data).


get_status_data(Count) ->
    Mac6 = Count div (256 * 256 * 256 * 256 * 256),
    Mac5 = (Count rem (256 * 256 * 256 * 256 * 256)) div (256 * 256 * 256 * 256),
    Mac4 = (Count rem (256 * 256 * 256 * 256)) div (256 * 256 * 256),
    Mac3 = (Count rem (256 * 256 * 256)) div (256 * 256),
    Mac2 = (Count rem (256 * 256)) div 256,
    Mac1 = Count rem 256,

    Temperature = erlang:round(random:uniform() * 100),

    Data = [16#A2,Mac6,Mac5,Mac4,Mac3,Mac2,Mac1,Temperature],

    erlang:list_to_binary(Data).
    