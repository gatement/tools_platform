-module(handler_tcp_device).
-include("tools_platform.hrl").
-behaviour(gen_tcp_server).
%% gen_tcp_server callbacks
-export([init/1, handle_data/5, terminate/2]).


%% ===================================================================
%% gen_tcp_server callbacks
%% ===================================================================

init([]) ->    
    {ok, device}.


handle_data(SourcePid, Socket, RawData, _UserData, Sn) -> 
    %error_logger:info_msg("~p received tcp data [~p]: ~p~n", [?MODULE, Sn, RawData]),
    try
        handle_data_inner(SourcePid, Socket, RawData, Sn)
    catch
        _:_ ->
            error_logger:info_msg("send stop signal to ~p because of handle_data exception.~n", [SourcePid]),
            SourcePid ! stop
    end.


terminate(_UserData, Sn) ->
    Pid = erlang:self(),
    model_dev_session:delete_by_pid(Pid),
    error_logger:info_msg("deleted deviceSession by pid: ~p~n", [Pid]), 

    socket_device:device_offline_notification(Sn),

    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

handle_data_inner(_SourcePid, _Socket, <<>>, _Sn) ->
    ok;
handle_data_inner(SourcePid, Socket, RawData, Sn) ->
    <<TypeCode:1/binary, _/binary>> = RawData,

    case TypeCode of
        <<16#A1>> ->
            <<Data:7/binary, RestRawData/binary>> = RawData,
            process_data_online(SourcePid, Socket, Data, Sn);
        <<16#A2>> ->
            <<Data:8/binary, RestRawData/binary>> = RawData,
            process_data_voltage(SourcePid, Socket, Data, Sn)
    end,

    handle_data_inner(SourcePid, Socket, RestRawData, Sn).


process_data_online(SourcePid, Socket, _Data, Sn) ->
    error_logger:info_msg("process_data_online(~p): ~p~n", [Sn, SourcePid]),

    %% check if device exist, if not, create it
    case model_dev_device:get_by_sn(Sn) of
        error ->
            erlang:exit(error);
        [] ->
            model_dev_device:create(#dev_device{
                id = uuid:to_string(uuid:uuid1()), 
                sn = Sn,
                name = "Device1",
                created = tools:datetime_string('yyyyMMdd_hhmmss')
            });
        _ ->
            do_nothing
    end,

    %% kick off the old TCP session with the same sn
    case model_dev_session:get_by_sn(Sn) of
        [] ->
            do_nothing;
        Models ->
            [X#dev_session.pid ! stop || X <- Models]
    end,

    model_dev_session:delete_by_sn(Sn),

    model_dev_session:create(#dev_session{
        id = uuid:to_string(uuid:uuid1()), 
        sn = Sn, 
        pid = SourcePid, 
        created = tools:datetime_string('yyyyMMdd_hhmmss')
    }),

    %% log the ip
    {ok, {Address, _Port}} = inet:peername(Socket),
    model_dev_status:update(Sn, ip_address, Address),

    %% push status to clients
    socket_device:device_status_changed_notification(Sn),

    ok.


process_data_voltage(_SourcePid, _Socket, Data, Sn) ->
    Voltage = extract_voltage(Data),
    error_logger:info_msg("process_data_voltage(~p): ~p~n", [Sn, Voltage]),

    %% save it to status table and as history data
    model_dev_status:update(Sn, voltage, Voltage),
    model_dev_data:create(#dev_data{
        id = uuid:to_string(uuid:uuid1()), 
        sn = Sn, 
        key = voltage, 
        value = Voltage, 
        datetime = tools:datetime_string('yyyyMMdd_hhmmss')
    }),

    %% push status to clients
    socket_device:device_status_changed_notification(Sn),

    ok.


extract_voltage(Data) ->
    <<_:7/binary, Voltage:8/integer>> = Data,
    Voltage.
