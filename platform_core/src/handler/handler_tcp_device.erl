-module(handler_tcp_device).
-include("tools_platform.hrl").
-behaviour(gen_tcp_server).
%% gen_tcp_server callbacks
-export([init/1, handle_data/5, terminate/2, get_hearbeat_data/0]).


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


get_hearbeat_data() ->
    <<$#, $0, $#>>.


%% ===================================================================
%% Local Functions
%% ===================================================================

handle_data_inner(_SourcePid, _Socket, <<>>, _Sn) ->
    ok;
handle_data_inner(SourcePid, Socket, RawData, Sn) ->
    <<TypeCode:1/binary, _/binary>> = RawData,

    case TypeCode of
        <<16#41>> ->
            <<Data:13/binary, RestRawData/binary>> = RawData,
            process_data_online(SourcePid, Socket, Data, Sn);
        <<16#42>> ->
            <<Data:3/binary, RestRawData/binary>> = RawData,
            process_data_voltage(SourcePid, Socket, Data, Sn);
        <<16#43>> ->
            <<Data:2/binary, RestRawData/binary>> = RawData,
            process_data_led1(SourcePid, Socket, Data, Sn);
        <<16#44>> ->
            %% device heartbeat response
            <<Data:3/binary, RestRawData/binary>> = RawData,
            process_data_heatbeat(SourcePid, Socket, Data, Sn)
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
                name = "DEV" ++ Sn,
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


process_data_voltage(SourcePid, _Socket, Data, Sn) ->
    Voltage = extract_voltage(Data),
    error_logger:info_msg("process_data_voltage(~p): ~p : ~p~n", [Sn, SourcePid, Voltage]),

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


process_data_led1(SourcePid, _Socket, Data, Sn) ->
    Led1 = extract_led1(Data),
    error_logger:info_msg("process_data_led1(~p): ~p : ~p~n", [Sn, SourcePid, Led1]),

    %% save it to status table and as history data
    model_dev_status:update(Sn, led1, Led1),
    model_dev_data:create(#dev_data{
        id = uuid:to_string(uuid:uuid1()), 
        sn = Sn, 
        key = led1, 
        value = Led1, 
        datetime = tools:datetime_string('yyyyMMdd_hhmmss')
    }),

    %% push status to clients
    socket_device:device_status_changed_notification(Sn),

    ok.


process_data_heatbeat(SourcePid, _Socket, _Data, Sn) ->
    error_logger:info_msg("process_data_heatbeat(~p): ~p~n", [Sn, SourcePid]),

    ok.


extract_voltage(Data) ->
    <<_:1/binary, Voltage:16/integer>> = Data,
    Voltage.


extract_led1(Data) ->
    <<_:1/binary, Led1:8/integer>> = Data,
    Led1.
