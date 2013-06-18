-module(mqtt_broker_handler).
-include("../platform_core/include/tools_platform.hrl").
-include("mqtt.hrl").
-behaviour(gen_tcp_server).
%% gen_tcp_server callbacks
-export([init/1, handle_data/5, terminate/3]).


%% ===================================================================
%% gen_tcp_server callbacks
%% ===================================================================

init(_) ->    
    {ok, undefined}.


handle_data(SourcePid, Socket, RawData, _UserData, ClientId) -> 
    %error_logger:info_msg("~p received tcp data [~p]: ~p~n", [?MODULE, ClientId, RawData]),
    try
        handle_data_inner(SourcePid, Socket, RawData, ClientId)
    catch
        _:_ ->
            error_logger:info_msg("send stop signal to ~p because of handle_data exception.~n", [SourcePid]),
            SourcePid ! stop
    end.


terminate(SourcePid, _UserData, _ClientId) ->
    model_mqtt_session:delete_by_pid(SourcePid),
    error_logger:info_msg("deleted mqtt session by pid: ~p~n", [SourcePid]), 

    %% TODO: pubish a offline notice to subscriber

    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

handle_data_inner(_, _, <<>>, _) ->
    ok;
handle_data_inner(SourcePid, Socket, RawData, ClientId) ->
    <<TypeCode:4/integer, _:4/integer, _/binary>> = RawData,
    RestRawData = case TypeCode of
        ?CONNECT -> 
            {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
            Data = binary:part(RawData, 0, FixedLength + RestLength), 
            process_data_online(SourcePid, Socket, Data, ClientId),
            binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength);
        ?PUBLISH ->
            {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
            Data = binary:part(RawData, 0, FixedLength + RestLength),         
            process_data_publish(SourcePid, Socket, Data, ClientId),
            binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength)
    end,
        
    handle_data_inner(SourcePid, Socket, RestRawData, ClientId).


process_data_online(SourcePid, _Socket, _Data, ClientId) ->
    error_logger:info_msg("process_data_online(~p): ~p~n", [ClientId, SourcePid]),
       
    %% kick off the old session with the same ClientId
    case model_mqtt_session:get(ClientId) of
        undefined ->
            do_nothing;
        Model ->
            Model#mqtt_session.pid ! stop
    end,
     
    model_mqtt_session:delete(ClientId),
      
    model_mqtt_session:create(#mqtt_session{
        client_id = ClientId, 
        pid = SourcePid, 
        created = tools:datetime_string('yyyyMMdd_hhmmss')
    }),
       
    %% send CONNACK
    ConnackData = mqtt:build_connack(0),
    SourcePid ! {send_tcp_data, ConnackData},
       
    %% TODO: publish client online(including IP) notice to subscribers
    %{ok, {Address, _Port}} = inet:peername(Socket),

    ok.


process_data_publish(SourcePid, _Socket, Data, ClientId) ->
    {Topic, Payload} = mqtt_utils:extract_publish_msg(Data),
    error_logger:info_msg("process_data_publish(~p) - ~p, Topic: ~p, Payload: ~p~n", [ClientId, SourcePid, Topic, Payload]),    

    %% TODO: publish it to subscribers

    ok.
