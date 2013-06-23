-module(mqtt_broker_handler).
-include("../platform_core/include/tools_platform.hrl").
-include("mqtt.hrl").
-behaviour(gen_tcp_server).
%% gen_tcp_server callbacks
-export([init/1, 
    process_data_online/4, 
    process_data_publish/4, 
    terminate/3]).


%% ===================================================================
%% gen_tcp_server callbacks
%% ===================================================================

init(_) ->    
    {ok, undefined}.


process_data_online(SourcePid, _Socket, _Data, ClientId) ->
    %error_logger:info_msg("process_data_online(~p): ~p~n", [ClientId, SourcePid]),

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

    %% publish client online(including IP) notice to subscribers
    PublishData = mqtt_cmd:online(ClientId),
    mqtt_broker:publish(ClientId, "#", PublishData),

    %% subscribe any PUBLISH starts with "/ClientId/"  
    subscribe_any_publish_to_me(ClientId),

    ok.


process_data_publish(_SourcePid, _Socket, Data, ClientId) ->
    {Topic, _Payload} = mqtt_utils:extract_publish_msg(Data),
    %error_logger:info_msg("process_data_publish(~p) - ~p, Topic: ~p, Payload: ~p~n", [ClientId, _SourcePid, Topic, _Payload]),    

    %% publish it to subscribers
    mqtt_broker:publish(ClientId, Topic, Data),

    ok.


terminate(SourcePid, Socket, ClientId) ->
    %% send DISCONNECT
    DisconnectMsg = mqtt:build_disconnect(),
    gen_tcp:send(Socket, DisconnectMsg),

    model_mqtt_session:delete_by_pid(SourcePid),
    %error_logger:info_msg("deleted mqtt session by pid: ~p~n", [SourcePid]), 

    %% pubish a offline notice to subscriber
    PublishData = mqtt_cmd:offline(ClientId),
    mqtt_broker:publish(ClientId, "#", PublishData),

    ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

subscribe_any_publish_to_me(ClientId) ->
    Topic = lists:flatten(io_lib:format("/~s/+", [ClientId])),

    case model_mqtt_subscription:exist(ClientId, Topic) of
        true ->
            do_nothing;

        false ->
            model_mqtt_subscription:create(#mqtt_subscription{
                    id = uuid:to_string(uuid:uuid1()), 
                    name = "Publish to me",
                    client_id = ClientId, 
                    topic = Topic,
                    qos = 0
            })
    end.