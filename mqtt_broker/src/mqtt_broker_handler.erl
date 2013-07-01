-module(mqtt_broker_handler).
-include("../platform_core/include/tools_platform.hrl").
-include("mqtt.hrl").
-behaviour(gen_tcp_server).
%% gen_tcp_server callbacks
-export([init/1, 
    process_data_online/4, 
    process_data_publish/4, 
    terminate/4]).


%% ===================================================================
%% gen_tcp_server callbacks
%% ===================================================================

init(_) ->    
    {ok, undefined}.


process_data_online(SourcePid, _Socket, Data, ClientId) ->
    {_, _, UserName, Password} = mqtt_utils:extract_connect_info(Data),
    %error_logger:info_msg("[~p] process_data_online(~p): ~p, username: ~p, password: ~p~n", [?MODULE, ClientId, SourcePid, UserName, Password]),

    case {UserName, Password} of
        {undefined, _} ->
            send_connack(SourcePid, ?NOT_AUTHORIZED),
            SourcePid ! {stop, bad_connect};

        {_, undefined} ->
            send_connack(SourcePid, ?NOT_AUTHORIZED),
            SourcePid ! {stop, bad_connect};
        _ ->
            case model_usr_user:get(UserName, Password) of        
                [] ->
                    send_connack(SourcePid, ?BAD_USERNAME_OR_PASSWORD),
                    SourcePid ! {stop, bad_connect};

                [User] when User#usr_user.enabled /= true ->
                    send_connack(SourcePid, ?IDENTIFIER_REJECTED),
                    SourcePid ! {stop, bad_connect};

                [_User] ->
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
                    send_connack(SourcePid, ?ACCEPTED),

                    %% publish client online(including IP) notice to subscribers
                    PublishData = mqtt_cmd:online(ClientId, UserName),
                    mqtt_broker:publish(ClientId, "#", "000000000000", "", PublishData),

                    %% subscribe any PUBLISH starts with "/ClientId/"  
                    subscribe_any_publish_to_me(ClientId)        
            end
    end,

    ok.


process_data_publish(_SourcePid, _Socket, Data, ClientId) ->
    {Topic, _Payload} = mqtt_utils:extract_publish_msg(Data),
    %error_logger:info_msg("[~p] process_data_publish(~p) topic: ~p, payload: ~p~n", [?MODULE, ClientId, Topic, _Payload]),    

    %% publish it to subscribers
    mqtt_broker:publish(ClientId, Topic, "000000000000", "", Data),

    ok.


terminate(SourcePid, Socket, ClientId, Reason) ->
    model_mqtt_session:delete_by_pid(SourcePid),
    %error_logger:info_msg("[~p] deleted mqtt session by pid: ~p~n", [?MODULE, SourcePid]), 

    {SendDisconnect, PublishOffline} = case Reason of
        %% no data come in after TCP was created
        connection_timout -> {false, false};
        %% user/pwd is missing or is bad
        bad_connect -> {false, false};
        %% did not receive client heartbeat within keep_alive_timer
        no_heartbeat -> {true, true};
        %% receive DISCONNECT msg
        disconnected -> {false, true};
        %% the TCP connection was closed
        tcp_closed -> {false, true}
    end,

    %% send DISCONNECT
    case SendDisconnect of
        false ->
            do_nothing;
        true ->
            DisconnectMsg = mqtt:build_disconnect(),
            gen_tcp:send(Socket, DisconnectMsg)
    end,

    %% pubish a offline notice to subscriber
    case PublishOffline of
        false ->
            do_nothing;
        true ->
            PublishData = mqtt_cmd:offline(ClientId),
            mqtt_broker:publish(ClientId, "#", "000000000000", "", PublishData)
    end,

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
                    client_id = ClientId, 
                    topic = Topic,
                    qos = 0,
                    desc = "Publish to me"
            })
    end.


send_connack(SourcePid, Result) ->
    ConnackData = mqtt:build_connack(Result),
    SourcePid ! {send_tcp_data, ConnackData}.
