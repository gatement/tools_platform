-module(mqtt_client_handler).
-include("../../platform_core/include/tools_platform.hrl").
-include("../../mqtt_broker/include/mqtt.hrl").
-export([process_data_publish/3]).


%% ===================================================================
%% API functions
%% ===================================================================

process_data_publish(_SourcePid, _Socket, RawData) ->
    %error_logger:info_msg("[~p] received data: ~p~n", [?MODULE, RawData]),
    {Topic, Payload} = mqtt_utils:extract_publish_info(RawData),
    ClientId = string:substr(Topic, 2, 12),
    <<TypeCode:8/integer, _/binary>> = Payload,

    case TypeCode of
        ?CMD_ONLINE ->
            <<_:3/binary, UserName/binary>> = Payload,
            process_data_online(ClientId, erlang:binary_to_list(UserName));

        ?CMD_OFFLINE ->
            process_data_offline(ClientId);

        ?CMD_UPTIME ->
            <<_:1/binary, Uptime4:8/integer, Uptime3:8/integer, Uptime2:8/integer, Uptime1:8/integer, _/binary>> = Payload,
            Uptime = Uptime4 * 16777216 + Uptime3 * 65536 + Uptime2 * 256 + Uptime1,
            process_data_uptime(ClientId, Uptime);

        ?CMD_SWITCH_STATUS ->
            <<_:1/binary, SwitchStatus:8/integer, _/binary>> = Payload,
            process_data_switch_status(ClientId, SwitchStatus)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

process_data_online(ClientId, UserName) ->
    %error_logger:info_msg("[~p] received [online] data: ~p~n", [?MODULE, ClientId]),

    %% check if device exist, if not, create it
    case model_dev_device:get(ClientId) of
        undefined ->
            model_dev_device:create(#dev_device{
                device_id = ClientId,
                user_id = UserName,
                name = "undefined",
                type = undefined,
                created = tools:datetime_string('yyyyMMdd_hhmmss')
            });
        error ->
            erlang:exit(error);
        _Model ->
            model_dev_device:update_user_id(ClientId, UserName)
    end,

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


process_data_offline(ClientId) ->
    %error_logger:info_msg("[~p] received [offline] data: ~p~n", [?MODULE, ClientId]),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


process_data_uptime(ClientId, Uptime) ->
    %% convert it to minutes
    Uptime2 = Uptime div 60,

    %error_logger:info_msg("[~p] received [uptime] data: ~p - ~p minutes~n", [?MODULE, ClientId, Uptime2]),

    model_dev_status:update(ClientId, "uptime", Uptime2),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


process_data_switch_status(ClientId, SwitchStatus) ->
    %error_logger:info_msg("[~p] received [switch status] data: ~p - ~p~n", [?MODULE, ClientId, SwitchStatus]),

    Switch1 = if
        (SwitchStatus band 2#00000001) =:= 1 ->
            "on";
        true ->
            "off"
    end,
    Switch2 = if
        (SwitchStatus band 2#00000010) =:= 2 ->
            "on";
        true ->
            "off"
    end,
    Switch3 = if
        (SwitchStatus band 2#00000100) =:= 4 ->
            "on";
        true ->
            "off"
    end,

    model_dev_status:update(ClientId, "switch1", Switch1),
    model_dev_status:update(ClientId, "switch2", Switch2),
    model_dev_status:update(ClientId, "switch3", Switch3),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.
