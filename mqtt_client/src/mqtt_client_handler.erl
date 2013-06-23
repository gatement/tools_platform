-module(mqtt_client_handler).
-include("../../platform_core/include/tools_platform.hrl").
-export([process_data_publish/3]).


%% ===================================================================
%% API functions
%% ===================================================================

process_data_publish(_SourcePid, _Socket, RawData) ->
    %error_logger:info_msg("~p received data: ~p~n", [?MODULE, RawData]),
    {Topic, Payload} = mqtt_utils:extract_publish_msg(RawData),
    ClientId = string:substr(Topic, 2, 12),
    <<TypeCode:8/integer, _/binary>> = Payload,

    case TypeCode of
    	1 ->
    		process_data_online(ClientId);
    	2 ->
    		process_data_offline(ClientId);
    	3 ->
    		<<_:1/binary, SwitchStatus:8/integer, _/binary>> = Payload,
    		process_data_switch_status(ClientId, SwitchStatus)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

process_data_online(ClientId) ->
	error_logger:info_msg("~p received [online] data: ~p~n", [?MODULE, ClientId]),

    %% check if device exist, if not, create it
    case model_dev_device:get(ClientId) of
        undefined ->
        	{DeviceType, DeviceName} = get_device_info(ClientId),

            model_dev_device:create(#dev_device{
                device_id = ClientId,
                user_id = "admin",
                name = DeviceName,
                type = DeviceType,
                created = tools:datetime_string('yyyyMMdd_hhmmss')
            });
        error ->
            erlang:exit(error);
        _Model ->
            do_nothing
    end,

	model_dev_status:update(ClientId, "online", true),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


process_data_offline(ClientId) ->
    error_logger:info_msg("~p received [offline] data: ~p~n", [?MODULE, ClientId]),

    model_dev_status:update(ClientId, "online", false),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


process_data_switch_status(ClientId, SwitchStatus) ->
    error_logger:info_msg("~p received [switch status] data: ~p - ~p~n", [?MODULE, ClientId, SwitchStatus]),

    Switch1 = if
        (SwitchStatus band 2#00000001) =:= 1 ->
            "on";
        true ->
            "off"
    end,

    model_dev_status:update(ClientId, "switch1", Switch1),

    %% push status to clients
    socket_device:device_status_changed_notification(ClientId),

    ok.


get_device_info(ClientId) ->
    case ClientId of
        "000000000001" ->
            {"main_mqtt_client", "main_mqtt_client"};
        "000000000002" ->
            {"controller", "controller"};
        "000000000003" ->
            {"computer", "linux server"};
        "000000000004" ->
            {"controller", "windows pc"};
        _ ->
            {"undefined", "undefined"}
    end.
