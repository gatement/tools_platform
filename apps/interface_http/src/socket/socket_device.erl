-module(socket_device).
-include("../../platform_core/include/tools_platform.hrl").
-compile(export_all).


%% ===================================================================
%% API functions
%% ===================================================================

handle_message({close, _, _}) ->
    model_usr_session:clear_socket(erlang:self());

handle_message({text, Request}) ->
    Text = erlang:binary_to_list(Request),
    %io:format("Socket receive: ~p~n", [json2:decode_string(Text)]),

    {ok, {struct, [{"cmd", Cmd}, {"sid", SessionId}, {"data", Data}]}} = json2:decode_string(Text),

    Response = case model_usr_session:get(SessionId) of
        error ->
            [{"success", false}, {"data", "Bad session, please re-login."}];
        [] ->
            [{"success", false}, {"data", "Bad session, please re-login."}];
        [UserSession] ->
            UserId = UserSession#usr_session.user_id,
            DeviceEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_DEVICE_ENABLED),
            case DeviceEnabled of
                true ->
                    model_usr_session:update_last_active(SessionId),
                    Function = erlang:list_to_atom(Cmd),
                    erlang:apply(?MODULE, Function, [Data, UserId, UserSession]);

                false -> 
                    [{"success", false}, {"data", "user has no device subscription."}]
            end
    end,

    %io:format("Function result: ~p~n", [Response]),
    Reply = json2:encode({struct, [{"cmd", Cmd}] ++ Response}),
    %io:format("Socket reply: ~p~n", [Reply]),

    {reply, {text, erlang:list_to_binary(Reply)}}.


%% ===================================================================
%% Send data to client sockets
%% ===================================================================

device_status_changed_notification(DeviceId) ->
    Device = model_dev_device:get(DeviceId),
    Values = model_dev_status:get_all_values_by_deviceId(DeviceId),

    Msg = json2:encode({struct, [
            {"cmd", "device_status_changed"}, 
            {"success", true},
            {"data", {struct, [
                {"device_id", DeviceId}, 
                {"type", Device#dev_device.type}, 
                {"name", Device#dev_device.name},
                {"online", model_mqtt_session:is_online(DeviceId)},
                {"permission", "shared"},
                {"values", {struct, Values}}
            ]}}
        ]}),

    notice(DeviceId, Msg).


notice(DeviceId, Msg) ->
    UserPids = model_usr_session:get_socket_pid_by_socket_territory("/device"),
    Pids = [Pids || {UserId, Pids} <- UserPids,
                    model_dev_device_user:is_have_permission(DeviceId, UserId) =:= true],
    [yaws_api:websocket_send(Pid, {text, erlang:list_to_binary(Msg)}) || Pid <- Pids].


%% ===================================================================
%% Request handlers
%% ===================================================================

update_socket(_Data, _UserId, UserSession) ->
    model_usr_session:update_socket(UserSession#usr_session.id, erlang:self(), "/device"),
    [{"success", true}, {"data", "ok."}].


list_devices(_Data, UserId, _UserSession) ->
	rest_tool_device:list_devices(UserId).


update_switch_status(Data, UserId, _UserSession) ->
	rest_tool_device:update_switch_status(Data, UserId).


send_command(Data, UserId, _UserSession) ->
	rest_tool_device:send_command(Data, UserId).


load_permissions(Data, UserId, _UserSession) ->
    {struct,[{"device_id", DeviceId}]} = Data,
    Users = model_dev_device_user:list(DeviceId, UserId),
    [{"success", true}, {"data", {array, Users}}].


add_permission(Data, UserId, _UserSession) ->
    {struct,[{"device_id", DeviceId},{"user_id", UserIdToAdd}]} = Data,
    case model_dev_device_user:create(#dev_device_user{
            id = uuid:to_string(uuid:uuid1()), 
            device_id = DeviceId, 
            user_id = UserIdToAdd
        }, UserId) of
        ok ->
            [{"success", true}, {"data", "ok."}];
        user_not_exist ->
            [{"success", false}, {"data", "user id not exist."}];
        error ->
            [{"success", false}, {"data", "error."}]
    end.


delete_permission(Data, UserId, _UserSession) ->
    {struct,[{"id", Id}]} = Data,
    case model_dev_device_user:delete(Id, UserId) of
        ok ->
            [{"success", true}, {"data", "ok."}];
        error ->
            [{"success", false}, {"data", "error."}]
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
