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

    model_usr_session:update_last_active(SessionId),

    Response = case model_usr_session:get(SessionId) of
        error ->
            [{"success", false}, {"data", "Bad session, please re-login."}];
        [] ->
            [{"success", false}, {"data", "Bad session, please re-login."}];
        [UserSession] ->
            UserId = UserSession#usr_session.user_id,
            Function = erlang:list_to_atom(Cmd),
            erlang:apply(?MODULE, Function, [Data, UserId, UserSession])
    end,

    %io:format("Function result: ~p~n", [Response]),
    Reply = json2:encode({struct, [{"cmd", Cmd}] ++ Response}),
    %io:format("Socket reply: ~p~n", [Reply]),

    {reply, {text, erlang:list_to_binary(Reply)}}.


%% ===================================================================
%% Send data to client sockets
%% ===================================================================

device_status_changed_notification(DeviceId) ->
io:format("~p~n", [1]),
    Device = model_dev_device:get(DeviceId),
io:format("~p~n", [Device]),
    Values = model_dev_status:get_all_values_by_deviceId(DeviceId),
io:format("~p~n", [3]),

    Msg0 = {struct, [
            {"cmd", "device_status_changed"}, 
            {"success", true},
            {"data", {struct, [
                {"device_id", DeviceId}, 
                {"type", Device#dev_device.type}, 
                {"name", Device#dev_device.name}, 
                {"values", {array, Values}}
            ]}}
        ]},
io:format("~p~n", [Msg0]),
    Msg = json2:encode(Msg0),
    notice(Msg).


notice(Msg) ->
    Pids = model_usr_session:get_socket_pid_by_socket_territory("/device"),
    [yaws_api:websocket_send(Pid, {text, erlang:list_to_binary(Msg)}) || Pid <- Pids].


%% ===================================================================
%% Request handlers
%% ===================================================================

update_socket(_Data, _UserId, UserSession) ->
    model_usr_session:update_socket(UserSession#usr_session.id, erlang:self(), "/device"),
    [{"success", true}, {"data", "ok."}].


list_online_devices(_Data, _UserId, _UserSession) ->
    DeviceIds = model_dev_status:get_online_device_ids(),

    Fun = fun(DeviceId) ->
        Device = model_dev_device:get(DeviceId),
        [Values] = model_dev_status:get_all_values_by_deviceId(DeviceId),

        {struct, [
                {"device_id", DeviceId}, 
                {"type", Device#dev_device.type}, 
                {"name", Device#dev_device.name}, 
                {"values", {array, Values}}
        ]}
    end,

    Devices = [ Fun(X) || X <- DeviceIds,
                            X =/= "000000000001"],

    [{"success", true}, {"data", {array, Devices}}].


update_switch_status(Data, _UserId, _UserSession) ->
    {struct,[{"device_id", Device},{"switch_id", SwitchId},{"status", Status}]} = Data,

    io:format("~ndevice-id-switch_id-status: ~p-~p-~p~n~n", [Device, SwitchId, Status]),
    [{"success", true}, {"data", "ok."}].


%% ===================================================================
%% Local Functions
%% ===================================================================
