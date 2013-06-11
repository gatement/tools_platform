-module(socket_device).
-include("../../core/include/tools_platform.hrl").
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

device_status_changed_notification(Sn) ->
    [Device] = model_dev_device:get_by_sn(Sn),
    IsOnline = model_dev_session:is_online(Sn),
    Voltage = case IsOnline of
        false ->
            0;
        true ->
            case model_dev_status:get_by_key(Sn, voltage) of
                undefined -> 0;
                Vol -> Vol
            end
    end,

    Msg = json2:encode({struct, [
            {"cmd", "device_status_changed"}, 
            {"success", true}, 
            {"data", {struct, [
                {"sn", Sn}, 
                {"name", Device#dev_device.name}, 
                {"is_online", IsOnline}, 
                {"voltage", Voltage}
            ]}}
        ]}),
    notice(Msg).


device_offline_notification(Sn) ->
    Msg = json2:encode({struct, [
            {"cmd", "device_status_changed"}, 
            {"success", true}, 
            {"data", {struct, [
                {"sn", Sn}, 
                {"is_online", false}
            ]}}
        ]}),
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
    DeviceSessionIds = model_dev_session:all_keys(),

    Fun = fun(SessionId) ->
        DeviceSession = model_dev_session:get(SessionId),
        Sn = DeviceSession#dev_session.sn,
        [Device] = model_dev_device:get_by_sn(Sn),
        Voltage = case model_dev_status:get_by_key(Sn, voltage) of
            undefined -> 0;
            Vol -> Vol
        end,
        {struct, [
                    {"sn", Sn}, 
                    {"name", Device#dev_device.name}, 
                    {"is_online", true}, 
                    {"voltage", Voltage}
                ]}
    end,

    Devices = [ Fun(X) || X <- DeviceSessionIds],

   [{"success", true}, {"data", {array, Devices}}].


%% ===================================================================
%% Local Functions
%% ===================================================================
