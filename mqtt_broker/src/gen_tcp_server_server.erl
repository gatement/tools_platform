-module(gen_tcp_server_server).
-include("mqtt.hrl").
-behaviour(gen_server).
%% API
-export([start_link/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {lsocket, 
                socket,
                ip, 
                port, 
                callback, 
                parent, 
                user_data, 
                client_id,
                keep_alive_timer}).

-define(GRACE, 10000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Callback, LSocket, UserArgs) ->
    gen_server:start_link(?MODULE, [LSocket, Callback, UserArgs, self()], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([LSocket, Callback, UserArgs, Parent]) ->
    {ok, UserData} = Callback:init(UserArgs),
    State = #state{
        lsocket = LSocket, 
        callback = Callback, 
        parent = Parent, 
        user_data = UserData},

    %error_logger:info_msg("a new gen_tcp_server_server was started.~n"),
    {ok, State, 0}.


handle_call(_Msg, _From, State) ->
    %error_logger:info_msg("gen_tcp_server_server was called: ~p.~n", [_Msg]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    %error_logger:info_msg("gen_tcp_server_server was casted: ~p.~n", [_Msg]),
    {noreply, State}.


handle_info({tcp, _Socket, RawData}, State) ->
    %error_logger:info_msg("received tcp data: ~p~n", [RawData]),
    case dispatch(handle_data, RawData, State) of
        stop ->
            {stop, normal, State};
        State2 ->
            {noreply, State2, State#state.keep_alive_timer}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    %error_logger:info_msg("gen_tcp_server_server was infoed: ~p.~n", [tcp_closed]),
    {stop, normal, State};

handle_info(timeout, #state{lsocket = LSocket, socket = Socket0, parent = Parent} = State) ->    
    case Socket0 of
        undefined ->
            {ok, Socket} = gen_tcp:accept(LSocket),
            gen_tcp_server_connection_sup:start_child(Parent),

            %% log
            {ok, {Address, Port}} = inet:peername(Socket),
            {ok, ConnectionTimeout} = application:get_env(mqtt_broker, connection_timout),
            {noreply, State#state{socket = Socket, ip = Address, port = Port}, ConnectionTimeout}; %% this socket must recieve the first message in Timeout seconds
        Socket ->
            case State#state.client_id of
                undefined ->
                    %% no first package income yet
                    Ip = State#state.ip,
                    Port = State#state.port,
                    {ok, ConnectionTimeout} = application:get_env(mqtt_broker, connection_timout),
                    error_logger:info_msg("disconnected ~p ~p:~p because of the first message doesn't arrive in ~p milliseconds.~n", [Socket, Ip, Port, ConnectionTimeout]),
                    {stop, normal, State}; %% no message arrived in time so suicide
                _ ->
                    %% the remote has no heartbeat so disconnect it
                    DisconnectMsg = mqtt:build_disconnect(),
                    gen_tcp_server:tcp_reply(Socket, DisconnectMsg),

                    Ip = State#state.ip,
                    Port = State#state.port,
                    error_logger:info_msg("disconnected ~p ~p:~p because of the remote has no heartbeat.~n", [Socket, Ip, Port]),
                    {stop, normal, State} %% no message arrived in time so suicide
            end
    end;

handle_info({send_tcp_data, Data}, #state{socket = Socket} = State) ->
    gen_tcp_server:tcp_reply(Socket, Data),
    {noreply, State, State#state.keep_alive_timer};

handle_info(stop, State) ->
    error_logger:info_msg("process ~p was stopped~n", [erlang:self()]),
    {stop, normal, State};
    
handle_info(_Msg, State) ->
    %error_logger:info_msg("gen_tcp_server_server was infoed: ~p.~n", [_Msg]),
    {noreply, State, State#state.keep_alive_timer}.


terminate(_Reason, State) ->
    %error_logger:info_msg("gen_tcp_server_server ~p was terminated with reason: ~p.~n", [State#state.socket, _Reason]),
    dispatch(terminate, State),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

dispatch(handle_data, RawData, State) ->
    State2 = case State#state.client_id of
        undefined ->
            {ClientId, KeepAlivetimer} = extract_connect_info(RawData),
            %error_logger:info_msg("mqtt client get online(~p), KeepAlivetimer = ~p seconds.~n", [ClientId, KeepAlivetimer]),
            State#state{client_id = ClientId, keep_alive_timer = KeepAlivetimer * 1000 + ?GRACE};
        _ ->
            State
    end,

    case filter_packages(State#state.socket, RawData) of
        stop ->
            stop;
        <<>> ->
            State2;

        RestData ->
            #state{socket = Socket, callback = Callback, user_data = UserData, client_id = ClientId2} = State2,
            SourcePid = erlang:self(),
            Callback:handle_data(SourcePid, Socket, RestData, UserData, ClientId2),

            State2
    end.


dispatch(terminate, State) ->
    #state{callback = Callback, user_data = UserData, client_id = ClientId} = State,
    SourcePid = erlang:self(),
    Callback:terminate(SourcePid, UserData, ClientId),
    ok.


filter_packages(Socket, RawData) -> 
    <<TypeCode:8/integer, _/binary>> = RawData,
    TypeCode2 = TypeCode bsr 4,
    case TypeCode2 of
        ?PINGREQ ->
            PingRespData = mqtt:build_pingresp(),
            error_logger:info_msg("[~p] is sending PINGRESP: ~p~n", [?MODULE, PingRespData]),
            gen_tcp:send(Socket, PingRespData),
            <<_:2/binary, RestData/binary>> = RawData,
            RestData;

        ?DISCONNECT ->
            stop;

        _ ->
            RawData
    end.


extract_connect_info(Data) ->
    RestData = mqtt_utils:strip_fixed_header(Data),

    <<_:10/binary, KeepAliveTimerH:8/integer, KeepAliveTimerL:8/integer, RestData2/binary>> = RestData,
    KeepAliveTimer = KeepAliveTimerH * 256 + KeepAliveTimerL,

    <<_:2/binary, ClientId0/binary>> = RestData2,
    ClientId = erlang:binary_to_list(ClientId0),

    {ClientId, KeepAliveTimer}.
