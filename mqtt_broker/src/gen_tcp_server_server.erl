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
    dispatch(handle_data, RawData, State);

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

    handle_packages(State2, RawData).


dispatch(terminate, State) ->
    #state{callback = Callback, socket = Socket, client_id = ClientId} = State,
    Callback:terminate(erlang:self(), Socket, ClientId),
    ok.


handle_packages(State, <<>>) ->
    {noreply, State, State#state.keep_alive_timer};

handle_packages(State, RawData) ->
    #state{
        socket = Socket, 
        callback = Callback, 
        client_id = ClientId} = State,

    {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
    Data = binary:part(RawData, 0, FixedLength + RestLength), 
    <<TypeCode:4/integer, _:4/integer, _/binary>> = Data,

    Result = case TypeCode of
        ?PINGREQ ->
            PingRespData = mqtt:build_pingresp(),
            error_logger:info_msg("[~p] is sending PINGRESP: ~p~n", [?MODULE, PingRespData]),
            gen_tcp:send(Socket, PingRespData),
            ok;

        ?DISCONNECT ->
            stop;
        
        ?CONNECT -> 
            Callback:process_data_online(erlang:self(), Socket, Data, ClientId),
            ok;
            
        ?PUBLISH ->     
            Callback:process_data_publish(erlang:self(), Socket, Data, ClientId),
            ok
    end,

    case Result of
        stop ->
            {stop, normal, State};
        ok ->
            RestRawData = binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength),
            handle_packages(State, RestRawData)
    end.


extract_connect_info(Data) ->
    RestData = mqtt_utils:strip_fixed_header(Data),

    <<_:10/binary, KeepAliveTimerH:8/integer, KeepAliveTimerL:8/integer, RestData2/binary>> = RestData,
    KeepAliveTimer = KeepAliveTimerH * 256 + KeepAliveTimerL,

    <<_:2/binary, ClientId0/binary>> = RestData2,
    ClientId = erlang:binary_to_list(ClientId0),

    {ClientId, KeepAliveTimer}.
