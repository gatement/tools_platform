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

    %error_logger:info_msg("[~p] was started.~n", [?MODULE]),
    {ok, State, 0}.


handle_call(_Msg, _From, State) ->
    %error_logger:info_msg("[~p] was called: ~p.~n", [?MODULE, _Msg]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    %error_logger:info_msg("[~P] was casted: ~p.~n", [?MODULE, _Msg]),
    {noreply, State}.


handle_info({tcp, _Socket, RawData}, State) ->
    %error_logger:info_msg("[~p] received tcp data: ~p~n", [?MODULE, RawData]),
    dispatch(handle_data, RawData, State);

handle_info({tcp_closed, _Socket}, State) ->
    %error_logger:info_msg("[~p] was infoed: ~p.~n", [?MODULE, tcp_closed]),
    {stop, tcp_closed, State};

handle_info(timeout, #state{lsocket = LSocket, socket = Socket0, parent = Parent} = State) ->    
    case Socket0 of
        undefined ->
            {ok, Socket} = gen_tcp:accept(LSocket),
            gen_tcp_server_connection_sup:start_child(Parent),

            %% log
            {ok, {Address, Port}} = inet:peername(Socket),
            {ok, ConnectionTimeout} = application:get_env(connection_timout),
            {noreply, State#state{socket = Socket, ip = Address, port = Port}, ConnectionTimeout}; %% this socket must recieve the first message in Timeout seconds
        _Socket ->
            case State#state.client_id of
                undefined ->
                    %% no first package income yet
                    _Ip = State#state.ip,
                    _Port = State#state.port,
                    {ok, _ConnectionTimeout} = application:get_env(connection_timout),
                    %error_logger:info_msg("disconnected ~p ~p:~p because of the first message doesn't arrive in ~p milliseconds.~n", [_Socket, _Ip, _Port, _ConnectionTimeout]),
                    {stop, connection_timout, State}; %% no message arrived in time so suicide
                _ ->
                    _Ip = State#state.ip,
                    _Port = State#state.port,
                    %error_logger:info_msg("disconnected ~p ~p:~p because of the remote has no heartbeat.~n", [_Socket, _Ip, _Port]),
                    {stop, no_heartbeat, State} %% no message arrived in time so suicide
            end
    end;

handle_info({send_tcp_data, Data}, #state{socket = Socket} = State) ->
    %error_logger:info_msg("[~p] send to socket ~p with data: ~p~n", [?MODULE, Socket, Data]),
    gen_tcp:send(Socket, Data),
    {noreply, State, State#state.keep_alive_timer};

handle_info({stop, Reason}, State) ->
    %error_logger:info_msg("[~p] process ~p was stopped: ~p~n", [?MODULE, erlang:self(), Reason]),
    {stop, Reason, State};
    
handle_info(_Msg, State) ->
    %error_logger:info_msg("[~p] was infoed: ~p.~n", [?MODULE, _Msg]),
    {noreply, State, State#state.keep_alive_timer}.


terminate(Reason, State) ->
    error_logger:info_msg("[~p] ~p was terminated with reason: ~p.~n", [?MODULE, State#state.socket, Reason]),
    dispatch(terminate, State, Reason),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

dispatch(handle_data, RawData, State) ->
    State2 = case State#state.client_id of
        undefined ->
            {ClientId0, KeepAliveTimer, _, _} = mqtt_utils:extract_connect_info(RawData),
			ClientId = string:to_upper(ClientId0),
            error_logger:info_msg("mqtt client get online(~p - ~p), KeepAliveTimer = ~p seconds.~n", [erlang:self(), ClientId, KeepAliveTimer]),
            State#state{client_id = ClientId, keep_alive_timer = KeepAliveTimer * 1000 + ?GRACE};
        _ ->
            State
    end,

    handle_packages(State2, RawData);

dispatch(terminate, State, Reason) ->
    #state{callback = Callback, socket = Socket, client_id = ClientId} = State,
    Callback:terminate(erlang:self(), Socket, ClientId, Reason),
    ok.


handle_packages(State, <<>>) ->
    {noreply, State, State#state.keep_alive_timer};

handle_packages(State, RawData) ->
    #state{
        socket = Socket, 
        callback = Callback, 
        client_id = ClientId} = State,

    {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
    PackData = binary:part(RawData, 0, FixedLength + RestLength), 
    <<TypeCode:4/integer, _:4/integer, _/binary>> = PackData,

    Result = case TypeCode of
        ?CONNECT -> 
            Callback:process_data_online(erlang:self(), Socket, PackData, ClientId),
            ok;

        ?PUBLISH ->     
            Callback:process_data_publish(erlang:self(), Socket, PackData, ClientId),
            ok;

		?PUBACK ->
            Callback:process_data_publish_ack(erlang:self(), Socket, PackData, ClientId),
            ok;

        ?PINGREQ ->
            PingRespData = mqtt:build_pingresp(),
            error_logger:info_msg("[~p] is sending PINGRESP(~p): ~p~n", [?MODULE, erlang:self(), PingRespData]),
            gen_tcp:send(Socket, PingRespData),
            ok;

        ?DISCONNECT ->
            error_logger:info_msg("[~p] received DISCONNECT(~p): ~p~n", [?MODULE, erlang:self(), ClientId]),
            disconnect            
    end,

    case Result of
        disconnect ->
            {stop, disconnected, State};
        ok ->
            RestRawData = binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength),
            handle_packages(State, RestRawData)
    end.
