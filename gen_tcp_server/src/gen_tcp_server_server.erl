-module(gen_tcp_server_server).
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
                opaque,
                is_checking_heartbeat}).

-define(HEATBEAT_TIMEOUT, 10000). %% in milliseconds

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
        user_data = UserData,
        is_checking_heartbeat = false},

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
    State2 = dispatch(handle_data, RawData, State),

    {ok, HeartbeatCheckingInterval} = application:get_env(gen_tcp_server, heartbeat_checking_interval),
    {noreply, State2, HeartbeatCheckingInterval};

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
            {ok, ConnectionIdleTimeout} = application:get_env(gen_tcp_server, connection_idle_timout),
            {noreply, State#state{socket = Socket, ip = Address, port = Port}, ConnectionIdleTimeout}; %% this socket must recieve the first message in Idle Timeout seconds
        Socket ->
            case State#state.opaque of
                undefined ->
                    %% no first package income yet
                    Ip = State#state.ip,
                    Port = State#state.port,
                    {ok, ConnectionIdleTimeout} = application:get_env(gen_tcp_server, connection_idle_timout),
                    error_logger:info_msg("disconnected ~p ~p:~p because of the first message doesn't arrive in ~p milliseconds.~n", [Socket, Ip, Port, ConnectionIdleTimeout]),
                    {stop, normal, State}; %% no message arrived in time so suicide
                _ ->
                    case State#state.is_checking_heartbeat of
                        false ->
                            %% check if remote is still alive (heartbeat)
                            Callback = State#state.callback,
                            HeartbeatData = Callback:get_hearbeat_data(),
                            gen_tcp_server:tcp_reply(Socket, HeartbeatData),
                            State2 = State#state{is_checking_heartbeat = true},
                            Ip = State#state.ip,
                            Port = State#state.port,
                            error_logger:info_msg("asking ~p ~p:~p for heartbeat (must reply within ~p ms).~n", [Socket, Ip, Port, ?HEATBEAT_TIMEOUT]),
                            {noreply, State2, ?HEATBEAT_TIMEOUT};
                        true ->
                            %% the remote has no heartbeat
                            Ip = State#state.ip,
                            Port = State#state.port,
                            error_logger:info_msg("disconnected ~p ~p:~p because of the remote has no heartbeat.~n", [Socket, Ip, Port]),
                            {stop, normal, State} %% no message arrived in time so suicide
                    end
            end
    end; 

handle_info({send_tcp_data, Data}, #state{socket = Socket} = State) ->
    gen_tcp_server:tcp_reply(Socket, Data),

    {ok, HeartbeatCheckingInterval} = application:get_env(gen_tcp_server, heartbeat_checking_interval),
    {noreply, State, HeartbeatCheckingInterval};

handle_info(stop, State) ->
    error_logger:info_msg("process ~p was stopped~n", [erlang:self()]),
    {stop, normal, State};
    
handle_info(_Msg, State) ->
    %error_logger:info_msg("gen_tcp_server_server was infoed: ~p.~n", [_Msg]),
    {ok, HeartbeatCheckingInterval} = application:get_env(gen_tcp_server, heartbeat_checking_interval),
    {noreply, State, HeartbeatCheckingInterval}.


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
    State2 = case State#state.opaque of
        undefined ->
            RequestType = State#state.user_data,
            Opaque0 = extract_sn(RawData, RequestType),
            State#state{opaque = Opaque0};
        _ ->
            State
    end,

    State3 = State2#state{is_checking_heartbeat = false},

    #state{socket = Socket, callback = Callback, user_data = UserData, opaque = Opaque} = State3,
    SourcePid = erlang:self(),
    Callback:handle_data(SourcePid, Socket, RawData, UserData, Opaque),

    State3.
    

dispatch(terminate, State) ->
    #state{callback = Callback, user_data = UserData, opaque = Opaque} = State,
    Callback:terminate(UserData, Opaque),
    ok.


extract_sn(Data, RequestType) ->
    case RequestType of
        device ->
            <<16#41, Mac11:8/integer, Mac10:8/integer, Mac9:8/integer, Mac8:8/integer, Mac7:8/integer, Mac6:8/integer, Mac5:8/integer, Mac4:8/integer, Mac3:8/integer, Mac2:8/integer, Mac1:8/integer, Mac0:8/integer, _/binary>> = Data,
            [Mac11, Mac10, Mac9, Mac8, Mac7, Mac6, Mac5, Mac4, Mac3, Mac2, Mac1, Mac0]
    end.
