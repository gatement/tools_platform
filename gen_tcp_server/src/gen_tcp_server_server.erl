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
                opaque}).


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

    %error_logger:info_msg("A new gen_tcp_server_server was started.~n"),
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
    {noreply, State2};

handle_info({tcp_closed, _Socket}, State) ->
    %error_logger:info_msg("gen_tcp_server_server was infoed: ~p.~n", [tcp_closed]),
    {stop, normal, State};

handle_info(timeout, #state{lsocket = LSocket, socket = Socket0, parent = Parent} = State) ->
    {ok, ConnectionIdleTimeout} = application:get_env(gen_tcp_server, connection_idle_timout),
    case Socket0 of
        undefined ->
            {ok, Socket} = gen_tcp:accept(LSocket),
            gen_tcp_server_connection_sup:start_child(Parent),

            %% log
            {ok, {Address, Port}} = inet:peername(Socket),
            {noreply, State#state{socket = Socket, ip = Address, port = Port}, ConnectionIdleTimeout}; %% this socket must recieve the first message in Idle Timeout seconds
        Socket ->
            Ip = State#state.ip,
            Port = State#state.port,
            error_logger:info_msg("Disconnected ~p ~p:~p because of the first message doesn't arrive in ~p milliseconds.~n", [Socket, Ip, Port, ConnectionIdleTimeout]),
            {stop, normal, State} %% no message arrived in time so suicide
    end; 

handle_info({send_tcp_data, Data}, #state{socket = Socket} = State) ->
    gen_tcp_server:tcp_reply(Socket, Data),
    {noreply, State};

handle_info(stop, State) ->    
    error_logger:info_msg("process ~p was stopped~n", [erlang:self()]),
    {stop, normal, State};
    
handle_info(_Msg, State) ->
    %error_logger:info_msg("gen_tcp_server_server was infoed: ~p.~n", [_Msg]),
    {noreply, State}.


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

    #state{socket = Socket, callback = Callback, user_data = UserData, opaque = Opaque} = State2,
    SourcePid = erlang:self(),
    Callback:handle_data(SourcePid, Socket, RawData, UserData, Opaque),

    State2.
    

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
