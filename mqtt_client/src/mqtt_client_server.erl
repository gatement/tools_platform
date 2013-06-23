-module(mqtt_client_server).
-include("../../mqtt_broker/include/mqtt.hrl").
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {socket}).

-define(CONNECTION_TIMEOUT, 10000).
-define(KEEP_ALIVE_TIMER, 1800000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    State = #state{},

    error_logger:info_msg("[~p] was started.~n", [?MODULE]),
    {ok, State, 0}.


handle_call(_Msg, _From, State) ->
    %error_logger:info_msg("[~p] was called: ~p.~n", [?MODULE, _Msg]),
    {reply, ok, State, ?KEEP_ALIVE_TIMER}.


handle_cast(_Msg, State) ->
    %error_logger:info_msg("[~p] was casted: ~p.~n", [?MODULE, _Msg]),
    {noreply, State, ?KEEP_ALIVE_TIMER}.


handle_info({tcp, _Socket, RawData}, State) ->
    %error_logger:info_msg("[~p] received tcp data: ~p~n", [?MODULE, RawData]),
    handle_packages(State, RawData);

handle_info({tcp_closed, _Socket}, State) ->
    %error_logger:info_msg("[~p] was infoed: ~p.~n", [?MODULE, tcp_closed]),
    {stop, normal, State};

handle_info(timeout, State) ->
    case State#state.socket of
        undefined ->            
            {ok, ServerHost} = application:get_env(mqtt_broker_host),
            {ok, ServerPort} = application:get_env(mqtt_broker_port),
            case gen_tcp:connect(ServerHost, ServerPort, [binary, {active, true}]) of
                {ok, Socket} ->
                    %% send CONNECT
                    {ok, ClientId} = application:get_env(client_id),
                    ConnectData = mqtt:build_connect(ClientId, ?KEEP_ALIVE_TIMER div 1000),
                    %error_logger:info_msg("[~p] sending CONNECT: ~p~n", [?MODULE, ConnectData]),
                    gen_tcp:send(Socket, ConnectData),

                    %% waiting for CONNACK
                    receive
                        {tcp, Socket, Msg} -> 
                            %error_logger:info_msg("[~p] received tcp data ~p: ~p~n", [?MODULE, erlang:self(), Msg]),
                            case mqtt_utils:is_connack_success(Msg) of
                                true ->            
                                    State2 = State#state{socket = Socket},            
                                    {noreply, State2, ?KEEP_ALIVE_TIMER};
                                false ->
                                    {stop, bad_connack, State}
                            end
                    after
                        ?CONNECTION_TIMEOUT ->
                            {stop, no_connack, State}
                    end;
                _ ->
                    {stop, connection_failed, State}
            end;
        Socket ->
            %% do heartbeat (ping broker)
            PingReqData = mqtt:build_pingreq(),
            %error_logger:info_msg("[~p] is sending PINGREQ: ~p~n", [?MODULE, PingReqData]),
            gen_tcp:send(Socket, PingReqData),
            {noreply, State, ?KEEP_ALIVE_TIMER}
    end;


handle_info({send_tcp_data, Data}, #state{socket = Socket} = State) ->    
    %error_logger:info_msg("[~p] send to socket ~p with data: ~p~n", [?MODULE, Socket, Data]),
    gen_tcp:send(Socket, Data),
    {noreply, State, ?KEEP_ALIVE_TIMER};

handle_info(stop, State) ->
    %error_logger:info_msg("[~p] process ~p was stopped~n", [?MODULE, erlang:self()]),
    {stop, normal, State};
    
handle_info(_Msg, State) ->
    %error_logger:info_msg("[~p] was infoed: ~p.~n", [?MODULE, _Msg]),
    {noreply, State, ?KEEP_ALIVE_TIMER}.


terminate(_Reason, _State) ->
    %error_logger:info_msg("[~p] process ~p was terminated with reason: ~p.~n", [?MODULE, erlang:self(), _Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

handle_packages(State, <<>>) ->
    {noreply, State, ?KEEP_ALIVE_TIMER};
handle_packages(State, RawData) ->
    {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
    Data = binary:part(RawData, 0, FixedLength + RestLength), 
    <<TypeCode:4/integer, _:4/integer, _/binary>> = Data,

    Result = case TypeCode of
        ?PUBLISH ->     
            mqtt_client_handler:process_data_publish(erlang:self(), State#state.socket, Data),
            ok;

        ?PINGRESP ->
            ok;

        ?DISCONNECT ->
            stop        
    end,

    case Result of
        stop ->
            {stop, normal, State};
        ok ->
            RestRawData = binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength),
            handle_packages(State, RestRawData)
    end.