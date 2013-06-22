-module(mqtt_client_handler).
-include("../../platform_core/include/tools_platform.hrl").
-export([handle_data/3]).


%% ===================================================================
%% API functions
%% ===================================================================

handle_data(SourcePid, Socket, RawData) -> 
    error_logger:info_msg("~p received tcp data: ~p~n", [?MODULE, RawData]),
    handle_data_inner(SourcePid, Socket, RawData).


%% ===================================================================
%% Local Functions
%% ===================================================================

handle_data_inner(_, _, <<>>) ->
    ok;
handle_data_inner(_SourcePid, _Socket, _RawData) ->
    %error_logger:info_msg("process_data_publish - ~p, Topic: ~p, Payload: ~p~n", [SourcePid, Topic, Payload]), 
    ok.
