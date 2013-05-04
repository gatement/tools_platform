-module(rrdtool).
-export([create/6, update/4, graph/6]).

%% ===================================================================
%% API functions
%% ===================================================================

create(Exe, File, Start, Step, DSs, RRAs) ->
	TimeStr = io_lib:format("--start ~p --step ~p", [Start, Step]), 
	DSsStr = tools:list_string_to_string(DSs, " ", []),
	RRAsStr = tools:list_string_to_string(RRAs, " ", []),
	Cmd = lists:flatten(io_lib:format("~s create ~s ~s ~s ~s", [Exe, File, TimeStr, DSsStr, RRAsStr])),
	Result = os:cmd(Cmd),
    %error_logger:info_msg("rrdtool create: ~p~n", [Cmd]),
    Result.


update(Exe, File, Template, Value) ->
	TemplateStr = io_lib:format("--template ~s", [Template]),
	ValueStr = io_lib:format("N:~p", [Value]),
	Cmd = lists:flatten(io_lib:format("~s update ~s ~s ~s", [Exe, File, TemplateStr, ValueStr])),
	Result = os:cmd(Cmd),
    %error_logger:info_msg("rrdtool update: ~p~n", [Cmd]),
    Result.


graph(Exe, Png, Start, End, Definitions, GraphElements) ->
	DefinitionsStr = tools:list_string_to_string(Definitions, " ", []),
	GraphElementsStr = tools:list_string_to_string(GraphElements, " ", []),
	Cmd = lists:flatten(io_lib:format("~s graph ~s --start ~p --end ~p ~s ~s", [Exe, Png, Start, End, DefinitionsStr, GraphElementsStr])),
	Result = os:cmd(Cmd),
    %error_logger:info_msg("rrdtool graph: ~p~n", [Cmd]),
    Result.
	

%% ===================================================================
%% Local Functions
%% ===================================================================
