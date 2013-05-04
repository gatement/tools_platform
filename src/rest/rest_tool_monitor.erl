-module(rest_tool_monitor).
-include("yaws_api.hrl").
-include("tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,

	MonitorEnabled = model_usr_preference:get(UserId, ?USR_PREFERENCE_MONITOR_ENABLED),

	case MonitorEnabled of
		true ->
			case Arg#arg.pathinfo of
				undefined -> 
					{status, 404};
				_ -> 
					out(Arg, string:tokens(Arg#arg.pathinfo, "/"), UserId)
			end;
			
		false -> 
			{status, 404}
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
out(Arg, ["rrd", "graph", "memory"], _UserId) ->
	PostVals = yaws_api:parse_post(Arg),
	From = proplists:get_value("fromDateTime", PostVals),
	To = proplists:get_value("toDateTime", PostVals),

	Url = rrdtool_graph(mem, From, To),

	Result = [{"success", true}, {"data", {struct, [{"url", Url}]}}],
	{content, "application/json", json2:encode({struct, Result})};

out(Arg, ["rrd", "graph", "cpu"], _UserId) ->
	PostVals = yaws_api:parse_post(Arg),
	From = proplists:get_value("fromDateTime", PostVals),
	To = proplists:get_value("toDateTime", PostVals),

	Url = rrdtool_graph(cpu, From, To),

	Result = [{"success", true}, {"data", {struct, [{"url", Url}]}}],
	{content, "application/json", json2:encode({struct, Result})};

out(_Arg, _, _) ->
	{status, 404}.


rrdtool_graph(Type, Start, End) ->
	RelFileName = lists:flatten(io_lib:format("/rrdtool_img/~s.png", [tools:generate_id(erlang:atom_to_list(Type))])),

	AbsFileName = lists:flatten(io_lib:format("~s/site~s", [code:priv_dir(tools_platform), RelFileName])),
    filelib:ensure_dir(AbsFileName),

	{ok, RrdtoolExe} = application:get_env(monitor, rrdtool_exe),	
	{ok, RrdFileName} = application:get_env(monitor, rrd_file_name),

	Definitions = case Type of
		mem ->
			[io_lib:format("DEF:memory=~s:mem:AVERAGE", [RrdFileName])];
		cpu ->
			[io_lib:format("DEF:cpu=~s:cpu:AVERAGE", [RrdFileName])]
	end,

	GraphElements = case Type of
		mem ->
			["LINE2:memory#FF0000:\"memory(%)\""];
		cpu ->
			["LINE2:cpu#FF0000:\"cpu(%)\""]
	end,

	rrdtool:graph(RrdtoolExe, AbsFileName, Start, End, Definitions, GraphElements),
	
	RelFileName.
