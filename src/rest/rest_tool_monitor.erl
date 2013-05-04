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
	out_rrd_graph(Arg, mem);

out(Arg, ["rrd", "graph", "cpu_load"], _UserId) ->
	out_rrd_graph(Arg, cpu_load);

out(Arg, ["rrd", "graph", "cpu_util"], _UserId) ->
	out_rrd_graph(Arg, cpu_util);

out(_Arg, _, _) ->
	{status, 404}.


out_rrd_graph(Arg, Type) ->
	PostVals = yaws_api:parse_post(Arg),
	From = proplists:get_value("fromDateTime", PostVals),
	To = proplists:get_value("toDateTime", PostVals),

	Url = rrdtool_graph(Type, From, To),

	Result = [{"success", true}, {"data", {struct, [{"url", Url}]}}],
	{content, "application/json", json2:encode({struct, Result})}.


rrdtool_graph(Type, Start, End) ->
	RelFileName = lists:flatten(io_lib:format("/rrdtool_img/~s.png", [tools:generate_id(erlang:atom_to_list(Type))])),

	AbsFileName = lists:flatten(io_lib:format("~s/site~s", [code:priv_dir(tools_platform), RelFileName])),
    filelib:ensure_dir(AbsFileName),

	{ok, RrdtoolExe} = application:get_env(monitor, rrdtool_exe),	
	{ok, RrdFileName} = application:get_env(monitor, rrd_file_name),

	Definitions = case Type of
		mem ->
			[io_lib:format("DEF:memory=~s:mem:AVERAGE", [RrdFileName])];
		cpu_load ->
			[io_lib:format("DEF:cpu_load=~s:cpu_load:AVERAGE", [RrdFileName])];
		cpu_util ->
			[io_lib:format("DEF:cpu_util=~s:cpu_util:AVERAGE", [RrdFileName])]
	end,

	GraphElements = case Type of
		mem ->
			["LINE2:memory#FF0000:\"memory(%)\""];
		cpu_load ->
			["LINE2:cpu_load#FF0000:\"cpu load\""];
		cpu_util ->
			["LINE2:cpu_util#FF0000:\"cpu util(%)\""]
	end,

	rrdtool:graph(RrdtoolExe, AbsFileName, Start, End, Definitions, GraphElements),
	
	RelFileName.
