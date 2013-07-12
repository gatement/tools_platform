-module(rest_tool_webchat).
-include("yaws_api.hrl").
-include("../../platform_core/include/tools_platform.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case Arg#arg.pathinfo of
		undefined -> 
			{status, 404};
		_ -> 
			out(Arg, string:tokens(Arg#arg.pathinfo, "/"))
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
out(Arg, ["index"]) ->
	Body = erlang:binary_to_list(Arg#arg.clidata),
	io:format("/webchat/index:~p~n", [Body]),
	{html, "good2"};
	
out(_Arg, _) ->
	{status, 404}.

