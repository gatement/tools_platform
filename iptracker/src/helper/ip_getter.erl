-module(ip_getter).
-export([get_ip/0]).


%% ===================================================================
%% API functions
%% ===================================================================

get_ip() ->
	{ok, Url} = application:get_env(iptracker, ip_getting_url),

	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Result}} = httpc:request(Url),

	KeyString = "IP Address: ",
	KeyStringLen = erlang:length(KeyString),
	Pos1 = string:str(Result, KeyString),
	Pos2 = string:str(Result, "</body>"),
	StartPos = Pos1 + KeyStringLen,
	EndPos = Pos2 - Pos1 - KeyStringLen,

	Ip = string:substr(Result, StartPos, EndPos),

	Ip.


%% ===================================================================
%% Local Functions
%% ===================================================================

