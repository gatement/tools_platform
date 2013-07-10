-module(interface_http_server).
-include("yaws.hrl").
%% API
-export([start/0, run/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	{ok, erlang:spawn(?MODULE, run, [])}.


%% ===================================================================
%% Local Functions
%% ===================================================================

run() ->
	{ok, Port} = application:get_env(port), 
	{ok, KeyFile} = application:get_env(keyfile), 
	{ok, CertFile} = application:get_env(certfile), 

	PrivDir = code:priv_dir(interface_http),
	DocRoot = PrivDir ++ "/site",
	LogDir = PrivDir ++ "/../../../priv/log",
	IncludeDir1 = PrivDir ++ "/../include",
	IncludeDir2 = PrivDir ++ "/../../platform_core/include",

	filelib:ensure_dir(LogDir ++ "/"),

	Id = "tools_platform",

	GconfList = [
		{id, Id},
		{logdir, LogDir},
		{include_dir, [IncludeDir1, IncludeDir2]},
		{runmods, []}],

	Ssl = case KeyFile of
		[] ->
			undefined;
		_ ->
			#ssl{
				keyfile = KeyFile,
				certfile = CertFile
			}
	end,

	SconfList = [
		{port, Port},
		{listen, {0,0,0,0}},
		{docroot, DocRoot},
		{arg_rewrite_mod, arg_rewriter},
		{errormod_404, nofound},
		{ssl, Ssl},
		{appmods, [
				{"/user/", rest_usr_user},
				{"/setting/", rest_gbl_setting},
				{"/mqtt/", rest_mqtt},
				{"/monitor/", rest_tool_monitor},
				{"/word/", rest_tool_word},
				{"/note/", rest_tool_note},
				{"/gallery/", rest_tool_gallery},
				{"/device/", rest_tool_device},
				{"/device_mgmt/", rest_device_mgmt}
			]}
	],

	{ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(DocRoot, SconfList, GconfList, Id),
	[supervisor:start_child(interface_http_sup, Ch) || Ch <- ChildSpecs],
	yaws_api:setconf(GC, SCList),

	ok.
