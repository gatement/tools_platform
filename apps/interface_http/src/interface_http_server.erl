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
    {ok, HttpPort} = application:get_env(http_port), 
    {ok, HttpsPort} = application:get_env(https_port), 
    {ok, KeyFile} = application:get_env(keyfile), 
    {ok, CertFile} = application:get_env(certfile), 

    PrivDir = code:priv_dir(interface_http),
    DocRoot = PrivDir ++ "/site",
    LogDir = PrivDir ++ "/../../../priv/log",
    IncludeDir1 = PrivDir ++ "/../include",
    IncludeDir2 = PrivDir ++ "/../../platform_core/include",

    filelib:ensure_dir(LogDir ++ "/"),
    
	GconfList = [ {logdir, LogDir},
		{include_dir, [IncludeDir1, IncludeDir2]},
		{runmods, []}],

	SconfList = [{listen, {0,0,0,0}},
		{docroot, DocRoot},
		{arg_rewrite_mod, arg_rewriter},
		{errormod_404, nofound},
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
			]}],
	
	%% HTTP 
    IdHttp = "tools_platform_http",
	GconfListHttp = [{id, IdHttp} | GconfList],
	SconfListHttp = [{port, HttpPort} | SconfList], 

    {ok, SCListHttp, GCHttp, ChildSpecsHttp} = yaws_api:embedded_start_conf(DocRoot, SconfListHttp, GconfListHttp, IdHttp),
    [supervisor:start_child(interface_http_sup, Ch) || Ch <- ChildSpecsHttp],
    yaws_api:setconf(GCHttp, SCListHttp),

	%% HTTPS
    IdHttps = "tools_platform_https",
	Ssl = #ssl{
		keyfile = KeyFile,
		certfile = CertFile
	},
	GconfListHttps = [{id, IdHttps} | GconfList],
	SconfListHttps = lists:append([{port, HttpsPort}, {ssl, Ssl}], SconfList), 

    {ok, SCListHttps, GCHttps, ChildSpecsHttps} = yaws_api:embedded_start_conf(DocRoot, SconfListHttps, GconfListHttps, IdHttps),
    [supervisor:start_child(interface_http_sup, Ch) || Ch <- ChildSpecsHttps],
    yaws_api:setconf(GCHttps, SCListHttps),

	ok.
