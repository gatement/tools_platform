-module(tools_platform_server).
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
    {ok, ParentDir} = application:get_env(parent_dir), 

    LogDir = ParentDir ++ "/priv/logs",
    filelib:ensure_dir(LogDir ++ "/"),

    Id = "tools_platform",
    DocRoot = ParentDir ++ "/priv/site",
    
    GconfList = [{id, Id},
                 {logdir, LogDir},
		 {include_dir, [ParentDir ++ "/include"]},
		 {runmods, [cleaner, monitor]}],

    SconfList = [{port, Port},
                 {listen, {0,0,0,0}},
                 {docroot, DocRoot},
		 {arg_rewrite_mod, arg_rewriter},
		 {errormod_404, nofound},
                 {appmods, [
			    {"/user/", rest_usr_user},
			    {"/setting/", rest_gbl_setting},
                {"/monitor/", rest_tool_monitor},
                {"/word/", rest_tool_word},
                {"/note/", rest_tool_note},
                {"/gallery/", rest_tool_gallery}
			   ]}],


    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(DocRoot, SconfList, GconfList, Id),
    [supervisor:start_child(tools_platform_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList).
