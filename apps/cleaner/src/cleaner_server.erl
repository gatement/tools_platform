-module(cleaner_server).
%% API
-export([start_link/0, run/0, clean_up/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, erlang:spawn(?MODULE, run, [])}.


%% ===================================================================
%% Local Functions
%% ===================================================================

run() ->
    {ok, RunInterval} = application:get_env(cleaner, run_interval),     
    timer:apply_after(RunInterval * 1000, ?MODULE, clean_up, []),
    ok.


clean_up() ->
    %error_logger:info_report("Start cleaning."),

    {ok, UserSessionTimeout} = application:get_env(cleaner, user_session_timeout), 
    model_usr_session:clear_old(UserSessionTimeout),

    {ok, RrdtoolImgTimeout} = application:get_env(cleaner, rrdtool_img_timeout), 
    clean_rrdtool_img(RrdtoolImgTimeout),

    run().


clean_rrdtool_img(RrdtoolImgTimeout) ->	
	ImgDir = code:priv_dir(tools_platform) ++ "/site/rrdtool_img",

	{ok, FileNames} = file:list_dir(ImgDir),

	Fun = fun(FileName) ->
		if 
			FileName =:= "." -> ignore;
			FileName =:= ".." -> ignore;
			true ->
				FileFullName = ImgDir ++ "/" ++ FileName,
				LastModified = filelib:last_modified(FileFullName),
				LastModifiedSeconds = calendar:datetime_to_gregorian_seconds(LastModified),
				NowSeconds = calendar:datetime_to_gregorian_seconds({erlang:date(), erlang:time()}),
				if 
					NowSeconds - LastModifiedSeconds > RrdtoolImgTimeout ->
						file:delete(FileFullName);
					true ->
						do_nothing
				end
		end
	end,

	lists:foreach(Fun, FileNames).
