-module(cleaner_server).
%% API
-export([start_link/0, run/0, clean_up/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("[~p] was started.~n", [?MODULE]),
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.


run() ->
	timer:sleep(20000),
    %error_logger:info_report("Start cleaning."),
    clean_up(),

    {ok, RunInterval} = application:get_env(cleaner, run_interval),     
    timer:sleep(RunInterval * 1000),

    run().


%% ===================================================================
%% Local Functions
%% ===================================================================

clean_up() ->
    {ok, UserSessionTimeout} = application:get_env(cleaner, user_session_timeout), 
    %error_logger:info_msg("Start cleaning - UserSessionTimeout: ~p~n", [UserSessionTimeout]),
    model_usr_session:clear_old(UserSessionTimeout),

	model_mqtt_pub_queue:clear_old(),

    {ok, RrdtoolImgTimeout} = application:get_env(cleaner, rrdtool_img_timeout), 
    %error_logger:info_msg("Start cleaning - RrdtoolImgTimeout: ~p~n", [RrdtoolImgTimeout]),
    clean_rrdtool_img(RrdtoolImgTimeout),

    ok.


clean_rrdtool_img(RrdtoolImgTimeout) ->	
	ImgDir = code:priv_dir(interface_http) ++ "/site/rrdtool_img",
    %error_logger:info_msg("Start cleaning - RrdtoolImgDir: ~p~n", [ImgDir]),

	case filelib:is_dir(ImgDir) of
		false ->
			do_nothing;
		true ->		
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

			lists:foreach(Fun, FileNames)
	end.
