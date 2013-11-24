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

    ok.
