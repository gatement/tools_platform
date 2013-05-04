-module(session_cleaner_server).
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
    {ok, RunInterval} = application:get_env(session_cleaner, run_interval),     
    timer:apply_after(RunInterval * 1000, ?MODULE, clean_up, []),
    ok.


clean_up() ->
    %error_logger:info_report("Clean user session."),
    {ok, UserSessionTimeout} = application:get_env(session_cleaner, user_session_timeout), 
    model_usr_session:clear_old(UserSessionTimeout),
    run().
