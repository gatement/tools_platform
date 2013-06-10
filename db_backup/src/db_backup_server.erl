-module(db_backup_server).
%% API
-export([start_link/0, run/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.


run() ->
	timer:sleep(60000),
    %error_logger:info_report("Start db backup."),
	do_backup(),

    {ok, RunInterval} = application:get_env(database_backup_interval), 
	timer:sleep(RunInterval * 1000),
    run().


%% ===================================================================
%% Local Functions
%% ===================================================================

do_backup() ->
    {ok, BackupFolder} = application:get_env(database_backup_dir), 
	FileName = lists:flatten(io_lib:format("~s/mnesia_~s.bak", [BackupFolder, tools:datetime_string('yyyyMMdd_hhmmss')])),
    
    %error_logger:info_msg("backup db to: ~p~n", [FileName]),

    mnesia:backup(FileName),

    ok.
