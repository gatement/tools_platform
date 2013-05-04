-module(monitor_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {log_file, interval, rrdtool_exe, rrd_file}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
	{ok, Interal0} = application:get_env(run_interval),
	Interal = Interal0 * 1000,

	{ok, LogFileName} = application:get_env(log_file_name),
	{ok, RrdFileName} = application:get_env(rrd_file_name),
	{ok, RrdtoolExe} = application:get_env(rrdtool_exe),

	filelib:ensure_dir(LogFileName),
	FileExists = filelib:is_file(LogFileName),
	{ok, LogFile} = file:open(LogFileName, [append]),	
	case FileExists of
		false -> write_header(LogFile);
		_ -> ok
	end,

	cpu_sup:avg1(), % call it for a first time, so the next call will be exact.

    State = #state{log_file = LogFile, interval = Interal, rrdtool_exe = RrdtoolExe, rrd_file = RrdFileName},
    error_logger:info_msg("monitor was started.~n"),
    {ok, State, Interal}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, #state{log_file = LogFile, interval = Interal, rrdtool_exe = RrdtoolExe, rrd_file = RrdFileName} = State) ->
	write_status(LogFile, RrdtoolExe, RrdFileName),
    {noreply, State, Interal};
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
	LogFile = State#state.log_file,
    file:close(LogFile),
    error_logger:info_msg("monitor was down.~n"),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

write_header(LogFile) ->
	Data = "Time, MemTotal(B), MemAlloc(B), MaxEPAlloc(B), NProcs, CpuLoad, CpuUtil(%)\n",
	write_data(LogFile, Data).


write_status(LogFile, RrdtoolExe, RrdFileName) ->
	{MemTotal, MemAllocated,{_Pid, MaxPidAllocated}} = memsup:get_memory_data(),
	NProcs = cpu_sup:nprocs(),
	CpuLoad = cpu_sup:avg1(),
	CpuUtil = cpu_sup:util(),

	Data = io_lib:format("~s, ~p, ~p, ~p, ~p, ~p, ~p~n", 
		[tools:datetime_string('yyyyMMdd_hhmmss'), MemTotal, MemAllocated, MaxPidAllocated, NProcs, CpuLoad, CpuUtil]),
	write_data(LogFile, Data),

	ValStr = io_lib:format("~p:~p:~p", [MemAllocated*100/MemTotal, CpuLoad/256, CpuUtil*100]),
	rrdtool:update(RrdtoolExe, RrdFileName, "mem:cpu_load:cpu_util", ValStr).


write_data(LogFile, Data) ->
    %error_logger:info_msg("writing log data: ~p~n", [Data]),
	file:write(LogFile, Data).
