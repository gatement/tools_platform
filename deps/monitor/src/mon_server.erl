-module(mon_server).
-behaviour(gen_server).
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {log_file, interval}).


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

	FileExists = filelib:is_file(LogFileName),
	{ok, LogFile} = file:open(LogFileName, [append]),
	case FileExists of
		false -> write_header(LogFile);
		_ -> ok
	end,

    State = #state{log_file = LogFile, interval = Interal},
    error_logger:info_report("monitor was started."),
    {ok, State, Interal}.


handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, #state{log_file = LogFile, interval = Interal} = State) ->
	write_status(LogFile),
    {noreply, State, Interal};
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
	LogFile = State#state.log_file,
    file:close(LogFile),
    error_logger:info_report("monitor was down."),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% Local Functions
%% ===================================================================

write_header(LogFile) ->
	Data = "Time, MemTotal(MB), MemAlloc(MB), MaxEPAlloc(KB)\n",
	write_data(LogFile, Data).


write_status(LogFile) ->
	{MemTotal, MemAllocated,{_Pid, MaxPidAllocated}} = memsup:get_memory_data(),

	Data = io_lib:format("~s, ~p, ~p, ~p~n", [tools:datetime_string('yyyyMMdd_hhmmss'), MemTotal/1024/1024, MemAllocated/1024/1024, MaxPidAllocated/1024]),
	write_data(LogFile, Data).


write_data(LogFile, Data) ->
    error_logger:info_report("writing log data: ~p", [Data]),
	file:write(LogFile, Data).
