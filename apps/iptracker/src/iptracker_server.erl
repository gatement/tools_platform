-module(iptracker_server).
-include("../../platform_core/include/tools_platform.hrl").
-include("../../mqtt_broker/include/mqtt.hrl").
-export([start_link/0, 
		 run/0,
		 do/1, 
		 doing/1]).
-define(IP_TABLE_KEY, ip).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    error_logger:info_msg("[~p] was started.~n", [?MODULE]),
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.


run() ->
	erlang:process_flag(trap_exit, true),

	TableId = ets:new(current_ip, [set, public]),
	do(TableId).


do(TableId) ->
	Pid = erlang:spawn_link(?MODULE, doing, [TableId]),

	receive
 		{'EXIT', Pid, normal} -> % success
 			{ok, RunInterval0} = application:get_env(iptracker, run_interval),
 			RunInterval = RunInterval0 * 1000,
			timer:sleep(RunInterval),
			do(TableId);

 		{'EXIT', Pid, Reason} -> % error 		
			tools:debug_log("~n========> error~n"),
			tools:debug_log("~p~n", [Reason]),
			tools:debug_log("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),

			send_error_email(Reason),
			
 			{ok, ErrorRetryInterval0} = application:get_env(iptracker, error_retry_interval),
 			ErrorRetryInterval = ErrorRetryInterval0 * 1000,
			timer:sleep(ErrorRetryInterval),
 			do(TableId)
 	end.


doing(TableId) ->
	Ip = ip_getter:get_ip(),

	tools:debug_log("Current IP: ~s~n", [Ip]),
	tools:debug_log("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),

	case ets:lookup(TableId, ?IP_TABLE_KEY) of
		[] ->
			Reason = "starting",
			ip_changed(Ip, Reason, TableId);

		[{?IP_TABLE_KEY, Val}] ->
			case Val of
				Ip -> 
					no_change;
				_  ->
					Reason = "changed",
					ip_changed(Ip, Reason, TableId)
			end
	end,

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

ip_changed(Ip, Reason, TableId) ->
	PeanuthullResult = case application:get_env(iptracker, peanuthull_enabled) of
		{ok, true} -> lists:flatten(io_lib:format("updated (~p)", [peanuthull:update_ip()]));
		{ok, false} -> "disabled"
	end,

	%send_success_email(Ip, Reason, PeanuthullResult),
	send_push_notification(Ip, Reason, PeanuthullResult),

	ets:insert(TableId, {?IP_TABLE_KEY, Ip}).


send_push_notification(Ip, Reason, _PeanuthullResult) ->
	Msg = lists:flatten(io_lib:format("~s(~s)", [Ip, Reason])),
    mqtt_broker:send_persistence_msg(Msg),
	ok.


send_success_email(Ip, Reason, PeanuthullResult) ->
	Subject = lists:flatten(io_lib:format("Server Ip: ~s (~s), Peanuthull: ~s", [Ip, Reason, PeanuthullResult])),
	Content = Subject,
	send_email(Subject, Content).


send_error_email(Reason) ->
	Subject = "Getting Server IP Error",
	Content = Reason,
	send_email(Subject, Content).


send_email(Subject, Content) ->
	Sender = model_gbl_setting:get(?GBL_SETTING_EMAIL),
	SenderPassword = model_gbl_setting:get(?GBL_SETTING_EMAIL_PASSWORD),

 	{ok, Receiver} = application:get_env(iptracker, receiver_email),

	smtp_client:send_email(Sender, SenderPassword, [Receiver], Subject, Content),

	ok.

