-module(peanuthull).
-export([update_ip/0]).


%% ===================================================================
%% API functions
%% ===================================================================

update_ip() ->
	tools:debug_log("~n======== start update ip to peanuthull========~n", []),

 	{ok, Service} = application:get_env(iptracker, peanuthull_service),
 	{ok, UserId} = application:get_env(iptracker, peanuthull_user_id),
 	{ok, Password} = application:get_env(iptracker, peanuthull_user_password),
	Url = lists:flatten(io_lib:format(Service, [UserId, Password])),

	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Result}} = httpc:request(get, {Url, [{"User-Agent", "Johnson's peanuthull client"}]}, [], []),

	tools:debug_log("return: ~s~n", [Result]),
	tools:debug_log("(~s)~n~n", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),

	Result.


%% ===================================================================
%% Local Functions
%% ===================================================================

