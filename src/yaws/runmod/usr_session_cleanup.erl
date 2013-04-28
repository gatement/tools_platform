-module(usr_session_cleanup).
-behaviour(supervisor).
%% supervisor callbacks
-export([start/0, init/1]).
%% API
-export([do/0, clean_up/0]).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

start() ->
	spawn(fun() -> supervisor:start_link(?MODULE, []) end).


init([]) ->
	{ok, {{one_for_one, 3, 10},
			[
				 {usr_session_cleanup,
				 {usr_session_cleanup, do, []},
				 permanent,
				 10000,
				 worker,
				 [usr_session_cleanup]}
			]
		 }
	}.


%% ===================================================================
%% API functions
%% ===================================================================

do() ->
	timer:apply_after(600000, ?MODULE, clean_up, []), % in milliseconds, 600000 = 10 min
	ok.


clean_up() ->
    model_usr_session:clear_old(129600), % in seconds, 129600 = 36 hours
    do().


%% ===================================================================
%% Local Functions
%% ===================================================================
