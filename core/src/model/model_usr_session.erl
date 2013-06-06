-module(model_usr_session).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
		delete/1, 
		delete_by_user/1,
		get/1, 
		exist/1, 
		update_last_active/1, 
		clear_old/1]).


%% ===================================================================
%% API functions
%% ===================================================================

create(Session) ->
	RandomStr = tools:random_string(15),
	Id = tools:generate_id(Session#usr_session.user_id) ++ RandomStr,
	LastActive = erlang:now(),

	Fun = fun() ->
			mnesia:write(Session#usr_session{id = Id,
							     last_active = LastActive})	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Id;
		_ -> error
	end.


delete(Id) ->
	Fun = fun() -> 
		mnesia:delete({usr_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, _} -> error
	end.


delete_by_user(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(usr_session), 
						X#usr_session.user_id =:= UserId]))
	end,
	{atomic, Sessions} = mnesia:transaction(Fun),

	DelFun = fun(Session) -> 
			model_usr_session:delete(Session#usr_session.id) 
	end,
	lists:foreach(DelFun, Sessions),

	ok.


get(Id) ->
	Fun = fun() -> 
		mnesia:read({usr_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, Session} -> Session;
		_ -> error
	end.


exist(Id) ->
	Fun = fun() -> 
		mnesia:read({usr_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, [_Session]} -> true
	end.


update_last_active(Id) ->
	case model_usr_session:get(Id) of
		[] -> 
			error;
		[Session] ->
			Fun = fun() ->
					mnesia:write(Session#usr_session{last_active = erlang:now()})	  
			end,

			mnesia:transaction(Fun)
	end.

%ttl is the max seconds a session can live 
%ttl needs to less than 7 days
clear_old(Ttl) ->
	{A, S, C} = erlang:now(),
	B = S - Ttl,
	OldTime = {A, B, C},

	Fun = fun() -> 
		Sessions = qlc:e(qlc:q([X || X <- mnesia:table(usr_session), 
						X#usr_session.last_active < OldTime])),

		DelFun = fun(Session) -> 
			model_usr_session:delete(Session#usr_session.id) 
		end,
		lists:foreach(DelFun, Sessions)
	end,

	mnesia:transaction(Fun),

	ok.

%% ===================================================================
%% Local Functions
%% ===================================================================
