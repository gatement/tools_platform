-module(model_nte_history).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/3,
		list/1,
		delete/1,
		delete_by_noteId/1]).


%% ===================================================================
%% API functions
%% ===================================================================

create(NoteId, Note, UserId) ->
	Id = tools:generate_id(UserId),
	Datetime = tools:datetime_string('yyyyMMdd hh:mm:ss'),

	Model = #nte_history{id = Id, note_id = NoteId, note = Note, datetime = Datetime},

	Fun = fun() ->
			mnesia:write(Model)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Model;
		_ -> error
	end.


list(NoteId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_history),
				X#nte_history.note_id =:= NoteId]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),

	% sort by datetime
	SortFun = fun(A, B) ->
		if
			A#nte_history.datetime >= B#nte_history.datetime -> true;
			true -> false
		end
	end,
	SortedModels = lists:sort(SortFun, Models),

	SortedModels.


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({nte_history, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


delete_by_noteId(NoteId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_history),
				X#nte_history.note_id =:= NoteId]))
	end,
	{atomic, Models} = mnesia:transaction(Fun),

	[?MODULE:delete(X#nte_history.id) || X <- Models],

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
