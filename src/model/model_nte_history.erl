-module(model_nte_history).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/3,
		list/1]).


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
	Models.


%% ===================================================================
%% Local Functions
%% ===================================================================
