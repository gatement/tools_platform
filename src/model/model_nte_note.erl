-module(model_nte_note).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/2,
		get/1, 
		get/2,
		exist/1,
		list/1, 
		delete/1,
		update_note_size/3, 
		update_note_position/3, 
		update_note_z_index/2, 
		update_note_color/2,
		update_note_content/3,
		update_note_content/4,
		move_to_category/2,
		update_note_category/2]).


%% ===================================================================
%% API functions
%% ===================================================================

create(Note, UserId) ->
	Id = tools:generate_id(UserId),
	LastUpdated = tools:epoch_milli_seconds(),

	Note2 = Note#nte_note{id = Id, last_updated = LastUpdated},

	Fun = fun() ->
			mnesia:write(Note2)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Note2;
		_ -> error
	end.


list(CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_note),
				X#nte_note.category_id =:= CategoryId]))
	end,

	{atomic, Notes} = mnesia:transaction(Fun),
	Notes.


get(Id) ->
	Fun = fun() ->
			mnesia:read({nte_note, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Note]} -> Note;
		_ -> error
	end.


get(NoteId, CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_note), 
						X#nte_note.id =:= NoteId, 
						X#nte_note.category_id =:= CategoryId]))
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Note]} -> Note;
		_ -> error
	end.


exist(Id) ->
	case model_nte_note:get(Id) of
		error -> false;
		_ -> true
	end.


delete(Id) ->
	Fun = fun() ->
			mnesia:delete({nte_note, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


update_note_size(Id, Width, Height) ->
	case model_nte_note:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				mnesia:write(Note#nte_note{width = Width, height = Height})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_note_position(Id, Left, Top) ->
	case model_nte_note:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				mnesia:write(Note#nte_note{left = Left, top = Top})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_note_z_index(Id, ZIndex) ->
	case model_nte_note:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				mnesia:write(Note#nte_note{z_index = ZIndex})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_note_color(Id, Color) ->
	case model_nte_note:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				mnesia:write(Note#nte_note{color = Color})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


update_note_content(Id, Content, UserId) -> 
	case model_nte_note:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
			   	model_nte_history:create(Note#nte_note.id, Note#nte_note.note, UserId),
				mnesia:write(Note#nte_note{note = Content, last_updated = tools:epoch_milli_seconds()})
			end,
			mnesia:transaction(Fun),
			ok
	end.


update_note_content(Id, Content, LastUpdated, UserId) -> 
	case model_nte_note:get(Id) of
		error -> 
			error;

		Note ->
			Fun = fun() ->
				if
					Note#nte_note.last_updated < LastUpdated ->
	                    model_nte_history:create(Note#nte_note.id, Note#nte_note.note, UserId),
						mnesia:write(Note#nte_note{note = Content, last_updated = tools:epoch_milli_seconds()}),
						ok;
					true -> ignore
				end
			end,
			
			mnesia:transaction(Fun)
	end.


update_note_category(NoteId, CategoryId) ->	
	case model_nte_note:get(NoteId) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				mnesia:write(Note#nte_note{category_id = CategoryId})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


move_to_category(FromCategoryId, ToCategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_note),
						X#nte_note.category_id =:= FromCategoryId]))
	end,
	{atomic, Notes} = mnesia:transaction(Fun),

	UpdateFun = fun(Model) ->
		mnesia:transaction(fun() -> 
			mnesia:write(Model#nte_note{category_id=ToCategoryId})
		end)
	end,
	lists:foreach(UpdateFun, Notes),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
