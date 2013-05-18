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
		update_note_category/2,
		move_note_to_trash/2]).


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
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(Id) of
		error -> 
			error;
		Note ->
			Fun = fun() ->
				case Note#nte_note.note of
					undefined -> do_nothing;
					[] -> do_nothing;
					_ -> model_nte_history:create(Note#nte_note.id, Note#nte_note.note, UserId)
				end,
				mnesia:write(Note#nte_note{note = Content, last_updated = tools:epoch_milli_seconds()})
			end,
			mnesia:transaction(Fun),
			ok
	end.


update_note_content(Id, Content, LastUpdated, UserId) -> 
	case ?MODULE:get(Id) of
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
	case ?MODULE:get(NoteId) of
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


move_note_to_trash(NoteId, UserId) ->	
	case ?MODULE:get(NoteId) of
		error -> 
			error;
		Note ->
			#nte_category{id = TrashCategoryId} = model_nte_category:get_trash_category(UserId),

			Fun = fun() ->
				mnesia:write(Note#nte_note{category_id = TrashCategoryId})
			end,
			mnesia:transaction(Fun), 
			ok
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
