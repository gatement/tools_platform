-module(model_nte_share).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([list/1,
		get/1,
		get_by_category_id/1,
		delete/1,
		delete_by_category_id/1,
		create/1,
		update/1, 
		exist/2, 
		get_permission_by_category_id/2, 
		get_permission_by_note_id/2, 
		category_list/1,
		is_shared/1]).


%% ===================================================================
%% API Functions
%% ===================================================================

list(CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([#note_share{id=Share#nte_share.id,
								 category_id=Share#nte_share.category_id, 
								 user_id=User#usr_user.id,
								 user_name=User#usr_user.name,
								 share_type=erlang:atom_to_list(Share#nte_share.share_type)} 
					|| Share <- mnesia:table(nte_share),
						User <- mnesia:table(usr_user), 
						Share#nte_share.user_id =:= User#usr_user.id, 
						Share#nte_share.category_id =:= CategoryId]))
	end,

	{atomic, Shares} = mnesia:transaction(Fun),
	Shares.
	

get(Id) ->
	Fun = fun() -> 
		mnesia:read({nte_share, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [NoteShare]} -> NoteShare
	end.


get_by_category_id(CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_share),
						X#nte_share.category_id =:= CategoryId]))
	end,

	{atomic, Shares} = mnesia:transaction(Fun),
	Shares.
	

delete(Id) ->
	Fun = fun() -> 
		mnesia:delete({nte_share, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


delete_by_category_id(CategoryId) ->
	Shares = model_nte_share:get_by_category_id(CategoryId),

	Fun = fun(Share) ->
		mnesia:transaction(fun() -> mnesia:delete({nte_share, Share#nte_share.id}) end)
	end,

	lists:foreach(Fun, Shares).
	

create(NoteSharing) ->
	case model_nte_share:exist(NoteSharing#nte_share.user_id, NoteSharing#nte_share.category_id) of
		true -> duplicate;
		false -> 
			Fun = fun() ->
				Id = NoteSharing#nte_share.user_id ++ NoteSharing#nte_share.category_id,
				mnesia:write(NoteSharing#nte_share{id = Id})	  
			end,

			case mnesia:transaction(Fun) of
				{atomic, ok} -> ok;
				_ -> error
			end
	end.
	

update(NoteSharing) ->
	Fun = fun() ->
			mnesia:write(NoteSharing)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


exist(UserId, CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_share), 
						X#nte_share.user_id =:= UserId, 
						X#nte_share.category_id =:= CategoryId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Categories} -> true
	end.


get_permission_by_category_id(UserId, CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.id =:= CategoryId, 
						X#nte_category.user_id =:= UserId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> 
			Fun2 = fun() -> 
				qlc:e(qlc:q([X#nte_share.share_type || X <- mnesia:table(nte_share), 
						X#nte_share.category_id =:= CategoryId, 
						X#nte_share.user_id =:= UserId]))
			end,

			case mnesia:transaction(Fun2) of
				{atomic, []} -> none;
				{atomic, [ShareType]} -> ShareType
			end;
		{atomic, _} ->
			owner
	end.


get_permission_by_note_id(UserId, NoteId) ->
	case model_nte_note:get(NoteId) of
		error -> none;
		Note -> model_nte_share:get_permission_by_category_id(UserId, Note#nte_note.category_id)
	end.


category_list(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([#note_category{id = Category#nte_category.id, 
									name = Category#nte_category.name ++ "(" ++ Category#nte_category.user_id ++ ")",
									permission = erlang:atom_to_list(Share#nte_share.share_type),
									is_default = false} 
				|| Share <- mnesia:table(nte_share),
				Category <- mnesia:table(nte_category), 
				Share#nte_share.category_id =:= Category#nte_category.id, 
				Share#nte_share.user_id =:= UserId]))
	end,
	{atomic, NoteCategories} = mnesia:transaction(Fun),
	NoteCategories.


is_shared(CategoryId) ->
	Shares = model_nte_share:get_by_category_id(CategoryId),
	case erlang:length(Shares) of
		0 -> false;
		_ -> true
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
