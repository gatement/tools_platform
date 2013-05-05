-module(model_nte_category).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1,
	     list/2,
	     exist/1,
	     exist/2,
	     exist/3,
	     update/4,
	     delete/2,
	     get/1,
	     get/2,
	     get_default_category/1,
	     get_any_category/1,
	     get_default_or_any_category/1,
	     clear_is_default/1,
	     count/1,
	     up/2,
	     down/2,
	     get_owner/1
	    ]).


%% ===================================================================
%% API functions
%% ===================================================================

create(NoteCategory) ->
	Id = tools:generate_id(NoteCategory#nte_category.user_id),
	DisplayOrder = tools:epoch_macro_seconds(),

	Fun = fun() ->
			mnesia:write(NoteCategory#nte_category{id=Id, display_order=DisplayOrder})	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


list(UserId, AppendShareTag) ->
	Fun = fun() -> 
		qlc:e(qlc:q([#note_category{id = X#nte_category.id, 
									name = X#nte_category.name,
									permission = "owner",
									is_default = X#nte_category.is_default,
									display_order = X#nte_category.display_order} 
						|| X <- mnesia:table(nte_category), 
						X#nte_category.user_id =:= UserId]))
	end,
	{atomic, NoteCategories} = mnesia:transaction(Fun),

	% append [s] to the name if it is shared
	UpdatedCategories = case AppendShareTag of
							true -> 
								UpdateNameFun = fun(Category) ->
									case model_nte_share:is_shared(Category#note_category.id) of
										true -> Category#note_category{name=Category#note_category.name ++ "[s]"};
										false -> Category
									end
								end,
								lists:map(UpdateNameFun, NoteCategories);
							false ->
								NoteCategories
						end,

	% sort categories by display_order
	SortFun = fun(A, B) ->
		if
			A#note_category.display_order =< B#note_category.display_order -> true;
			true -> false
		end
	end,
	SortedCategories = lists:sort(SortFun, UpdatedCategories),

	SortedCategories.


exist(Id) ->
	case model_nte_category:get(Id) of
		error -> false;
		_ -> true
	end.


exist(UserId, Name) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.user_id =:= UserId, 
						X#nte_category.name =:= Name]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Categories} -> true
	end.


exist(UserId, Name, ExceptCategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.id /= ExceptCategoryId,
						X#nte_category.user_id =:= UserId, 
						X#nte_category.name =:= Name]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Categories} -> true
	end.


update(Id, Name, IsDefault, UserId) ->
	Fun = fun() ->
			case mnesia:read({nte_category, Id}) of
				[] -> 
					error;
				[Category] ->
					%if it is default, clear other categories default value under the current user
					case IsDefault of
						true -> model_nte_category:clear_is_default(UserId);
						_ -> ok
					end,

					mnesia:write(Category#nte_category{name=Name, is_default=IsDefault}),
					ok
			end
	end,
	{atomic, Result} = mnesia:transaction(Fun),
	Result.


delete(UserId, CategoryId) ->
	case model_nte_category:get(UserId, CategoryId) of
		[] -> error;
		[_Category] ->
			CategoryCount = model_nte_category:count(UserId),
			if 
				CategoryCount < 2 -> last;
				true ->
					Fun = fun() ->
						%delete category
						mnesia:transaction(fun() -> mnesia:delete({nte_category, CategoryId}) end),
						
						% delete note share as well
	                    model_nte_share:delete_by_category_id(CategoryId), 

						%move notes to default category
						DefaultCategoryId = (model_nte_category:get_default_or_any_category(UserId))#nte_category.id,
						model_nte_note:move_to_category(CategoryId, DefaultCategoryId)
					end,

					mnesia:transaction(Fun),
					ok
			end
	end.


get(Id) ->
	Fun = fun() ->
			mnesia:read({nte_category, Id})
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Category]} -> Category;
		_ -> error
	end.


get(UserId, CategoryId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.id =:= CategoryId, 
						X#nte_category.user_id =:= UserId]))
	end,
	{atomic, NoteCategories} = mnesia:transaction(Fun),
	NoteCategories.


get_default_category(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.user_id =:= UserId,
						X#nte_category.is_default =:= true]))
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} -> none;
		{atomic, [Category]} -> Category
	end.


get_any_category(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category),
						X#nte_category.user_id =:= UserId]))
	end,

	{atomic, Categories} = mnesia:transaction(Fun),
	lists:last(Categories).


get_default_or_any_category(UserId) ->
	case model_nte_category:get_default_category(UserId) of
		none ->  model_nte_category:get_any_category(UserId);
		Category -> Category
	end.


clear_is_default(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.user_id =:= UserId]))
	end,
	{atomic, Categories} = mnesia:transaction(Fun),

	ClearFun = fun(Category) ->
		mnesia:transaction(fun() -> 
			mnesia:write(Category#nte_category{is_default=false})
		end)
	end,
	lists:foreach(ClearFun, Categories),

	ok.


count(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
						X#nte_category.user_id =:= UserId]))
	end,
	{atomic, Categories} = mnesia:transaction(Fun),
	erlang:length(Categories).


up(UserId, CategoryId) ->
	case model_nte_category:get(CategoryId) of
		error -> error;
		Category ->
			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
								X#nte_category.user_id =:= UserId, 
								X#nte_category.display_order < Category#nte_category.display_order]))
			end,
			{atomic, MyCategories} = mnesia:transaction(Fun),

			case erlang:length(MyCategories) of
				0 -> ignore;
				1 -> exchange_display_order(Category, lists:last(MyCategories));
				_ ->				
					SortFun = fun(A, B) ->
						if
							A#nte_category.display_order =< B#nte_category.display_order -> true;
							true -> false
						end
					end,
					SortedCategories = lists:sort(SortFun, MyCategories),
					exchange_display_order(Category, lists:last(SortedCategories))
			end,
			ok

	end.


down(UserId, CategoryId) ->
	case model_nte_category:get(CategoryId) of
		error -> error;
		Category ->
			Fun = fun() -> 
				qlc:e(qlc:q([X || X <- mnesia:table(nte_category), 
								X#nte_category.user_id =:= UserId, 
								X#nte_category.display_order > Category#nte_category.display_order]))
			end,
			{atomic, MyCategories} = mnesia:transaction(Fun),

			case erlang:length(MyCategories) of
				0 -> ignore;
				1 -> exchange_display_order(Category, lists:last(MyCategories));
				_ ->				
					SortFun = fun(A, B) ->
						if
							A#nte_category.display_order =< B#nte_category.display_order -> true;
							true -> false
						end
					end,
					SortedCategories = lists:sort(SortFun, MyCategories),
					exchange_display_order(Category, lists:nth(1, SortedCategories))
			end,
			ok
	end.


get_owner(CategoryId) ->
	case model_nte_category:get(CategoryId) of
		error -> error;
		Category -> Category#nte_category.user_id
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================


exchange_display_order(Category1, Category2) ->
	UpdateFun = fun() ->
		TempDisplayOrder = Category1#nte_category.display_order,
		mnesia:write(Category1#nte_category{display_order=Category2#nte_category.display_order}),
		mnesia:write(Category2#nte_category{display_order=TempDisplayOrder})
	end,
	mnesia:transaction(UpdateFun).