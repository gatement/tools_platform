-module(model_gly_item).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_item/1
		,create_album/3
		,get/1
		,get/2
		,list/4
		,rename/3
		,delete/2
		,move/3
		,get_parent_id/2]).


%% ===================================================================
%% API functions
%% ===================================================================

create_item(Model0) ->
	DisplayOrder = tools:epoch_milli_seconds(),
	Created = erlang:universaltime(),

	Model = Model0#gly_item{
					display_order = DisplayOrder,
					created = Created},

	Fun = fun() ->
			mnesia:write(Model)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Model;
		_ -> error
	end.


create_album(AlbumName, ParentId, UserId) ->
	Id = tools:generate_id(UserId),
	DisplayOrder = tools:epoch_milli_seconds(),
	Type = "album",
	Created = erlang:universaltime(),

	Album = #gly_item{id = Id, 
					parent_id = ParentId,
					user_id = UserId, 
					name = AlbumName, 
					type = Type, 
					display_order = DisplayOrder,
					created = Created},

	Fun = fun() ->
		mnesia:write(Album)
	end,

	mnesia:transaction(Fun).


get(Id) ->
	Fun = fun() -> 
		mnesia:read({gly_item, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Item]} -> Item
	end.


get(Id, UserId) ->	
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.id =:= Id]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Item]} -> Item
	end.


list(UserId, ParentId, Type, OrderAscending) ->	
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.type =:= Type,
					X#gly_item.parent_id =:= ParentId]))
	end,

	{atomic, Items} = mnesia:transaction(Fun),

	% sort items by display_order desc
	SortFun = fun(A, B) ->
		if
			A#gly_item.display_order < B#gly_item.display_order -> 
				OrderAscending;
			true -> 
				case OrderAscending of true -> false; _ -> true end
		end
	end,
	lists:sort(SortFun, Items).


rename(ItemId, ItemName, UserId) ->
	case ?MODULE:get(ItemId, UserId) of
		error ->
			error;
		Model0 ->
			Model = Model0#gly_item{name = ItemName},

			Fun = fun() ->
				mnesia:write(Model)
			end,

			mnesia:transaction(Fun),
			ok
	end.


delete(ItemId, UserId) ->
	case ?MODULE:get(ItemId, UserId) of
		error ->
			error;
		Model ->
			case Model#gly_item.type of
				"album" ->
					case is_album_empty(ItemId) of
						true ->
							mnesia:transaction(fun() -> 
								mnesia:delete({gly_item, ItemId})
							end);
						_ ->
							error
					end;
				_ ->
					%% delete correlation file if it is not album
					Path = Model#gly_item.path,
					delete_file(Path, UserId),

					mnesia:transaction(fun() -> 
						mnesia:delete({gly_item, ItemId})
					end)
			end
	end.


move(ItemId, TargetItemId, UserId) ->
	case ?MODULE:get(ItemId, UserId) of
		error ->
			error;
		Model ->
			case ?MODULE:get(TargetItemId, UserId) of
				error ->
					error;
				TargetModel ->
					Model = Model#gly_item{parent_id = TargetModel#gly_item.id},

					Fun = fun() ->
						mnesia:write(Model)
					end,

					mnesia:transaction(Fun),
					ok
			end
	end.
	

get_parent_id(ItemId, UserId) ->
	case ?MODULE:get(ItemId, UserId) of
		error ->
			error;
		Model ->
			case Model#gly_item.parent_id of
				undefined -> "";
				ParentId -> ParentId
			end
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================

delete_file(Path, UserId) ->
	{ok, OriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
	OriginalFile = io_lib:format("~s/~s/~s", [OriginalDir, UserId, Path]),
	file:delete(OriginalFile).


is_album_empty(ItemId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.parent_id =:= ItemId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> true;
		{atomic, _Models} -> false
	end.
