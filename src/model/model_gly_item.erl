-module(model_gly_item).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/1
		,get/2
		,create_album/2
		,list/2
		,rename/3
		,delete/2
		,move/3
		,get_parent_id/2]).


%% ===================================================================
%% API functions
%% ===================================================================

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


create_album(AlbumName, UserId) ->
	Id = tools:generate_id(UserId),
	DisplayOrder = tools:epoch_milli_seconds(),
	Type = "album",
	Created = erlang:universaltime(),

	Album = #gly_item{id = Id, 
							user_id = UserId, 
							name = AlbumName, 
							type = Type, 
							display_order = DisplayOrder,
							created = Created},

	Fun = fun() ->
		mnesia:write(Album)
	end,

	mnesia:transaction(Fun).


list(UserId, ParentId) ->	
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.parent_id =:= ParentId]))
	end,

	{atomic, Items} = mnesia:transaction(Fun),

	% sort items by display_order desc
	SortFun = fun(A, B) ->
		if
			A#gly_item.display_order > B#gly_item.display_order -> true;
			true -> false
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
			Fun = fun() ->
				%% delete correlation file if it is not album
				case Model#gly_item.type of
					"album" ->
						do_nothing;
					_ ->
						Path = Model#gly_item.path,
						delete_file(Path, UserId)
				end,

				mnesia:delete({gly_item, Model#gly_item.id})
			end,

			mnesia:transaction(Fun),
			ok
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
