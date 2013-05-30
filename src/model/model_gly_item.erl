-module(model_gly_item).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_item/1
		,create_album/3
		,get/1
		,get/2
		,get/3
		,get_type/1
		,get_permission/2
		,get_ancestor_path/4
		,list/5
		,rename/3
		,delete/2
		,move/3
		,copy/3
		,get_parent_id/2
		,get_by_parentId/3
		,set_as_cover/2]).


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


%% only have permission can get
get(Id, UserId) ->	
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.id =:= Id]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> 
			case model_gly_share:get_permission(Id, UserId) of
				"deny" ->
					error;
				_ ->
					?MODULE:get(Id)
			end;
		{atomic, [Item]} -> 
			Item
	end.


%% only have permission can get
get(Id, UserId, Type) ->	
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.type =:= Type,
					X#gly_item.id =:= Id]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> 
			case model_gly_share:get_permission(Id, UserId) of
				"deny" ->
					error;
				_ ->
					?MODULE:get(Id)
			end;
		{atomic, [Item]} -> 
			Item
	end.


get_type(Id) ->
	Fun = fun() -> 
		mnesia:read({gly_item, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> error;
		{atomic, [Item]} -> Item#gly_item.type
	end.


get_permission(ItemId, UserId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.id =:= ItemId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> 
			model_gly_share:get_permission(ItemId, UserId);
		{atomic, [_Model]} -> 
			"owner"
	end.


get_ancestor_path(ItemId, UserId, Prefix, IncludeSelf) ->
	case ?MODULE:get_permission(ItemId, UserId) of
		"deny" ->
			Prefix;
		_ ->
			case IncludeSelf of
				true ->	
					Prefix ++ get_ancestor_path_inner(ItemId, "");
				false ->
					Module = ?MODULE:get(ItemId),
					Prefix ++ get_ancestor_path_inner(Module#gly_item.parent_id, "")
			end
	end.


list(UserId, ParentId, Type, OrderAscending, ModifyName) ->
	Items = case ParentId of 
		undefined ->
			Fun = fun() -> 
					qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
							X#gly_item.user_id =:= UserId,
							X#gly_item.type =:= Type,
							X#gly_item.parent_id =:= undefined]))
			end,

			{atomic, Items0} = mnesia:transaction(Fun),
			Items0;
		_ ->
			%% check permission
			case ?MODULE:get_permission(ParentId, UserId) of
				"deny" -> 
					[];
				_ -> 
					Fun = fun() -> 
							qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
									X#gly_item.type =:= Type,
									X#gly_item.parent_id =:= ParentId]))
					end,

					{atomic, Items0} = mnesia:transaction(Fun),
					Items0
			end
	end,

	% sort items by display_order desc
	SortFun = fun(A, B) ->
		if
			A#gly_item.display_order < B#gly_item.display_order -> 
				OrderAscending;
			true -> 
				case OrderAscending of true -> false; _ -> true end
		end
	end,
	Items2 = lists:sort(SortFun, Items),

	case ModifyName of
		false ->
			Items2;
		true ->
			[case model_gly_share:is_sharing(X#gly_item.id) of
				true ->
					X#gly_item{name = X#gly_item.name ++ "[s]"};
				false ->
					X
			end
			|| X <- Items2]
	end.


rename(ItemId, ItemName, UserId) ->
	%% only owner can do rename
	case ?MODULE:get_permission(ItemId, UserId) of
		"owner" ->
			Model0 = ?MODULE:get(ItemId),
			Model = Model0#gly_item{name = ItemName},

			Fun = fun() ->
				mnesia:write(Model)
			end,

			mnesia:transaction(Fun),
			ok;
		_ ->
			error
	end.


delete(ItemId, UserId) ->
	case ?MODULE:get_permission(ItemId, UserId) of
		"owner" ->
			Model = ?MODULE:get(ItemId),
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
			end;
		_ ->
			error
	end.


move(ItemId, TargetItemId, UserId) ->
	%% only owner can move item
	case ?MODULE:get_permission(ItemId, UserId) of
		"owner" ->
			%% have permission to the target item
			case ?MODULE:get_permission(TargetItemId, UserId) of
				"owner" ->
					Model0 = ?MODULE:get(ItemId),
					%% it has already in the target album
					case Model0#gly_item.parent_id =:= TargetItemId of
						true -> 
							error;
						_ ->
							%% can not move in itself
							case Model0#gly_item.id =:= TargetItemId of
								true ->
									error;
								_ ->
									case Model0#gly_item.parent_id =:= undefined of
										true -> 
											%% the last root album can not be moved, or there will be no entrance
											case get_root_album_count(UserId) of
												1 ->
													error;
												_ ->
													Model = Model0#gly_item{parent_id = TargetItemId},
													Fun = fun() ->
														mnesia:write(Model)
													end,
													mnesia:transaction(Fun),
													ok
											end;
										false ->
											Model = Model0#gly_item{parent_id = TargetItemId},
											Fun = fun() ->
												mnesia:write(Model)
											end,
											mnesia:transaction(Fun),
											ok
									end
							end
					end;
				_ ->
					error
			end;
		_ ->
			error
	end.


copy(ItemId, TargetItemId, UserId) ->
	%% only owner can copy item
	case ?MODULE:get_permission(ItemId, UserId) of
		"owner" ->
			%% have permission to the target item
			case ?MODULE:get_permission(TargetItemId, UserId) of
				"owner" ->
					Model0 = ?MODULE:get(ItemId),
					%% it has already in the target album
					case Model0#gly_item.parent_id =:= TargetItemId of
						true ->
							error;
						_ ->
							%% copy file
							Id2 = tools:generate_id(UserId),
							Path = Model0#gly_item.path,

							{ok, OriginalDir} = application:get_env(tools_platform, tool_gallery_original_dir),
				        	FileFullName = lists:flatten(io_lib:format("~s/~s", [OriginalDir, Path])),

				        	Path2 = re:replace(Path, ItemId, Id2, [global, {return, list}]),
				        	FileFullName2 = re:replace(FileFullName, ItemId, Id2, [global, {return, list}]),

				        	file:copy(FileFullName, FileFullName2),

				        	%% create item record
					    	?MODULE:create_item(#gly_item{
					    		id = Id2, 
					    		user_id = UserId, 
					    		parent_id = TargetItemId, 
					    		name = Model0#gly_item.name, 
					    		path = Path2, 
					    		type = Model0#gly_item.type,
					    		mime_type = Model0#gly_item.mime_type
					    	}),

							ok
					end;
				_ ->
					error
			end;
		_ ->
			error
	end.
	

get_parent_id(ItemId, UserId) ->
	%% have permission to get
	case ?MODULE:get(ItemId, UserId) of
		error ->
			error;
		Model ->
			case Model#gly_item.parent_id of
				undefined -> "";
				ParentId -> ParentId
			end
	end.


get_by_parentId(ParentId, UserId, Type) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.parent_id =:= ParentId,
					X#gly_item.type =:= Type]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


set_as_cover(ItemId, UserId) ->
	case ?MODULE:get(ItemId, UserId, "image") of
		error ->
			error;
		Model ->
			case ?MODULE:get_permission(Model#gly_item.parent_id, UserId) of
				"owner" ->
					ParentModel0 = ?MODULE:get(Model#gly_item.parent_id, UserId, "album"),
					ParentModel = ParentModel0#gly_item{path = Model#gly_item.path, mime_type = Model#gly_item.mime_type},
					Fun = fun() ->
						mnesia:write(ParentModel)
					end,
					mnesia:transaction(Fun),
					ok;
				_ ->
					error
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


get_root_album_count(UserId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.user_id =:= UserId,
					X#gly_item.type =:= "album",
					X#gly_item.parent_id =:= undefined]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	erlang:length(Models).


get_ancestor_path_inner(undefined, Names) ->
	Names;
get_ancestor_path_inner(ItemId, Names) ->
	Model = ?MODULE:get(ItemId),
	Names2 = Model#gly_item.name ++ "/" ++ Names,
	get_ancestor_path_inner(Model#gly_item.parent_id, Names2).
