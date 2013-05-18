-module(model_gly_item).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/1
		,create_album/2
		,list/2]).


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


%% ===================================================================
%% Local Functions
%% ===================================================================
