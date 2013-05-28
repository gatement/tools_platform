-module(model_gly_share).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, list/2, get_permission/2, get_item_ids/1, exist/2]).


%% ===================================================================
%% API functions
%% ===================================================================

create(Model) ->
	case ?MODULE:exist(Model#gly_share.user_id, Model#gly_share.item_id) of
		true -> duplicate;
		false -> 
			Fun = fun() ->
				Id = Model#gly_share.user_id ++ Model#gly_share.item_id,
				mnesia:write(Model#gly_share{id = Id})	  
			end,

			case mnesia:transaction(Fun) of
				{atomic, ok} -> ok;
				_ -> error
			end
	end.


list(ItemId, UserId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_share),
					X#gly_share.item_id =:= ItemId,
					X#gly_share.user_id =:= UserId]))
	end,

	{atomic, Models} = mnesia:transaction(Fun),
	Models.


get_permission(undefined, _UserId) ->
	"deny";

get_permission(ItemId, UserId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_share),
					X#gly_share.item_id =:= ItemId,
					X#gly_share.user_id =:= UserId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> 
			Item = model_gly_item:get(ItemId),
			get_permission(Item#gly_item.parent_id, UserId);
		{atomic, [Model]} -> Model#gly_share.share_type
	end.


get_item_ids(UserId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_share),
					X#gly_share.user_id =:= UserId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> [];
		{atomic, Models} -> [X#gly_share.item_id || X <- Models]
	end.


exist(UserId, ItemId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(gly_share), 
						X#gly_share.user_id =:= UserId, 
						X#gly_share.item_id =:= ItemId]))
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, _Models} -> true
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================

