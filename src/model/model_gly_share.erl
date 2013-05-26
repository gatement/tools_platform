-module(model_gly_share).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get_permission/2, get_item_ids/1]).


%% ===================================================================
%% API functions
%% ===================================================================

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

%% ===================================================================
%% Local Functions
%% ===================================================================

