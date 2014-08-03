%% export gallery items
%% set SRC_DIR and all lgh:do(), gallery items will locate at the DST_DIR

-module(lgh).
-compile([export_all]).
-include_lib("stdlib/include/qlc.hrl").
-record(gly_item, {id, user_id, parent_id, name, path, type, mime_type, display_order, created}).

-define(SRC_DIR, "/Users/gatement/app/tools_platform/priv/original").
-define(DST_DIR, "/Users/gatement/app/tools_platform/priv/img").

%% ===================================================================
%% API functions
%% ===================================================================
get_uids() ->
    TableName = gly_item,
	AllKeys = mnesia:dirty_all_keys(TableName),
    Fun = fun(Key, Uids) ->
            [Model] = mnesia:dirty_read(TableName, Key),
            Uid = Model#gly_item.user_id,
            case lists:member(Uid, Uids) of
                true -> Uids;
                false -> [Uid | Uids]
            end
          end,
    _Uids = lists:foldl(Fun, [], AllKeys).

%% return: ["application","album","video","image"]
get_types() ->
    TableName = gly_item,
	AllKeys = mnesia:dirty_all_keys(TableName),
    Fun = fun(Key, Types) ->
            [Model] = mnesia:dirty_read(TableName, Key),
            Type = Model#gly_item.type,
            case lists:member(Type, Types) of
                true -> Types;
                false -> [Type | Types]
            end
          end,
    _Types = lists:foldl(Fun, [], AllKeys).

get_top_items() ->
    get_by_parent_id(undefined).

get_by_parent_id(ParentId) ->
	Fun = fun() -> 
			qlc:e(qlc:q([X || X <- mnesia:table(gly_item),
					X#gly_item.parent_id =:= ParentId]))
	end,

    {atomic, Results} = mnesia:transaction(Fun),
    Results.

doing(Model, ParentDir) ->
    Type = Model#gly_item.type,
    Id = Model#gly_item.id,
    ParentId = Model#gly_item.parent_id,
    Name = Model#gly_item.name,
    Path = Model#gly_item.path,
    Uid = Model#gly_item.user_id,
    io:format("doing: ~p~n", [Name]),
    case Type of
        "album" ->
            %% recursive sons
            ParentDir2 = case ParentId of
                             undefined ->
                                io_lib:format("~s/~s/~s/", [ParentDir, Uid, Name]);
                             _ ->
                                io_lib:format("~s/~s/", [ParentDir, Name])
                         end,
            lists:foreach(fun(Item) -> doing(Item, ParentDir2) end
                          , get_by_parent_id(Id)),
            ok;
        _ ->
            SrcPath = io_lib:format("~s/~s", [?SRC_DIR, Path]),
            DstPath = io_lib:format("~s/~s", [ParentDir, Name]),
            %%% ensure dir
            filelib:ensure_dir(DstPath),
            %%% copy
            file:copy(SrcPath, DstPath),
            ok
    end,
    ok.

do() -> 
    TopItems = get_top_items(),
    lists:foreach(fun(Model) -> doing(Model, ?DST_DIR) end, TopItems),
    ok.
