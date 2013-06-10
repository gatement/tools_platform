-module(model_dev_status).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/2, 
        update/3,
        get_by_key/2,
        get_all_values_by_sn/1]).


%% ===================================================================
%% API functions
%% ===================================================================

get(Sn, Key) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.sn =:= Sn, 
                          X#dev_status.key =:= Key]))
    end,
    
    case mnesia:transaction(ReadFun) of
        [] -> undefined;
        [Model] -> Model;
        _ -> error
    end.


update(Sn, Key, Data) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.sn =:= Sn, 
                          X#dev_status.key =:= Key]))
    end,
    
    Model1 = case mnesia:transaction(ReadFun) of
        {atomic, []} -> undefined;
        {atomic, [Model0]} -> Model0;
        _ -> error
    end,

    Model2 = case Model1 of
        undefined ->
            #dev_status{
                id = uuid:to_string(uuid:uuid1()), 
                sn = Sn, 
                key = Key, 
                value = Data,
                updated = tools:datetime_string('yyyyMMdd_hhmmss')
            };
        Model ->
            Model#dev_status{value = Data, updated = tools:datetime_string('yyyyMMdd_hhmmss')}
    end,

    WriteFun = fun() ->
        mnesia:write(Model2)   
    end,

    case mnesia:transaction(WriteFun) of
        {atomic, ok} -> Model2;
        _ -> error
    end.


get_by_key(Sn, Key) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.sn =:= Sn, 
                          X#dev_status.key =:= Key]))
    end,
    
    case mnesia:transaction(ReadFun) of
        {atomic, []} -> undefined;
        {atomic, [Model]} -> Model#dev_status.value;
        _ -> error
    end.


get_all_values_by_sn(Sn) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.sn =:= Sn]))
    end,
    
    case mnesia:transaction(ReadFun) of
        {atomic, []} -> undefined;
        {atomic, [Model]} -> {Model#dev_status.key, Model#dev_status.value};
        _ -> error
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
