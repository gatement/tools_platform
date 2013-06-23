-module(model_dev_status).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([get/2, 
        update/3,
        get_by_key/2,
        get_all_values_by_deviceId/1, 
        get_online_device_ids/0]).


%% ===================================================================
%% API functions
%% ===================================================================

get(DeviceId, Key) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.device_id =:= DeviceId, 
                          X#dev_status.key =:= Key]))
    end,
    
    case mnesia:transaction(ReadFun) of
        [] -> undefined;
        [Model] -> Model;
        _ -> error
    end.


update(DeviceId, Key, Data) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.device_id =:= DeviceId, 
                          X#dev_status.key =:= Key]))
    end,
    
    Model1 = case mnesia:transaction(ReadFun) of
        {atomic, []} -> undefined;
        {atomic, [Model0]} -> Model0;
        _ -> error
    end,

    Model2 = case Model1 of
        error ->
            erlang:exit(mnesia_error);
        undefined ->
            #dev_status{
                id = uuid:to_string(uuid:uuid1()),
                device_id = DeviceId, 
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


get_by_key(DeviceId, Key) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.device_id =:= DeviceId, 
                          X#dev_status.key =:= Key]))
    end,
    
    case mnesia:transaction(ReadFun) of
        {atomic, []} -> undefined;
        {atomic, [Model]} -> Model#dev_status.value;
        _ -> error
    end.


get_all_values_by_deviceId(DeviceId) ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_status), 
                          X#dev_status.device_id =:= DeviceId]))
    end,
    
    {atomic, Models} = mnesia:transaction(ReadFun),

    [{X#dev_status.key, X#dev_status.value} || X <- Models].


get_online_device_ids() ->
    ReadFun = fun() -> 
        qlc:e(qlc:q([X#dev_status.device_id || X <- mnesia:table(dev_status), 
                          X#dev_status.key =:= "online",
                          X#dev_status.value =:= true]))
    end,
    
    {atomic, DeviceIds} = mnesia:transaction(ReadFun),

    DeviceIds.


%% ===================================================================
%% Local Functions
%% ===================================================================
