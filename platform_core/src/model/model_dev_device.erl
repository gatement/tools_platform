-module(model_dev_device).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
		get/1,
		get_by_sn/1,
		all_keys/0]).

%% ===================================================================
%% API functions
%% ===================================================================

create(Model) ->
	Fun = fun() ->
		mnesia:write(Model)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Model;
		_ -> error
	end.


get(Id) ->
	Fun = fun() ->
		mnesia:read(dev_device, Id)
	end,

	case mnesia:transaction(Fun) of
        {atomic, []} -> undefined;
		{atomic, [Model]} -> Model;
		_ -> error
	end.


get_by_sn(Sn) ->
    Fun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_device), 
                          X#dev_device.sn =:= Sn]))
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Models} -> Models;
        _ -> error
    end.


all_keys() ->
	Fun = fun() ->
		mnesia:all_keys(dev_device)
	end,

	case mnesia:transaction(Fun) of
		{atomic, Keys} -> Keys;
		_ -> error
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
