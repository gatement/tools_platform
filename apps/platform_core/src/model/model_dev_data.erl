-module(model_dev_data).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1, 
		get_by_deviceId/1]).

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


get_by_deviceId(DeviceId) ->
    Fun = fun() -> 
        qlc:e(qlc:q([X || X <- mnesia:table(dev_data), 
                          X#dev_data.device_id =:= DeviceId]))
    end,
    
    case mnesia:transaction(Fun) of
        {atomic, Models} -> Models;
        _ -> error
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
