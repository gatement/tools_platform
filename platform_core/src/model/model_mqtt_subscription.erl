-module(model_mqtt_subscription).
-include("tools_platform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create/1]).

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


%% ===================================================================
%% Local Functions
%% ===================================================================
