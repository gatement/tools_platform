-module(notfound).
-include("yaws_api.hrl").
-export([out404/3]).


%% ===================================================================
%% API functions
%% ===================================================================

out404(_Arg, _GC, _SC) ->
	[{status, 404},
	{ehtml,
		{html, [],
			[
				{h3,[], "Not Found"},
		 		{p, [], "The requested URL was not found on this server or you don't have permission."},
				{a, [{href, "/"}], "home"}
			]
		}
	}].


%% ===================================================================
%% Local Functions
%% ===================================================================
