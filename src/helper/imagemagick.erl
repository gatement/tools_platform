-module(imagemagick).
-export([convert/4]).

-vsn("0.1.0").

%% ===================================================================
%% API functions
%% ===================================================================

convert(Exe, Source, Target, Height) ->
	Cmd = lists:flatten(io_lib:format("~s ~s -thumbnail \"x~p\" ~s", [Exe, Source, Height, Target])),
	Result = os:cmd(Cmd),
    %error_logger:info_msg("imagemagick convert: ~p~n", [Cmd]),
    Result.

    
%% ===================================================================
%% Local Functions
%% ===================================================================
