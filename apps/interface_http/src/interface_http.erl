-module(interface_http).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->    
	application:start(crypto),
	application:start(asn1),
	application:start(public_key),
	application:start(ssl),
	ok = application:start(interface_http),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
