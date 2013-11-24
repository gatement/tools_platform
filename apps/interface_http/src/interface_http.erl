-module(interface_http).
-export([start/0]).


%% ===================================================================
%% application callbacks
%% ===================================================================

start() ->    
	ok = application:start(crypto),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(interface_http),

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================
