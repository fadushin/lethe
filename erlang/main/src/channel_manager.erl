%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of dushin.net nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY dushin.net ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL dushin.net BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
-module(channel_manager).
-export([start/0, stop/1, get_channel/2]).

%
% external functions
%

start() ->
    spawn(fun() -> loop() end).

stop(ChannelManagerPid) ->
    xrpc:call(ChannelManagerPid, {halt}).

get_channel(ChannelManagerPid, ChannelId) ->
    xrpc:call(ChannelManagerPid, {get, ChannelId}).


%
% internal functions
%

loop() ->
    receive
        {ClientPid, {remove, ChannelId}} ->
            %io:format("channel_manager:loop {remove ~p}~n", [ChannelId]),
            erase(ChannelId),
            %io:format("channel_manager:loop ~p erased~n", [ChannelId]),
            xrpc:response(ClientPid, ok),
            loop();
        {ClientPid, {halt}} ->
            stop_channels(),
            xrpc:response(ClientPid, ok);
        {ClientPid, {get, ChannelId}} ->
            %io:format("channel_manager:loop {get ~p}~n", [ChannelId]),
            xrpc:response(ClientPid, get_or_create(ChannelId)),
            loop();
        {ClientPid, UnsupportedMessage} ->
            xrpc:response(ClientPid, unsupported_message),
            loop();
        Spam ->
            io:format("Unsolicited call to channel manager: ~p; discarding.~n", [Spam]),
            loop()
    end.

get_or_create(ChannelId) ->
    value(ChannelId, get(ChannelId)).

value(ChannelId, undefined) ->
    %io:format("value undefined.  Channel: ~p~n", [ChannelId]),
    ChannelPid = channel:start(ChannelId, self()),
    put(ChannelId, ChannelPid),
    ChannelPid;
value(_ChannelId, X) ->
    %io:format("value defined: ~p~n", [X]),
    X.

stop_channels() ->
    %% TODO
    ok.
