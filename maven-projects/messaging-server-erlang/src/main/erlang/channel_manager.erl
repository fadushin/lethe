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

-include("channel.hrl").
-include("channel_manager.hrl").

%
% external functions
%

start() ->
    #channel_manager {
        channel_manager_pid = spawn(fun() -> loop() end)
    }.

stop(ChannelManager) ->
    xrpc:call(
        ChannelManager#channel_manager.channel_manager_pid, 
        stop
    ).

get_channel(ChannelManager, ChannelId) ->
    xrpc:call(
        ChannelManager#channel_manager.channel_manager_pid, 
        {get, ChannelId}
    ).


%
% internal functions
%

loop() ->
    receive
        %%
        %%
        %%
        {ClientPid, {remove, ChannelId}} ->
            erase(ChannelId),
            xrpc:response(ClientPid, ok),
            loop();
        %%
        %%
        %%
        {ClientPid, stop} ->
            stop_channels(),
            xrpc:response(ClientPid, ok);
        %%
        %%
        %%
        {ClientPid, {get, ChannelId}} ->
            xrpc:response(ClientPid, get_or_create(ChannelId)),
            loop();
        %%
        %%
        %%
        {ClientPid, _} ->
            xrpc:response(ClientPid, unsupported_message),
            loop();
        %%
        %%
        %%
        _Spam ->
            loop()
    end.

get_or_create(ChannelId) ->
    value(ChannelId, get(ChannelId)).

value(ChannelId, undefined) ->
    Me = self(),
    Channel = channel:start(
        ChannelId, 
        fun(Id) -> xrpc:call(Me, {remove, Id}) end
    ),
    put(ChannelId, Channel),
    Channel;
value(_ChannelId, X) ->
    X.

stop_channels() ->
    %% TODO
    ok.
