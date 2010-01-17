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
-module(channel_client).

-export([start/0, start/1, stop/1]).

-include("net_dushin_lethe_channel.hrl").
-include("net_dushin_lethe_timer.hrl").

-record(
    state,
    {
        channel_id,
        peer,
        peer_pinger,
        peer_getter,
        message_getter,
        message_poster
    }
).

start() -> start([]).

start(Config) ->
    spawn(
        fun() -> init(Config) end
    ).

stop(ClientId) ->
    xrpc:call(ClientId, stop).

init(Config) ->
    ChannelId = net_dushin_lethe_lists:find_value(
        Config, channel_id, undefined
    ),
    Peer = #peer {
        name = list_to_atom(uuid:generate_string())
    },
    net_dushin_lethe_server:join(ChannelId, Peer),
    loop(
        #state {
            channel_id = ChannelId,
            peer = Peer,
            peer_pinger = net_dushin_lethe_timer:start(
                #timer_spec {
                    f = fun(Spec) -> 
                        net_dushin_lethe_server:ping(ChannelId, Peer#peer.name),
                        Spec
                    end,
                    timeout_ms = net_dushin_lethe_lists:find_value(
                        Config, ping_interval_ms, 5003
                    ),
                    loop = true
                }
            ),
            peer_getter = net_dushin_lethe_timer:start(
                #timer_spec {
                    f = fun(Spec) -> get_peers(Spec, ChannelId) end,
                    timeout_ms = net_dushin_lethe_lists:find_value(
                        Config, get_peers_interval_ms, 4501
                    ),
                    loop = true,
                    bag = []
                }
            ),
            message_getter = net_dushin_lethe_timer:start(
                #timer_spec {
                    f = fun(Spec) -> get_messages(Spec, ChannelId) end,
                    timeout_ms = net_dushin_lethe_lists:find_value(
                        Config, get_mesages_interval_ms, 2501
                    ),
                    loop = true,
                    bag = []
                }
            ),
            message_poster = net_dushin_lethe_timer:start(
                #timer_spec {
                    f = fun(Spec) -> maybe_post_message(Spec, ChannelId) end,
                    timeout_ms = net_dushin_lethe_lists:find_value(
                        Config, maybe_post_message_interval_ms, 7243
                    ),
                    loop = true,
                    bag = []
                }
            )
        }
    ).


get_peers(Spec, ChannelId) ->
    PeerNames = Spec#timer_spec.bag,
    io:format("Peer Getter: current peer names = ~p~n", [PeerNames]),
    {Add, Remove} = net_dushin_lethe_server:get_peers(ChannelId, PeerNames),
    Spec#timer_spec {
        bag = reconcile({Add, Remove}, PeerNames)
    }.

reconcile({Add, Remove}, PeerNames) ->
    add_peers(Add, remove_peers(Remove, PeerNames)).

remove_peers(Remove, PeerNames) ->
    lists:filter(fun(PeerName) -> not(lists:member(PeerName, Remove)) end, PeerNames).

add_peers(Peers, PeerNames) ->
    PeerNames ++ lists:map(fun(Peer) -> Peer#peer.name end, Peers).

get_messages(Spec, ChannelId) ->
    CurrentMessages = Spec#timer_spec.bag,
    io:format("Message Getter: current messages = ~p~n", [CurrentMessages]),
    NewMessages = case CurrentMessages of
        [] ->
            net_dushin_lethe_server:get_messages(ChannelId);
        [H|_T] ->
            net_dushin_lethe_server:get_messages(ChannelId, H#message.timestamp)
    end,
    Spec#timer_spec {
        bag = NewMessages ++ CurrentMessages
    }.
    
maybe_post_message(Spec, ChannelId) ->
    R = random:uniform(1),
    io:format("maybe_post_message: R = ~p~n", [R]),
    case random:uniform(2) of
        1 ->
            io:format("Message Poster: posting message...~n", []),
            net_dushin_lethe_server:post_message(ChannelId, #message{});
        2 ->
            ok
    end,
    Spec.


loop(State) ->
    receive
        {ClientPid, stop} ->
            net_dushin_lethe_timer:stop(State#state.peer_pinger),
            net_dushin_lethe_timer:stop(State#state.peer_getter),
            net_dushin_lethe_timer:stop(State#state.message_getter),
            net_dushin_lethe_timer:stop(State#state.message_poster),
			Peer = State#state.peer,
            net_dushin_lethe_server:leave(State#state.channel_id, Peer#peer.name),
            net_dushin_lethe_rpc:response(ClientPid, ok)
    end.
