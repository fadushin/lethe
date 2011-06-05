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
-module(net_dushin_lethe_channel).
-export(
    [
        start/1, start/2, stop/1, 
        join/2, get_peers/1, get_peers/2, leave/2, ping/2, 
        post_message/2, get_messages/1, get_messages/2
    ]
).
-include("net_dushin_lethe_channel.hrl").
-include("net_dushin_lethe_log.hrl").
-include("net_dushin_lethe_timer.hrl").

%%
%% @type    channel_context() ->
%%              channel_id = atom(),
%%              channel_manager_pid = pid(),
%%              max_length = integer(),
%%              peer_timeout_ms = integer(),
%%              check_interval_ms = integer()
%%
-record(
    channel_context,
    {
        channel_id,
        channel_timeout_ms,
        messages_pid,
        peers_pid,
        timer,
        config
    }
).

-record(
    peer_context,
    {
        channel_id,
        peer_timeout_ms,
        max_peers
    }
).

-record(
    message_context,
    {
        channel_id,
        message_timeout_ms,
        max_messages
    }
).


%%
%% external functions
%%

%%
%% @type    channel_config() -> [option()].
%%
%% @type    option() -> {key(), term()}.
%%
%% @type    key() ->
%%              timeout_ms |            (the default timout to be used on a call through any of the channel APIs.  Default: 1 second)
%%              max_peers |             (the max number of peers allowable on a channel)
%%              peer_timeout_ms |       (ms before an inactive peer is removed from peer list.  Default: 1 min)
%%              max_messages |          (the max number of messages cached on a channel)
%%              message_timeout_ms |    (ms before a message is removed from message list.  Default: 30 mins)
%%              shutdown_handler |      (handler to be called when a channel is shut down due to inactivity.  Default: undefined)
%%              sweep_interval_ms |     (interval between sweeps of stale channel data.  Default: 15 secs)
%%              channel_timeout_ms      (time in ms before which an idle channel will self-destruct)
%%

%% 
%% @spec        start(atom()) -> channel().
%%
%% @effects     start(ChannelId, [])
%% 
start(ChannelId) ->
    start(ChannelId, []).

%% 
%% @spec        start(atom(), channel_config()) -> channel().
%% 
start(ChannelId, Config) ->
    #channel {
        id = ChannelId,
        timeout_ms = net_dushin_lethe_lists:find_value(
            Config, timeout_ms, 1000
        ),
        channel_pid = spawn(
            fun() -> channel_init(ChannelId, Config) end
        )
    }.

%%
%% @spec        stop(channel()) -> ok | {error, timeout}.
%%
stop(Channel) ->
    net_dushin_lethe_rpc:call(Channel#channel.channel_pid, stop, Channel#channel.timeout_ms).

%%
%% @spec        join(channel(), peer()) -> ok | {error, too_many_peers} | {error, timeout}.
%%
join(Channel, Peer) ->
    net_dushin_lethe_rpc:call(get_peers_pid(Channel), {join, Peer}, Channel#channel.timeout_ms).

%%
%% @spec        ping(channel(), atom()) -> ok | {error, peer_does_not_exist} | {error, timeout}.
%%
ping(Channel, PeerName) ->
    net_dushin_lethe_rpc:call(get_peers_pid(Channel), {ping, PeerName}, Channel#channel.timeout_ms).

%%
%% @spec        leave(channel(), atom()) -> ok | {error, timeout}.
%%
leave(Channel, PeerName) ->
    net_dushin_lethe_rpc:call(get_peers_pid(Channel), {leave, PeerName}, Channel#channel.timeout_ms).

%%
%% @spec        get_peers(channel()) -> peer_list() | {error, timeout}.
%%
%% @effects     get_peers(Channel, [])
%%
get_peers(Channel) ->
    Result = get_peers(Channel, []),
    case Result of
        {error, timeout} -> Result; 
        {Add, []} -> Add
    end.

%%
%% @spec        get_peers(channel(), string_list()) -> {peer_list(), string_list()} | {error, timeout}.
%%
get_peers(Channel, PeerNames) ->
    net_dushin_lethe_rpc:call(get_peers_pid(Channel), {get, PeerNames}, Channel#channel.timeout_ms).

%%
%% @spec        post_message(channel(), message()) -> message() | {error, timeout}.
%%
post_message(Channel, Message) ->
    net_dushin_lethe_rpc:call(get_messages_pid(Channel), {post, Message}, Channel#channel.timeout_ms).

%%
%% @spec        get_messages(channel()) -> message_list() | {error, timeout}.
%%
%% @effects     get_messages(Channel, all)
%%
get_messages(Channel) ->
    get_messages(Channel, all).

%%
%% @spec        get_messages(channel(), timestamp() | all) -> message_list() | {error, timeout}.
%%
get_messages(Channel, Since) ->
    net_dushin_lethe_rpc:call(get_messages_pid(Channel), {get, Since}, Channel#channel.timeout_ms).


%%
%% internal functions
%%

channel_init(ChannelId, Config) ->
    process_flag(trap_exit, true),
    PeersPid = spawn_peers(ChannelId, Config),
    MessagesPid = spawn_messages(ChannelId, Config),
    Timer = net_dushin_lethe_timer:start(
        #timer_spec {
            f = fun(Spec) -> 
                net_dushin_lethe_rpc:send(PeersPid, sweep),
                net_dushin_lethe_rpc:send(MessagesPid, sweep),
                Spec
            end,
            timeout_ms = net_dushin_lethe_lists:find_value(
                Config, sweep_interval_ms, 15 * 1000
            ),
            loop = true
        }
    ),
    ?LETHE_INFO("Channel ~p started", [ChannelId]),
    channel_loop(
        #channel_context {
            channel_id = ChannelId,
            timer = Timer,
            peers_pid = PeersPid,
            messages_pid = MessagesPid,
            config = Config,
            channel_timeout_ms = net_dushin_lethe_lists:find_value(
                Config, channel_timeout_ms, 1000 * 60 * 30
            )
        }
    ).

get_peers_pid(Channel) ->
    net_dushin_lethe_rpc:call(Channel#channel.channel_pid, get_peers_pid).

get_messages_pid(Channel) ->
    net_dushin_lethe_rpc:call(Channel#channel.channel_pid, get_messages_pid).

spawn_peers(ChannelId, Config) ->
    spawn_link(
        fun() -> peer_loop(
            #peer_context {
                channel_id = ChannelId,
                peer_timeout_ms = net_dushin_lethe_lists:find_value(
                    Config, peer_timeout_ms, 60 * 1000
                ),
                max_peers = net_dushin_lethe_lists:find_value(
                    Config, max_peers, 25
                )
            },
            []
        ) end
    ).

spawn_messages(ChannelId, Config) ->
    spawn_link(
        fun() -> message_loop(
            #message_context {
                channel_id = ChannelId,
                message_timeout_ms = net_dushin_lethe_lists:find_value(
                    Config, message_timeout_ms, 1000 * 60 * 30
                ),
                max_messages = net_dushin_lethe_lists:find_value(
                    Config, max_messages, 100
                )
            },
            []
        ) end
    ).

channel_loop(Ctx) ->
    ChannelId = Ctx#channel_context.channel_id,
    receive
        %%
        %%
        %%
        {ClientPid, get_peers_pid} ->
            net_dushin_lethe_rpc:response(ClientPid, Ctx#channel_context.peers_pid),
            channel_loop(Ctx);
        %%
        %%
        %%
        {ClientPid, get_messages_pid} ->
            net_dushin_lethe_rpc:response(ClientPid, Ctx#channel_context.messages_pid),
            channel_loop(Ctx);
        %%
        %%
        %%
        {ClientPid, stop} ->
            ?LETHE_INFO("Channel ~p stopping...", [ChannelId]),
            channel_stop(Ctx),
            net_dushin_lethe_rpc:response(ClientPid, ok);
        %%
        %%
        %%
        {'EXIT', Pid, Reason} ->
            ?LETHE_WARNING(
                "An error was trapped by channel loop (~p): Pid=~p; Reason=~p", 
                [ChannelId, Pid, Reason]
            ),
            PeersPid = Ctx#channel_context.peers_pid,
            MessagesPid = Ctx#channel_context.messages_pid,
            %% TODO handle rebooting the timer
            case Pid of
                PeersPid ->
                    channel_loop(
                        Ctx#channel_context {
                            peers_pid = spawn_peers(
                                Ctx#channel_context.channel_id,
                                Ctx#channel_context.config
                            )
                        }
                    );
                MessagesPid ->
                    channel_loop(
                        Ctx#channel_context {
                            messages_pid = spawn_messages(
                                Ctx#channel_context.channel_id,
                                Ctx#channel_context.config
                            )
                        }
                    );
                _ ->
                    throw({internal_error, "Unexpected trap", Pid, Reason})
            end;
        %%
        %%
        %%
        _ ->
            channel_loop(Ctx)
    %%
    %%
    %%
    after Ctx#channel_context.channel_timeout_ms ->
        ?LETHE_INFO("Channel ~p timed out.  Stopping...", [ChannelId]),
        channel_stop(Ctx),
        F = net_dushin_lethe_lists:find_value(
            Ctx#channel_context.config, 
            shutdown_handler,
            fun(_ChannelId) -> ok end
        ),
        F(Ctx#channel_context.channel_id)
    end.

channel_stop(Ctx) ->
    net_dushin_lethe_timer:stop(Ctx#channel_context.timer),
    net_dushin_lethe_rpc:call(Ctx#channel_context.peers_pid, stop),
    net_dushin_lethe_rpc:call(Ctx#channel_context.messages_pid, stop).
    
%%
%%
%%
peer_loop(Ctx, Peers) ->
    ChannelId = Ctx#peer_context.channel_id,
    receive
        %%
        %%
        %%
        {ClientPid, stop} ->
            ?LETHE_DEBUG("Peer Loop ~p stopping", [ChannelId]),
            net_dushin_lethe_rpc:response(ClientPid, ok);
            %% done
        %%
        %%
        %%
        {ClientPid, {join, Peer}} ->
            ?LETHE_INFO("Channel ~p: peer joined: ~p", [ChannelId, Peer#peer.name]),
            case add_peer(Peer, Peers, Ctx#peer_context.max_peers) of
                too_many_peers ->
                    ?LETHE_WARNING(
                        "Too many peers on channel ~p;  Peer ~p rejected", 
                        [ChannelId, Peer#peer.name]
                    ),
                    net_dushin_lethe_rpc:response(ClientPid, {error, too_many_peers}),
                    peer_loop(Ctx, Peers);
                NewPeers ->
                    net_dushin_lethe_rpc:response(ClientPid, ok),
                    peer_loop(Ctx, NewPeers)
            end;
        %%
        %%
        %%
        {ClientPid, {ping, PeerName}} ->
            ?LETHE_DEBUG("ping: PeerName = ~p", [PeerName]),
            UpdatedPeers = update_peer(#peer{name=PeerName}, Peers),
            {Response, NewPeers} = case UpdatedPeers of
                {error, peer_does_not_exist} -> 
                    ?LETHE_WARNING(
                        "Non existent peer pinged on channel ~p;  Peer = ~p", 
                        [ChannelId, PeerName]
                    ),
                    {{error, peer_does_not_exist}, Peers};
                _ -> 
                    {ok, UpdatedPeers}
            end,
            net_dushin_lethe_rpc:response(ClientPid, Response),
            ?LETHE_DEBUG("After ping, NewPeers = ~4p", [NewPeers]),
            peer_loop(Ctx, NewPeers);
        %%
        %%
        %%
        {ClientPid, {get, PeerNames}} ->
            Result = find_peers(PeerNames, Peers),
            net_dushin_lethe_rpc:response(ClientPid, Result),
            peer_loop(Ctx, Peers);
        %%
        %% Remove the peer identified by the specified name from the list of peers
        %%
        {ClientPid, {leave, PeerName}} ->
            ?LETHE_INFO("Channel ~p; peer leaving: ~p", [ChannelId, PeerName]),
            NewPeers = remove_peer(PeerName, Peers),
            net_dushin_lethe_rpc:response(ClientPid, ok),
            peer_loop(Ctx, NewPeers);
        %%
        %% Sweep the peer list for stale peers.  This message
        %% requires no response
        %%
        {_ClientPid, sweep} ->
            NewPeers = filter_stale_peers(Peers, Ctx#peer_context.peer_timeout_ms),
            peer_loop(Ctx, NewPeers);
        %%
        %% Report malformed messages
        %%
        {ClientPid, _} ->
            xrcp:response(ClientPid, {error, unknown_message}),
            peer_loop(Ctx, Peers);
        %%
        %% Ignore any spam
        %%
        _Spam ->
            peer_loop(Ctx, Peers)
    end.

%% 
%% 
%% 
message_loop(Ctx, Messages) ->
    ChannelId = Ctx#message_context.channel_id,
    receive
        %%
        %%
        %%
        {ClientPid, stop} ->
			?LETHE_DEBUG("Message Loop ~p stopping", [ChannelId]),
            net_dushin_lethe_rpc:response(ClientPid, ok);
        %%
        %%
        %%
        {ClientPid, {post, Message}} ->
            NewMessages = [NewMessage | _] = 
                add_message(Message, Messages, Ctx#message_context.max_messages),
            ?LETHE_INFO("Channel ~p; added message ~p", [ChannelId, NewMessage#message.uuid]),
            net_dushin_lethe_rpc:response(ClientPid, NewMessage),
            message_loop(Ctx, NewMessages);
        %%
        %%
        %%
        {ClientPid, {get, Since}} ->
            SinceMessages = lists:reverse(get_messages_since(Messages, Since)),
            net_dushin_lethe_rpc:response(ClientPid, SinceMessages),
            message_loop(Ctx, Messages);
        %%
        %% Sweep the peer list for stale messages.  This message
        %% requires no response
        %%
        {_ClientPid, sweep} ->
            NewMessages = filter_stale_messages(Messages, Ctx#message_context.message_timeout_ms),
            message_loop(Ctx, NewMessages);
        %%
        %% Report malformed messages
        %%
        {ClientPid, _} ->
            xrcp:response(ClientPid, {error, unknown_message}),
            message_loop(Ctx, Messages);
        %%
        %% Ignore any spam
        %%
        _Spam ->
            message_loop(Ctx, Messages)
    end.


add_peer(Peer, Peers, MaxLen) ->
    case length(Peers) < MaxLen of
        true ->
            case update_peer(Peer, Peers) of
                {_, peer_does_not_exist} ->
                    [stamp_peer(Peer) | Peers];
                NewPeers ->
                    NewPeers
            end;
        false ->
            too_many_peers
    end.

add_message(Message, Messages, MaxLen) ->
    NewMessage = stamp_message(Message),
    case length(Messages) < MaxLen of
        true ->
            [NewMessage | Messages];
        false ->
            [NewMessage | remove_last(Messages)]
    end.


remove_last([]) ->
    [];
remove_last(Messages = [_H|_T]) ->
    lists:reverse(tail(lists:reverse(Messages))).

tail([_H|T]) ->
    T.

stamp_message(Message) ->
    Message#message {
        timestamp = net_dushin_lethe_time:current_ms()
    }.


find_peers(PeerNames, Peers) ->
    Add = [Peer || Peer <- Peers, not(lists:member(Peer#peer.name, PeerNames))],
    Remove = [PeerName || PeerName <- PeerNames, not(peer_name_occurs(PeerName, Peers))],
    case Remove of
        [] -> ok;
        NonEmpty -> ?LETHE_DEBUG("The following peers had been removed: ~4p", [NonEmpty])
    end,
    {
        Add,
        Remove
    }.

peer_name_occurs(PeerName, Peers) ->
    
    case lists:filter(fun(Peer) -> Peer#peer.name =:= PeerName end, Peers) of
        [] ->
            false;
        _ ->
            true
    end.

get_messages_since(Messages, Since) ->
    case Since of
        all ->
            Messages;
        _ ->
            get_messages_since([], Messages, Messages, Since)
    end.

get_messages_since(_L, [], All, _Since) ->
    All;
get_messages_since(L, [#message{timestamp = Since} = _H|_T], _All, Since) ->
    lists:reverse(L);
get_messages_since(L, [H|T], All, Since) ->
    get_messages_since([H|L], T, All, Since).


filter_stale_messages(Messages, TimeoutMs) ->
    lists:filter(
        fun(Message) -> 
            Ret = is_not_stale(Message#message.timestamp, TimeoutMs),
            case Ret of
                false ->
                    ?LETHE_INFO("Message ~p is stale -- removing", [Message#message.uuid]);
                _ ->
                    ok
            end,
            Ret
        end, 
        Messages
    ).

stamp_peer(Peer) ->
    Ret = Peer#peer{last_update = net_dushin_lethe_time:current_ms()},
    ?LETHE_DEBUG("Stamped Peer: ~p, ~p~n", [Ret#peer.name, Ret#peer.last_update]),
    Ret.

remove_peer(PeerName, Peers) ->
    remove_peer(PeerName, [], Peers).

remove_peer(_, L, []) ->
    lists:reverse(L);
remove_peer(PeerName, L, [H|T]) ->
    case PeerName =:= H#peer.name of
        true ->
            ?LETHE_DEBUG("peer removed: ~p", [PeerName]),
            stitch(L, T);
        false ->
            remove_peer(PeerName, [H|L], T)
    end.

stitch([], T) ->
    T;
stitch([H|LT], T) ->
    stitch(LT, [H|T]).

update_peer(Peer, Peers) ->
    update_peer(Peer, [], Peers).

update_peer(_Peer, _, []) ->
    {error, peer_does_not_exist};
update_peer(Peer, L, [H|T]) ->
    case peer_matches(Peer, H) of
        true -> stitch(L, [stamp_peer(H) | T]);
        false -> update_peer(Peer, [H|L], T)
    end.

peer_matches(P1, P2) ->
    P1#peer.name =:= P2#peer.name.


filter_stale_peers(Peers, TimeoutMs) ->
    lists:filter(
        fun(Peer) -> 
            Ret = is_not_stale(Peer#peer.last_update, TimeoutMs),
            case Ret of
                false ->
                    ?LETHE_INFO("Peer ~p is stale -- removing...", [Peer#peer.name]);
                _ ->
                    ok
            end,
            Ret
        end, 
        Peers
    ).

is_not_stale(TestMs, TimeoutMs) ->
    (net_dushin_lethe_time:current_ms() - TestMs) < TimeoutMs.
