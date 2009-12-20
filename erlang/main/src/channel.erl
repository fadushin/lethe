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
-module(channel).
-export(
    [
        start/1, start/2, stop/1, 
        join/2, get_peers/1, get_peers/2, leave/2, ping/2, 
        post_message/2, get_messages/1, get_messages/2
    ]
).
-include("channel.hrl").
-include("xtimer.hrl").

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
        channel_timeout_ms = 1800000,
        messages_pid,
        peers_pid,
        shutdown_handler,
        timer
    }
).

-record(
    peer_context,
    {
        channel_id,
        peer_timeout_ms = 15000,
        max_length = 100
    }
).

-record(
    message_context,
    {
        channel_id,
        message_timeout_ms = 1800000,
        max_length = 100
    }
).


%%
%% external functions
%%

%% 
%% @spec        start(string()) -> channel().
%%
%% @effects     start(ChannelId, undefined)
%% 
start(ChannelId) ->
    start(ChannelId, undefined).

%% 
%% @spec        start(string(), func()) -> channel().
%% 
start(ChannelId, ShutdownHandler) ->
    #channel {
        id = ChannelId,
        channel_pid = spawn(
            fun() -> channel_init(ChannelId, ShutdownHandler) end
        )
    }.

%%
%% @spec        stop(channel()) -> ok | {error, timeout}.
%%
stop(Channel) ->
    xrpc:call(Channel#channel.channel_pid, stop).

%%
%% @spec        join(channel(), peer()) -> ok | {error, too_many_peers} | {error, timeout}.
%%
join(Channel, Peer) ->
    xrpc:call(get_peers_pid(Channel), {join, Peer}).

%%
%% @spec        ping(channel(), string()) -> ok.
%%
ping(Channel, PeerName) ->
    xrpc:call(get_peers_pid(Channel), {ping, PeerName}).

%%
%% @spec        leave(channel(), string()) -> ok.
%%
leave(Channel, PeerName) ->
    xrpc:call(get_peers_pid(Channel), {leave, PeerName}).

%%
%% @spec        get_peers(channel()) -> {peer_list(), string_list()}.
%%
%% @effects     get_peers(Channel, [])
%%
get_peers(Channel) ->
    get_peers(Channel, []).

%%
%% @spec        get_peers(channel(), string_list()) -> {peer_list(), string_list()}.
%%
get_peers(Channel, PeerNames) ->
    xrpc:call(get_peers_pid(Channel), {get, PeerNames}).

%%
%% @spec        post_message(channel(), message()) -> message().
%%
post_message(Channel, Message) ->
    xrpc:call(get_messages_pid(Channel), {post, Message}).

%%
%% @spec        get_messages(channel()) -> message_list().
%%
%% @effects     get_messages(Channel, all)
%%
get_messages(Channel) ->
    get_messages(Channel, all).

%%
%% @spec        get_messages(channel(), timestamp() | all) -> message_list().
%%
get_messages(Channel, Since) ->
    xrpc:call(get_messages_pid(Channel), {get, Since}).


%%
%% internal functions
%%

channel_init(ChannelId, ShutdownHandler) ->
    process_flag(trap_exit, true),
    PeersPid = spawn_peers(ChannelId),
    MessagesPid = spawn_messages(ChannelId),
    Timer = xtimer:start(
        #timer_spec {
            f = fun() -> 
                xrpc:send(PeersPid, sweep),
                xrpc:send(MessagesPid, sweep)
            end,
            timeout_ms = 15000,
            loop = true
        }
    ),
    channel_loop(
        #channel_context {
            timer = Timer,
            peers_pid = PeersPid,
            messages_pid = MessagesPid,
            shutdown_handler = ShutdownHandler
        }
    ).

get_peers_pid(Channel) ->
    xrpc:call(Channel#channel.channel_pid, get_peers_pid).

get_messages_pid(Channel) ->
    xrpc:call(Channel#channel.channel_pid, get_messages_pid).

spawn_peers(ChannelId) ->
    spawn_link(
        fun() -> peer_loop(
            #peer_context {
                channel_id = ChannelId
            },
            []
        ) end
    ).

spawn_messages(ChannelId) ->
    spawn_link(
        fun() -> message_loop(
            #message_context {
                channel_id = ChannelId
            },
            []
        ) end
    ).

channel_loop(Ctx) ->
    receive
        %%
        %%
        %%
        {ClientPid, get_peers_pid} ->
            xrpc:response(ClientPid, Ctx#channel_context.peers_pid),
            channel_loop(Ctx);
        %%
        %%
        %%
        {ClientPid, get_messages_pid} ->
            xrpc:response(ClientPid, Ctx#channel_context.messages_pid),
            channel_loop(Ctx);
        %%
        %%
        %%
        {ClientPid, stop} ->
            channel_stop(Ctx),
            xrpc:response(ClientPid, ok);
        %%
        %%
        %%
        {'EXIT', Pid, Reason} ->
            PeersPid = Ctx#channel_context.peers_pid,
            MessagesPid = Ctx#channel_context.messages_pid,
            %% TODO handle rebooting the timer
            case Pid of
                PeersPid ->
                    channel_loop(
                        Ctx#channel_context {
                            peers_pid = spawn_peers(Ctx#channel_context.channel_id)
                        }
                    );
                MessagesPid ->
                    channel_loop(
                        Ctx#channel_context {
                            messages_pid = spawn_messages(Ctx#channel_context.channel_id)
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
        channel_stop(Ctx),
        case Ctx#channel_context.shutdown_handler of
            undefined ->
                done;
            F ->
                F()
        end
        %% done
    end.

channel_stop(Ctx) ->
    xtimer:stop(Ctx#channel_context.timer),
    xrpc:call(Ctx#channel_context.peers_pid, stop),
    xrpc:call(Ctx#channel_context.messages_pid, stop).
    
%%
%%
%%
peer_loop(Ctx, Peers) ->
    receive
        %%
        %%
        %%
        {ClientPid, stop} ->
            xrpc:response(ClientPid, ok);
            %% done
        %%
        %%
        %%
        {ClientPid, {join, Peer}} ->
            case add_peer(Peer, Peers, Ctx#peer_context.max_length) of
                too_many_peers ->
                    xrpc:response(ClientPid, {error, too_many_peers}),
                    peer_loop(Ctx, Peers);
                NewPeers ->
                    xrpc:response(ClientPid, ok),
                    peer_loop(Ctx, NewPeers)
            end;
        %%
        %%
        %%
        {ClientPid, {ping, PeerName}} ->
            NewPeers = update_peer(#peer{name=PeerName}, Peers),
            xrpc:response(ClientPid, ok),
            peer_loop(Ctx, NewPeers);
        %%
        %%
        %%
        {ClientPid, {get, PeerNames}} ->
            xrpc:response(ClientPid, find_peers(PeerNames, Peers)),
            peer_loop(Ctx, Peers);
        %%
        %% Remove the peer identified by the specified name from the list of peers
        %%
        {ClientPid, {leave, PeerName}} ->
            NewPeers = remove_peer(PeerName, Peers),
            xrpc:response(ClientPid, ok),
            peer_loop(Ctx, NewPeers);
        %%
        %% Sweep the peer list for stale peers.  This message
        %% requires no response
        %%
        {_ClientPid, sweep} ->
            NewPeers = check_stale_peers(Peers, Ctx#peer_context.peer_timeout_ms),
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
    receive
        %%
        %%
        %%
        {ClientPid, stop} ->
            xrpc:response(ClientPid, ok);
        %%
        %%
        %%
        {ClientPid, {post, Message}} ->
            NewMessages = [NewMessage | _] = 
                add_message(Message, Messages, Ctx#message_context.max_length),
            xrpc:response(ClientPid, NewMessage),
            message_loop(Ctx, NewMessages);
        %%
        %%
        %%
        {ClientPid, {get, Since}} ->
            SinceMessages = get_messages_since(Messages, Since),
            xrpc:response(ClientPid, SinceMessages),
            message_loop(Ctx, Messages);
        %%
        %% Sweep the peer list for stale messages.  This message
        %% requires no response
        %%
        {_ClientPid, sweep} ->
            NewMessages = check_stale_messages(Messages, Ctx#message_context.message_timeout_ms),
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
                {error, _} ->
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
        timestamp = erlang:now()
    }.


find_peers(PeerNames, Peers) ->
    {
        [Peer || Peer <- Peers, not(lists:member(Peer#peer.name, PeerNames))],
        [PeerName || PeerName <- PeerNames, not(peer_name_occurs(PeerName, Peers))]
    }.

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


check_stale_messages(Messages, TimeoutMs) ->
    lists:filter(
        fun(Message) -> 
            delta_ms(Message#message.timestamp, erlang:now()) < TimeoutMs 
        end, 
        Messages
    ).

peer_name_occurs(PeerName, Peers) ->
    case lists:filter(fun(Peer) -> Peer#peer.name =:= PeerName end, Peers) of
        [] ->
            false;
        _ ->
            true
    end.

stamp_peer(Peer) ->
    Peer#peer{last_update = erlang:now()}.

remove_peer(PeerName, Peers) ->
    remove_peer(PeerName, [], Peers).

remove_peer(_, L, []) ->
    lists:reverse(L);
remove_peer(PeerName, L, [H|T]) ->
    case PeerName =:= H#peer.name of
        true ->
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
        false -> update_peer(Peer, L, T)
    end.

peer_matches(P1, P2) ->
    P1#peer.name =:= P2#peer.name.


check_stale_peers(Peers, TimeoutMs) ->
    lists:filter(fun(Peer) -> is_not_stale(Peer, TimeoutMs) end, Peers).

is_not_stale(Peer, TimeoutMs) ->
    delta_ms(Peer#peer.last_update, erlang:now()) < TimeoutMs.

delta_ms({A1,A2,A3}, {B1,B2,B3}) ->
    (B1 - A1) * 1000000000 + (B2 - A2) * 1000 + (B3 - A3) / 1000.0 .
