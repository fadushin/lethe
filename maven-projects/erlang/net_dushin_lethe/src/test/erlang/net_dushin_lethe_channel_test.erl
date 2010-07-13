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
-module(net_dushin_lethe_channel_test).

-include_lib("eunit/include/eunit.hrl").

-include("net_dushin_lethe_channel.hrl").

start_stop_test() ->
    Test = net_dushin_lethe_channel:start(start_stop_test),
    ?assertMatch(ok, net_dushin_lethe_channel:stop(Test)),
    ?assertMatch({error, timeout}, net_dushin_lethe_channel:get_peers(Test)),
    ok.


channel_timeout_test() ->
    Me = self(),
    Test = net_dushin_lethe_channel:start(
        channel_timeout_test, 
        [
            {channel_timeout_ms, 500},
            {shutdown_handler, fun(ChannelId) -> Me ! ChannelId end}
        ]
    ),
    ?assertMatch([], net_dushin_lethe_channel:get_peers(Test)),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test)),
    Response = receive
        Message -> Message
    after 1000 -> error
    end,
    ?assertMatch(Response, channel_timeout_test),
    ?assertMatch({error, timeout}, net_dushin_lethe_channel:get_peers(Test)),
    ok.
    

peers_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(peers_test),
    %%
    %% there should be no peers in the channel
    %%
    ?assertMatch([], net_dushin_lethe_channel:get_peers(Test)),
    %%
    %% Fred joins, and check for membership
    %%
    Fred = #peer{name = fred},
    ?assertMatch(ok, net_dushin_lethe_channel:join(Test, Fred)),
    FredAdd = net_dushin_lethe_channel:get_peers(Test),
    ?assertEqual(length(FredAdd), 1),
    ?assert(peer_list_contains(FredAdd, fred)),
    %%
    %% Check that getting the list of peers with fred already
    %% among the peer names returns the empty pair, and that
    %% checking with margot's name indicates that she should be
    %% removed.
    %%
    ?assertMatch({[], []}, net_dushin_lethe_channel:get_peers(Test, [fred])),
    ?assertMatch({[], [margot]}, net_dushin_lethe_channel:get_peers(Test, [fred, margot])),
    {GetPeersMargot, [margot]} = net_dushin_lethe_channel:get_peers(Test, [margot]),
    ?assert(peer_list_contains(GetPeersMargot, fred)),
    %%
    %% Margot joins
    %%
    Margot = #peer{name = margot},
    ?assertMatch(ok, net_dushin_lethe_channel:join(Test, Margot)),
    MargotAdd = net_dushin_lethe_channel:get_peers(Test),
    ?assert(peer_list_contains(MargotAdd, fred)),
    ?assert(peer_list_contains(MargotAdd, margot)),
    {MargotAdd,  []} = net_dushin_lethe_channel:get_peers(Test, []),
    {MargotAdd1, []} = net_dushin_lethe_channel:get_peers(Test, [fred]),
    ?assert(not peer_list_contains(MargotAdd1, fred)),
    ?assert(peer_list_contains(MargotAdd1, margot)),
    {MargotAdd2, []} = net_dushin_lethe_channel:get_peers(Test, [margot]),
    ?assert(peer_list_contains(MargotAdd2, fred)),
    ?assert(not peer_list_contains(MargotAdd2, margot)),
    {[], []} = net_dushin_lethe_channel:get_peers(Test, [fred, margot]),
    {[], [stranger]} = net_dushin_lethe_channel:get_peers(Test, [fred, margot, stranger]),
    %%
    %% Fred leaves
    %%
    ?assertMatch(ok, net_dushin_lethe_channel:leave(Test, fred)),
    FredLeave = net_dushin_lethe_channel:get_peers(Test),
    ?assertEqual(length(FredLeave), 1),
    ?assert(peer_list_contains(FredLeave, margot)),
    %%
    %% Margot leaves
    %%
    ?assertMatch(ok, net_dushin_lethe_channel:leave(Test, margot)),
    [] = net_dushin_lethe_channel:get_peers(Test),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).

    
max_peers_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(
        max_peers_test, 
        [
            {max_peers, 2}
        ]
    ),
    %%
    %% Fill the list of peers
    %%
    Fred = #peer{name = fred},
    Margot = #peer{name = margot},
    ?assertMatch(ok, net_dushin_lethe_channel:join(Test, Fred)),
    ?assertMatch(ok, net_dushin_lethe_channel:join(Test, Margot)),
    FredMargotAdd = net_dushin_lethe_channel:get_peers(Test),
    ?assertEqual(length(FredMargotAdd), 2),
    ?assert(peer_list_contains(FredMargotAdd, fred)),
    ?assert(peer_list_contains(FredMargotAdd, margot)),
    %%
    %% Try to add a new peer, and check for expected error
    %%
    ?assertMatch({error, too_many_peers}, net_dushin_lethe_channel:join(Test, #peer{name=too_many_peers})),
    net_dushin_lethe_channel:leave(Test, fred),
    FredLeave = net_dushin_lethe_channel:get_peers(Test),
    ?assertEqual(length(FredLeave), 1),
    ?assert(not(peer_list_contains(FredLeave, fred))),
    ?assert(peer_list_contains(FredLeave, margot)),
    ?assertMatch(ok, net_dushin_lethe_channel:join(Test, #peer{name=some_new_peer})),
    SomeNewPeerJoins = net_dushin_lethe_channel:get_peers(Test),
    ?assertEqual(length(SomeNewPeerJoins), 2),
    ?assert(not(peer_list_contains(FredLeave, fred))),
    ?assert(peer_list_contains(SomeNewPeerJoins, margot)),
    ?assert(peer_list_contains(SomeNewPeerJoins, some_new_peer)),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).

    
peers_timeout_test_disabled() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(
        peers_timeout_test, 
        [
            {peer_timeout_ms, 500}, 
            {sweep_interval_ms, 600}
        ]
    ),
    %%
    %% Add a peer
    %%
    {_Fred, FredAdd} = check_add_peer(Test, fred),
    ?assertEqual(length(FredAdd), 1),
    %%
    %% Wait until after the timeout interval, and 
    %% check that the peer has been removed
    %%
    sleep(1000),
    ?assertMatch([], net_dushin_lethe_channel:get_peers(Test)),
    %%
    %% Join the channel, and ping 3 times every 350ms.
    %%
    {_Margot, MargotAdd} = check_add_peer(Test, margot),
    ?assertEqual(length(MargotAdd), 1),
    sleep(350),
    ok = net_dushin_lethe_channel:ping(Test, margot),
    ?assert(channel_contains_peer(Test, margot)),
    sleep(350),
    ok = net_dushin_lethe_channel:ping(Test, margot),
    ?assert(channel_contains_peer(Test, margot)),
    sleep(350),
    ok = net_dushin_lethe_channel:ping(Test, margot),
    ?assert(channel_contains_peer(Test, margot)),
    %%
    %% now wait for margot to time out
    %%
    sleep(1000),
    ?assertMatch([], net_dushin_lethe_channel:get_peers(Test)),
    ?assertMatch({error, peer_does_not_exist}, net_dushin_lethe_channel:ping(Test, margot)),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).


messages_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(messages_test),
    %%
    %% there should be no messages in the channel
    %%
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test)),
    %%
    %% Add a message, and check that it's in the list of messages
    %%
    FirstMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=first}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assertEqual(length(net_dushin_lethe_channel:get_messages(Test)), 1),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test, FirstMessage#message.timestamp)),
    %%
    %% Add another message
    %%
    SecondMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=second}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(SecondMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assertEqual(length(net_dushin_lethe_channel:get_messages(Test)), 2),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test, SecondMessage#message.timestamp)),
    ?assert(
        lists:member(
            SecondMessage, 
            net_dushin_lethe_channel:get_messages(Test, FirstMessage#message.timestamp)
        )
    ),
    ?assert(
        not lists:member(
            FirstMessage, 
            net_dushin_lethe_channel:get_messages(Test, FirstMessage#message.timestamp)
        )
    ),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).

    
max_messages_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(
        max_messages_test, 
        [
            {max_messages, 2}
        ]
    ),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test)),
    %%
    %% Fill the list of messages
    %%
    FirstMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=first}),
    SecondMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=second}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(SecondMessage, net_dushin_lethe_channel:get_messages(Test))),
    %%
    %% Now add a message.  First should be removed
    %%
    ThirdMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=third}),
    ?assert(not lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(SecondMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(ThirdMessage, net_dushin_lethe_channel:get_messages(Test))),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).

    
messages_timeout_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = net_dushin_lethe_channel:start(
        messages_timeout_test, 
        [
            {message_timeout_ms, 500}, 
            {sweep_interval_ms, 50}
        ]
    ),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test)),
    %%
    %% Add 2 messages, separated by 200ms
    %%
    FirstMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=first}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    sleep(200),
    SecondMessage = net_dushin_lethe_channel:post_message(Test, #message{blob=second}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(SecondMessage, net_dushin_lethe_channel:get_messages(Test))),
    %%
    %% Wait until after the timeout interval on the first mesage, and 
    %% check that the it has been removed (but not the second)
    %%
    sleep(350),
    ?assert(not lists:member(FirstMessage, net_dushin_lethe_channel:get_messages(Test))),
    ?assert(lists:member(SecondMessage, net_dushin_lethe_channel:get_messages(Test))),
    %%
    %% Now wait for the second message to expire
    %%
    sleep(350),
    ?assertMatch([], net_dushin_lethe_channel:get_messages(Test)),
    %%
    %% done
    %%
    net_dushin_lethe_channel:stop(Test).



%%
%% internal operations
%%

check_add_peer(Channel, PeerName) ->
    Peer = #peer{name = PeerName},
    ?assertMatch(ok, net_dushin_lethe_channel:join(Channel, Peer)),
    Add = net_dushin_lethe_channel:get_peers(Channel),
    ?assert(peer_list_contains(Add, PeerName)),
    {Peer, Add}.

    
peer_list_contains(PeerList, PeerName) ->
    lists:any(fun(Peer) -> Peer#peer.name =:= PeerName end, PeerList).

channel_contains_peer(Channel, PeerName) ->
    Peers = net_dushin_lethe_channel:get_peers(Channel),
    % io:format("Peers in channel ~p: ~p~n", [Channel, Peers]),
    peer_list_contains(Peers, PeerName).

sleep(Ms) ->
    receive after Ms -> ok end.


