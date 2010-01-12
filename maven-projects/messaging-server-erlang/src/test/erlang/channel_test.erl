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
-module(channel_test).

-include_lib("eunit/include/eunit.hrl").

-include("channel.hrl").

channel_start_stop_test() ->
    Test = channel:start(test),
    ?assertMatch({[], []}, channel:get_peers(Test)),
    ?assertMatch(ok, channel:stop(Test)),
    ?assertMatch({error, timeout}, channel:get_peers(Test)),
    ok.
    
channel_peers_test() ->
    %%
    %% start with a fresh channel
    %%
    Test = channel:start(test),
    %%
    %% there should be no peers in the channel
    %%
    ?assertMatch({[], []}, channel:get_peers(Test)),
    %%
    %% Fred joins, and check for membership
    %%
    Fred = #peer{name = fred},
    ?assertMatch(ok, channel:join(Test, Fred)),
    {FredAdd, []} = channel:get_peers(Test),
	[FredPeer] = FredAdd,
    FredName = FredPeer#peer.name,
    ?assertMatch(FredName, fred),
    %%
    %% Check that getting the list of peers with fred already
    %% among the peer names returns the empty pair, and that
    %% checking with margot's name indicates that she should be
    %% removed.
    %%
    ?assertMatch({[], []}, channel:get_peers(Test, [fred])),
    ?assertMatch({[], [margot]}, channel:get_peers(Test, [fred, margot])),
    ?assertMatch({[Peer], [margot]}, channel:get_peers(Test, [margot])),
    %%
    %% Margot joins
    %%
    Margot = #peer{name = margot},
    ?assertMatch(ok, channel:join(Test, Margot)),
    {MargotAdd, []} = channel:get_peers(Test),
    ?assert(peer_list_contains(MargotAdd, fred)),
    ?assert(peer_list_contains(MargotAdd, margot)),
    %%
    %%
    %%
    ?assertMatch(ok, channel:leave(Test, fred)),
    {FredLeave, []} = channel:get_peers(Test),
    [MargotPeer] = FredLeave,
    MargotName = MargotPeer#peer.name,
    ?assertMatch(MargotName, margot),
    
    %%
    %% done
    %%
    channel:stop(Test).



peer_list_contains(PeerList, PeerName) ->
    lists:any(fun(Peer) -> Peer#peer.name =:= PeerName end, PeerList).
            



