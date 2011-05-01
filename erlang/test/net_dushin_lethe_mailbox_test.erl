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
-module(net_dushin_lethe_mailbox_test).

-include_lib("eunit/include/eunit.hrl").

-include("net_dushin_lethe_channel.hrl").
-include("net_dushin_lethe_mailbox.hrl").

%start_log_test() -> net_dushin_lethe_log:start([{filter, [{net_dushin_lethe_mailbox, [debug]}]}]).

start_stop_test() ->
    %%
    %% Start the server, stop it, and check that it's no longer available
    %%
    {ok, Pid} = net_dushin_lethe_mailbox:start(),
    {error, {already_started, Pid}} = net_dushin_lethe_mailbox:start(),
    ?assertMatch(ok, net_dushin_lethe_mailbox:stop()),
    ?assertMatch({'EXIT', {noproc, _}}, (catch net_dushin_lethe_mailbox:stop())),
    %%
    %% done
    %%
    ok.

create_mailbox_test() ->
    %%
    %% Start the server
    %%
    ?assertMatch({ok, _Pid}, net_dushin_lethe_mailbox:start()),
    net_dushin_lethe_mailbox:delete_mailbox(fred),
    %%
    %% Create the mailbox, and check that it exists 
    %%
    ?assertMatch(false, net_dushin_lethe_mailbox:mailbox_exists(fred)),
    Fred = #peer{name=fred},
    Mailbox = net_dushin_lethe_mailbox:create_mailbox(Fred),
    ?assertMatch(fred, Mailbox#mailbox.id),
    ?assertMatch(true, net_dushin_lethe_mailbox:mailbox_exists(fred)),
    %%
    %% Delete the mailbox, and check that it does not exist
    %%
    net_dushin_lethe_mailbox:delete_mailbox(fred),
    ?assertMatch(false, net_dushin_lethe_mailbox:mailbox_exists(fred)),
    %%
    %% done
    %%
    ?assertMatch(ok, net_dushin_lethe_mailbox:stop()).

mailbox_test() ->
    %%
    %% Start the server
    %%
    ?assertMatch({ok, _Pid}, net_dushin_lethe_mailbox:start()),
    net_dushin_lethe_mailbox:delete_mailbox(fred),
    Fred = #peer{name=fred},
    net_dushin_lethe_mailbox:create_mailbox(Fred),
    %%
    %% Check to make sure the peer is right
    %%
    Peer = net_dushin_lethe_mailbox:get_peer(fred),
    ?assertMatch(fred, Peer#peer.name),
    %%
    %% Check to make sure there are no messages in the mailbox
    %%
    Mailbox = net_dushin_lethe_mailbox:get_mailbox(fred),
    ?assertMatch(fred, Mailbox#mailbox.id),
    ?assertMatch([], Mailbox#mailbox.messages),
    %%
    %% Post a message, and make sure it's in the list of messages
    %%
    Message = #message{uuid = fubar},
    net_dushin_lethe_mailbox:post_message(fred, Message),
    NewMailbox = net_dushin_lethe_mailbox:get_mailbox(fred),
    ?assertMatch([Message], NewMailbox#mailbox.messages),
    %%
    %% Remove the message, and make sure it's not in the list of messages
    %%
    net_dushin_lethe_mailbox:delete_message(fred, Message#message.uuid),
    NewMailbox2 = net_dushin_lethe_mailbox:get_mailbox(fred),
    ?assertMatch([], NewMailbox2#mailbox.messages),
    %%
    %% done
    %%
    net_dushin_lethe_mailbox:delete_mailbox(fred),
    ?assertMatch(ok, net_dushin_lethe_mailbox:stop()).

%%
%% internal operations
%%

