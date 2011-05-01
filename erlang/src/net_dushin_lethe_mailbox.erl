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

-module(net_dushin_lethe_mailbox).

-behavior(gen_server).

%%
%% Include files
%%
-include("net_dushin_lethe_mailbox.hrl").
-include("net_dushin_lethe_channel.hrl").
-include("net_dushin_lethe_log.hrl").

%%
%% Exported Functions
%%
-export(
    [
        %%
        %% gen_server implementation
        %%
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
        %%
        %% Client API
        %%
        start/0, start/1, stop/0, stop/1,
        create_mailbox/1, mailbox_exists/1, delete_mailbox/1,
        get_peer/1, get_mailbox/1, post_message/2, delete_message/2
    ]
).

-record(
    context,
    {
        config
    }
).

%%
%% API Functions
%%

%%
%% @spec    start() -> ok
%%
start() ->
    start(#mailbox_config{}).

%%
%% @spec    start(mailbox_config()) -> ok
%%
start(Config) ->
    ?LETHE_DEBUG("Starting Mailbox Server...", []),
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        Config,
        []
    ).

%%
%% @spec    stop() -> ok
%%
stop() ->
    stop(1000).

%%
%% @spec    stop(timeout_ms()) -> ok
%%
%% Example: stop(1000).
%%
stop(TimeoutMs) ->
    ?LETHE_DEBUG("Stopping Mailbox Server...", []),
    gen_server:call(
        ?MODULE,
        stop,
        TimeoutMs
    ).

%%
%% Mailbox operations
%%

%%
%% create_mailbox(peer()) -> ok | throws {error_mailbox_exists, PeerId}
%%
create_mailbox(Peer) ->
    ?LETHE_DEBUG("Creating Mailbox: ~p...", [Peer]),
    gen_server:call(
        ?MODULE,
        {create_mailbox, Peer}
    ).

mailbox_exists(PeerId) ->
    gen_server:call(
        ?MODULE,
        {mailbox_exists, PeerId}
    ).

delete_mailbox(PeerId) ->
    ?LETHE_DEBUG("Deleting Mailbox: ~p...", [PeerId]),
    gen_server:call(
        ?MODULE,
        {delete_mailbox, PeerId}
    ).

get_peer(PeerId) ->
    gen_server:call(
        ?MODULE,
        {get_peer, PeerId}
    ).

get_mailbox(PeerId) ->
    gen_server:call(
        ?MODULE,
        {get_mailbox, PeerId}
    ).

post_message(PeerId, Message) ->
    ?LETHE_DEBUG("Posting message to Mailbox; peer-id: ~p; Message: ~p...", [PeerId, Message]),
    gen_server:call(
        ?MODULE,
        {post_message, PeerId, Message}, infinity
    ).

delete_message(PeerId, Uuid) ->
    ?LETHE_DEBUG("Deleting message from Mailbox; peer-id: ~p; Message-uuid: ~p...", [PeerId, Uuid]),
    gen_server:call(
        ?MODULE,
        {delete_message, PeerId, Uuid}, infinity
    ).


%%
%% gen_server implementation
%%

init(Config) -> 
    %process_flag(trap_exit, true),
    case dets:open_file(
        ?MODULE,
        Config#mailbox_config.dets_config
    ) of 
        {ok, ?MODULE} ->
            ok;
        {error, Reason} ->
            ?LETHE_SEVERE("Unable to open Mailbox Table: Reason: ~p", [Reason])
    end,
    {
        ok,
        #context {
            config = Config
        }
    }.

handle_call({create_mailbox, Peer}, _From, State) ->
    {reply, create_mailbox(Peer, State), State};
handle_call({mailbox_exists, PeerId}, _From, State) ->
    {reply, mailbox_exists(PeerId, State), State};
handle_call({delete_mailbox, PeerId}, _From, State) ->
    {reply, delete_mailbox(PeerId, State), State};
handle_call({get_mailbox, PeerId}, _From, State) ->
    Mailbox = get_mailbox(PeerId, State), 
    {reply, Mailbox, State};
handle_call({get_peer, PeerId}, _From, State) ->
    Mailbox = get_mailbox(PeerId, State),
    Peer = Mailbox#mailbox.peer,
    {reply, Peer, State};
handle_call({post_message, PeerId, Message}, _From, State) ->
    Mailbox = get_mailbox(PeerId, State),
    set_mailbox(Mailbox#mailbox{messages = [Message | Mailbox#mailbox.messages]}, true),
    {reply, ok, State};
handle_call({delete_message, PeerId, Uuid}, _From, State) ->
    Mailbox = get_mailbox(PeerId, State),
    set_mailbox(Mailbox#mailbox{messages = remove_message(Mailbox#mailbox.messages, Uuid)}, true),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, State) -> 
    ?LETHE_DEBUG("lethe mailbox server terminated with reason ~p and state ~p", [Reason, State]),
    dets:close(?MODULE).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Local Functions
%%

create_mailbox(Peer, _State) ->
    PeerId = Peer#peer.name,
    Mailbox = set_mailbox(#mailbox{id = PeerId, peer = Peer}),
    ?LETHE_INFO("Created Mailbox: ", [Mailbox]),
    Mailbox.

set_mailbox(Mailbox) ->
    set_mailbox(Mailbox, false).
set_mailbox(Mailbox, Overwrite) ->
    case Overwrite of
        true ->
            delete_mailbox(Mailbox#mailbox.id, nil);
        false -> ok
    end,
    Peer = Mailbox#mailbox.peer,
    PeerId = Peer#peer.name,
    case dets:insert_new(?MODULE, {PeerId, Mailbox}) of
        true ->
            Mailbox;
        false ->
            throw({error_mailbox_exists, PeerId})
    end.

mailbox_exists(PeerId, _State) ->
    case dets:lookup(?MODULE, PeerId) of
        {error, Reason} ->
            throw({error, Reason});
        [] ->
            false;
        [_Value|[]] ->
            true;
        _Any ->
            throw({error, "DETS: Invalid config!"})
    end.

delete_mailbox(PeerId, State) ->
    case mailbox_exists(PeerId, State) of
        true ->
            ?LETHE_DEBUG("Mailbox exists: PeerId=~p; deleting...", [PeerId]),
            case dets:delete(?MODULE, PeerId) of
                {error, Reason} ->
                    ?LETHE_DEBUG(
                        "An error occurred attempting to delete a mailbox: PeerId=~p; Reason=~p", 
                        [PeerId, Reason]
                    ),
                    throw({error, Reason});
                ok ->
                    ?LETHE_INFO("Deleted Mailbox: ", [PeerId]),
                    ok
            end;
        false ->
            ?LETHE_DEBUG("Mailbox not deleted because it does not exist: PeerId=~p", [PeerId]),
            ok
    end.

get_mailbox(PeerId, _State) ->
    case dets:lookup(?MODULE, PeerId) of
        {error, Reason} ->
            throw({error, Reason});
        [] ->
            throw({error_mailbox_not_exist, PeerId});
        [{PeerId, Value}|[]] ->
            Value;
        _Any ->
            throw({error, "DETS: Invalid config!"})
    end.

remove_message(MessageList, Uuid) ->
    lists:filter(
        fun(Message) -> 
            not(Message#message.uuid =:= Uuid) 
        end, 
        MessageList
    ).
