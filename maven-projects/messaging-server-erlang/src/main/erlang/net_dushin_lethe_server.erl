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
-module(net_dushin_lethe_server).

-behavior(gen_server).

-export(
    [
        %%
        %% Client API
        %%
        start/0, start/1, stop/0, stop/1, 
        join/2, get_peers/1, get_peers/2, leave/2, ping/2, 
        post_message/2, get_messages/1, get_messages/2,
        %%
        %% gen_server implementation
        %%
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
    ]
).

-include("channel.hrl").
-include("net_dushin_lethe_server.hrl").


-record(
    server_context,
    {
        config,
        tab
    }
).


%%
%% Client API
%%

%%
%% @type    net_dushin_lethe_server_config() -> [option()].
%%
%% @type    option() -> {key(), term()}.
%%
%% @type    key() ->
%%              channel_config
%%

%%
%% @spec    start() -> ok
%%
start() ->
    start([]).

%%
%% @spec    start(net_dushin_lethe_server_config()) -> ok
%%
start(Config) ->
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
    stop(#call_opts{}).

%%
%% @spec    stop([option()]) -> ok
%%
%% Example: stop([{timeout_ms, 1000}]).
%%
stop(Options) ->
    gen_server:call(
        ?MODULE,
        stop,
        net_dushin_lethe_lists:find_value(
            Options,
            timeout_ms,
            1000
        )
    ).

%%
%% @spec        join(atom(), peer()) -> ok | {error, too_many_peers} | {error, timeout}.
%%
join(ChannelId, Peer) ->
    channel:join(get_channel(ChannelId), Peer).

%%
%% @spec        ping(atom(), atom()) -> ok.
%%
ping(ChannelId, PeerName) ->
    channel:ping(get_channel(ChannelId), PeerName).

%%
%% @spec        leave(atom(), atom()) -> ok.
%%
leave(ChannelId, PeerName) ->
    channel:leave(get_channel(ChannelId), PeerName).

%%
%% @spec        get_peers(atom()) -> {peer_list(), [atom()]}.
%%
get_peers(ChannelId) ->
    channel:get_peers(get_channel(ChannelId)).

%%
%% @spec        get_peers(atom(), [atom()]) -> {peer_list(), [atom()]}.
%%
get_peers(ChannelId, PeerNames) ->
    channel:get_peers(get_channel(ChannelId), PeerNames).

%%
%% @spec        post_message(atom(), message()) -> message().
%%
post_message(ChannelId, Message) ->
    channel:post_message(get_channel(ChannelId), Message).

%%
%% @spec        get_messages(atom()) -> message_list().
%%
get_messages(ChannelId) ->
    channel:get_messages(get_channel(ChannelId)).

%%
%% @spec        get_messages(atom(), timestamp() | all) -> message_list().
%%
get_messages(ChannelId, Since) ->
    channel:get_messages(get_channel(ChannelId), Since).


%%
%% gen_server implementation
%%

init(Config) -> 
    process_flag(trap_exit, true),
    {
        ok, 
        #server_context {
            config = Config,
            tab = ets:new(
                ?MODULE,
                [set, private]
            )
        }
    }.

handle_call({get, ChannelId}, _From, State) ->
    {
        reply, 
        get_or_create(ChannelId, State), 
        State
    };
handle_call({remove, ChannelId}, _From, State) ->
    remove_channel(State#server_context.tab, ChannelId),
    {
        reply, 
        ok, 
        State
    };
handle_call(stop, _From, State) ->
    stop_channels(State#server_context.tab),
    {stop, requested, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, State) -> 
    io:format("lethe server terminate with reason ~p and state ~p~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%
%% internal functions
%%

get_channel(ChannelId) ->
    gen_server:call(
        ?MODULE,
        {get, ChannelId}
    ).

remove_channel(ChannelId) ->
    gen_server:call(
        ?MODULE,
        {remove, ChannelId}
    ).

get_or_create(ChannelId, State) ->
    value(State, ChannelId, get_channel(State#server_context.tab, ChannelId)).

value(State, ChannelId, undefined) ->
    ShutdownHandler = fun(Id) -> remove_channel(Id) end,
    Channel = channel:start(
        ChannelId,
        [ 
            {shutdown_handler, ShutdownHandler} | 
            net_dushin_lethe_lists:find_value(
                State#server_context.config,
                channel_config,
                []
            )
        ]
    ),
    ets:insert(State#server_context.tab, {ChannelId, Channel}),
    Channel;
value(_Ets, _ChannelId, X) ->
    X.

get_channel(Ets, ChannelId) ->
    case ets:lookup(Ets, ChannelId) of
        [] ->
            undefined;
        [{ChannelId, Value}|[]] ->
            Value
    end.

remove_channel(Ets, ChannelId) ->
    ets:delete(Ets, ChannelId).

stop_channels(Ets) ->
    ets:delete_all_objects(Ets), % TODO is this necessary?
    ets:delete(Ets),
    ok.
