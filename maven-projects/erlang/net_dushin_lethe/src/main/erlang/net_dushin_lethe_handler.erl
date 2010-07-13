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

-module(net_dushin_lethe_handler).

-include("yaws.hrl"). 
-include("yaws_api.hrl"). 
-include("net_dushin_lethe_channel.hrl"). 

-export([out/1, handle_rpc/3]).

%%
%% Yaws Handler operations
%%

out(Arg) -> 
    Req = Arg#arg.req, 
    ReqPath = Arg#arg.pathinfo, 
    handle_request(Req#http_request.method, ReqPath, Arg).

%%
%% Internal operations
%%

handle_request('POST', "/rpc" ++ _, Arg) ->
    Peer = inet:peername(Arg#arg.clisock),
    {ok, {IP, _}} = Peer,
    yaws_rpc:handler_session(Arg#arg{state = [{ip, IP}]}, {?MODULE, handle_rpc});
handle_request(_, Path, Arg) -> % catchall 
    io:format("~p ~p~n", [Path, Arg]),
    make_response(200, "<p>What exactly are you looking for?</p>").

%%
%% handle_rpc gets called through yaws_rpc:handler_session; it is the
%% handler for all of the JSON RPC operations.
%%

handle_rpc(_State, {call, Method, Params} = _Request, Session) ->  
    %io:format("Request = ~p~n", [Request]),
    Response = 
            try get_response(Method, Params)
            catch
                Exception -> Exception
            end,
    %io:format("Response = ~p~n", [Response]),
    {true, 0, Session, {response, Response}}.
%
% input args:
%   ChannelId,
%   PeerName,
%   {
%       update_peers: [peerid, ..., peerid],
%       update_messages: since
%   }
%
%
%
get_response(update, [ChannelId, PeerName, {struct, [{update_peers, UpdatePeers}, {update_messages, Since}]}]) ->
    net_dushin_lethe_server:ping(
        list_to_atom(ChannelId),
        list_to_atom(PeerName)
    ),
    PeerUpdate = get_response(get_peers, [ChannelId, UpdatePeers]),
    MessageUpdate = get_response(get_messages_since, [ChannelId, Since]),
    {
        struct,
        [
            {peer_update, PeerUpdate},
            {message_update, MessageUpdate}
        ]
    };

get_response(get_channels, _Params) ->
    channels_to_json(net_dushin_lethe_server:get_channels());

get_response(get_peers, [ChannelId, {array, PeerNames}]) ->
    PeerAtoms = lists:map(
            fun(PeerName) ->
                list_to_atom(PeerName)
            end,
            PeerNames
        ),
    {Add, Remove} = net_dushin_lethe_server:get_peers(
        list_to_atom(ChannelId),
        PeerAtoms
    ),
    io:format("get_peers(~p) -> {Add, Remove} = ~p~n", 
        [
            PeerAtoms,
            {lists:map(fun(Peer) -> Peer#peer.name end, Add), Remove}
        ]
    ),
    {
        struct,
        [
            {add, peers_to_json(Add)},
            {remove, {array, lists:map(fun(Name) -> atom_to_list(Name) end, Remove)}}
        ]
    };

get_response(join, [ChannelId, Peer]) ->
    io:format("join(~p, ~p)~n", [ChannelId, Peer]),
    net_dushin_lethe_server:join(
        list_to_atom(ChannelId),
        json_to_peer(Peer)
    ),
    "ok";

get_response(ping, [ChannelId, PeerName]) ->
    net_dushin_lethe_server:ping(
        list_to_atom(ChannelId),
        list_to_atom(PeerName)
    ),
    "ok";

get_response(leave, [ChannelId, PeerName]) ->
    io:format("leave: ~p~n", [list_to_atom(PeerName)]),
    net_dushin_lethe_server:leave(
        list_to_atom(ChannelId),
        list_to_atom(PeerName)
    ),
    "ok";

get_response(get_messages, [ChannelId]) ->
    get_response(
        get_messages_since, [ChannelId, all]
    );

get_response(get_messages_since, [ChannelId, Since]) ->
    Messages = net_dushin_lethe_server:get_messages(
        list_to_atom(ChannelId), Since
    ),
    io:format(
        "get_messages_since(~p, ~p): ~p~n", 
        [ChannelId, Since, Messages]
    ),
    messages_to_json(
        Messages
    );

get_response(post_message, [ChannelId, Message]) ->
    net_dushin_lethe_server:post_message(
        list_to_atom(ChannelId),
        json_to_message(Message)
    ),
    "ok";

get_response(_Method, _Params) ->
    %% TODO come up with a way to handle unknown methods
    atom_to_list(undefined).


%%
%% JSON translation operations
%%

channels_to_json(ChannelEntryList) ->
    {
        array, 
        lists:map(
            fun({ChannelName, _Channel}) ->
                atom_to_list(ChannelName)
            end,
            ChannelEntryList
        )
    }.

peers_to_json(PeerList) ->
    {
        array, 
        lists:map(
            fun(Peer) ->
                {
                    struct,
                    [
                        {id, atom_to_list(Peer#peer.name)},
                        {blob, Peer#peer.blob}
                    ]
                }
            end,
            PeerList
        )
    }.

messages_to_json(MessageList) ->
    {
        array, 
        lists:map(
            fun(Message) ->
                {
                    struct,
                    [
                        {timestamp, Message#message.timestamp},
                        {blob, Message#message.blob}
                    ]
                }
            end,
            MessageList
        )
    }.

json_to_peer({struct, PropertyList}) ->
    #peer {
        name = list_to_atom(net_dushin_lethe_lists:find_value(PropertyList, id)),
        blob = net_dushin_lethe_lists:find_value(PropertyList, blob, "")
    }.

json_to_message({struct, PropertyList}) ->
    #message {
        blob = net_dushin_lethe_lists:find_value(PropertyList, blob)
        % uuid = net_dushin_lethe_lists:find_value(PropertyList, uuid)
    }.

%%
%% old cruft
%%

make_response(Status, Message) -> 
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) -> 
    make_all_response(Status, make_header(Type), Message).

make_header(Type) -> 
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) -> 
    [
        {status, Status}, 
        {allheaders, Headers}, 
        {html, Message}
    ].
