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
%% callbacks
%%

handle_rpc(_State, {call, Method, Params} = Request, Session) ->  
    io:format("Request = ~p~n", [Request]),
    Response = get_response(Method, Params),
    io:format("Response = ~p~n", [Response]),
    {true, 0, Session, {response, Response}}.

get_response(get_channels, _Params) ->
    channels_to_json(net_dushin_lethe_server:get_channels());

get_response(get_peers, [ChannelId]) ->
    peers_to_json(net_dushin_lethe_server:get_peers(list_to_atom(ChannelId)));

get_response(_Method, _Params) ->
    %% TODO come up with a way to handle unknown methods
    atom_to_list(undefined).


%%
%% private operations
%%

handle_request('POST', "/rpc" ++ _, Arg) ->
    Peer = inet:peername(Arg#arg.clisock),
    {ok, {IP, _}} = Peer,
    yaws_rpc:handler_session(Arg#arg{state = [{ip, IP}]}, {?MODULE, handle_rpc});
handle_request('GET', "/get_channels" ++ _, _Arg) ->
    make_response(
        200,
        io_lib:format("<pre>~n~p~n</pre>~n", [net_dushin_lethe_server:get_channels()])
    );
handle_request(_, Path, Arg) -> % catchall 
    io:format("~p ~p~n", [Path, Arg]),
    make_response(200, "<p>What exactly are you looking for?</p>").

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
                atom_to_list(Peer#peer.name)
            end,
            PeerList
        )
    }.


%%
%% old cruft
%%

get_path(Arg) -> 
    Req = Arg#arg.req, 
    {abs_path, Path} = Req#http_request.path,
    Path.

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

channelListToHTML(ChannelList) ->
    "<table>" ++ channelListToHTMLTable(ChannelList) ++ "</table>".

channelListToHTMLTable([]) ->
    [];
channelListToHTMLTable([{ChannelName, _Channel}|T]) ->
    "<tr><td>" ++ atom_to_list(ChannelName) ++ "</td></tr>" ++ channelListToHTMLTable(T).