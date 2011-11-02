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
-module(net_dushin_lethe_launcher).

%%
%% Include files
%%
-include("net_dushin_lethe_channel.hrl").

%%
%% Exported Functions
%%
-export([start/0, stop/0, ping/1, stop/1]).

%%
%% API Functions
%%
start() ->
    application:start(net_dushin_lethe).

stop() ->
    application:stop(net_dushin_lethe).

ping([Node]) ->
    io:format("Lethe is ~s at node ~p~n", [
        case net_adm:ping(Node) of
            pong ->
                "running";
            pang ->
                "not running"
        end,
        Node
    ]),
    init:stop().

stop([Node]) ->
    case net_adm:ping(Node) of
        pong -> 
            io:format("~s~n", [
                case rpc:call(Node, init, stop, []) of
                    ok ->
                        "Lethe stopped";
                    {badrpc, Reason} ->
                        io_lib:format(
                            "An error occurred attempting to stop Lethe at ~p: ~p~n",
                            [Node, Reason]
                        )
                end
            ]);
        pang ->
            io:format("There is no node with the name ~p~n", [Node])
    end,
    init:stop().
