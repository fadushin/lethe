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

-export([out/1, handle_request/3]).

out(Arg) -> 
    Req = Arg#arg.req, ReqPath = Arg#arg.pathinfo, 
    handle_request(Req#http_request.method, ReqPath, Arg).

get_path(Arg) -> 
    Req = Arg#arg.req, {abs_path, Path} = Req#http_request.path,
    Path.

handle_request('GET', [47,97,99,99,111,117,110,116 | _], _Arg) -> % "/account" ... 
    make_response(200, "<p>Please login or logout, okay?</p>");

handle_request('GET', [47,112,114,111,102,105,108,101 | _], _Arg) -> % "/profile" ... 
    make_response(200, "<p>This is a slick profile.</p>");

handle_request(_, Path, Arg) -> % catchall 
    io:format("~p ~p~n", [Path, Arg]),
    make_response(200, "<p>What exactly are you looking for?</p>").

make_response(Status, Message) -> make_response(Status, "text/html", Message).

make_response(Status, Type, Message) -> make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) -> [{status, Status}, {allheaders, Headers}, {html, Message}].