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
-module(net_dushin_lethe_log).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([severe/2, warning/2, info/2, debug/2]).

%%
%% API Functions
%%

%% TODO make this a configurable process, etc

severe(Fmt, Args) ->
    log("SEVERE " ++ Fmt, Args).

warning(Fmt, Args) ->
    log("WARNING " ++ Fmt, Args).

info(Fmt, Args) ->
    log("INFO " ++ Fmt, Args).

debug(Fmt, Args) ->
    ok. %log("DEBUG " ++ Fmt, Args).



%%
%% Local Functions
%%

log(Fmt, Args) ->
    io:format("~p: " ++ Fmt ++ "~n", [current_ms() | Args]).


current_ms() ->
    {Megasecs, Secs, MicroSecs} = erlang:now(),
    Val = (Megasecs * 1000000000 + Secs * 1000 + erlang:trunc(MicroSecs / 1000.0)),
    Val.

