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
-module(net_dushin_lethe_syslog).

%%
%% Include files
%%

%%
%% Exported Functions
%%

-export(
    [
        init/1, 
		handle_event/2, 
		handle_call/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3, 
		format_status/2
    ]
).

-behavior(gen_event).

%%
%% API Functions
%%
init(Config) ->
    io:format("net_dushin_lethe_syslog:init~n", []),
    {ok, Config}.
    % case gen_udp:open(0, [list]) of
    %    {ok, Socket} ->

handle_event(Event, State) -> 
    io:format("net_dushin_lethe_syslog:handle_event(~p, ~p)~n", [Event, State]),
    {ok, State}.

handle_call(Request, State) ->
    io:format("net_dushin_lethe_syslog:handle_call(~p, ~p)~n", [Request, State]),
    {ok, State}.

handle_info(Info, State) ->
    io:format("net_dushin_lethe_syslog:handle_info(~p, ~p)~n", [Info, State]),
    {ok, State}.

terminate(Arg, State) ->
    io:format("net_dushin_lethe_syslog:terminate(~p, ~p)~n", [Arg, State]),
    ok.

code_change(OldVsn, State, Extra) -> 
    io:format("net_dushin_lethe_syslog:code_change(~p, ~p, ~p)~n", [OldVsn, State, Extra]),
    {ok, State}.

format_status(Opt, [PDict, State]) ->
    io:format("net_dushin_lethe_syslog:format_status(~p, [~p, ~p])~n", [Opt, PDict, State]),
    ok.
        


%%
%% Local Functions
%%

