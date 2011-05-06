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
-module(net_dushin_lethe).

-behavior(application).

-include("yaws.hrl"). 

-export(
    [
        %%
        %% application implementation
        %%
        start/2, stop/1
    ]
).

%%
%% application behavior
%%

start(_Type, Options) ->
    net_dushin_lethe_log:start(net_dushin_lethe_lists:find_value(Options, log_args, [])),
    start_yaws(net_dushin_lethe_lists:find_value(Options, yaws_args, [])),
    net_dushin_lethe_sup:start(net_dushin_lethe_lists:find_value(Options, lethe_args, [])).

stop(_State) ->
    application:stop(yaws).

%%
%% Internal functions
%%

start_yaws(Options) ->
    %process_flag(trap_exit, true), 
    case application:start(yaws) of
        ok -> 
            set_conf(Options),
            io:format("Yaws started with options: ~p~n", [Options]);
        Error -> 
            io:format("Error: ~p~n", [Error]),
            {stop, Error}
    end.

set_conf(Options) -> 
    LetheRoot = net_dushin_lethe_lists:find_value(Options, lethe_root, "."),
    GC = #gconf{
        trace = net_dushin_lethe_lists:find_value(Options, trace, false), 
        logdir = LetheRoot ++ "/logs", 
        yaws = "lethe-0.1-SNAPSHOT" 
        %, tmpdir = LetheRoot ++ "/.yaws"
    },
    SCS = lists:map(
        fun(SC) ->
            #sconf{
                port = net_dushin_lethe_lists:find_value(SC, port, 8080), 
                servername = net_dushin_lethe_lists:find_value(SC, servername, "localhost"), 
                listen = net_dushin_lethe_lists:find_value(SC, listen, {0, 0, 0, 0}), 
                docroot = net_dushin_lethe_lists:find_value(SC, docroot, LetheRoot ++ "/content"), 
                appmods = [
                    {
                        net_dushin_lethe_lists:find_value(SC, lethe_prefix, "/rs"), 
                        net_dushin_lethe_handler
                    }
                ]
            }
        end,
        net_dushin_lethe_lists:find_value(
            Options, scs, [[]]
        )
    ),
    io:format("Calling yaws_api:setconf(~p, [~p])~n", [GC, SCS]),
    case catch yaws_api:setconf(GC, [SCS]) of
        ok -> {ok, started}; 
        Error -> {stop, Error}
    end.
