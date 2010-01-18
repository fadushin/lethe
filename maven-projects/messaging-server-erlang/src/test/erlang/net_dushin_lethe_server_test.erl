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
-module(net_dushin_lethe_server_test).

-include_lib("eunit/include/eunit.hrl").

-include("net_dushin_lethe_channel.hrl").

start_stop_test() ->
    %%
    %% Start the server, stop it, and check that it's no longer available
    %%
    {ok, Pid} = net_dushin_lethe_server:start(),
    {error, {already_started, Pid}} = net_dushin_lethe_server:start(),
    ?assertMatch(ok, net_dushin_lethe_server:stop()),
    ?assertMatch({'EXIT', {noproc, _}}, (catch net_dushin_lethe_server:stop())),
    %%
    %% done
    %%
    ok.

channel_api_test() ->
    %%
    %% Start the server
    %%
    ?assertMatch({ok, _Pid}, net_dushin_lethe_server:start()),
    %%
    %% Run through the standard API
    %%
    Fred = #peer{name = fred},
    ?assertMatch(ok, net_dushin_lethe_server:join(test, Fred)),
    net_dushin_lethe_server:ping(test, Fred#peer.name),
    net_dushin_lethe_server:get_peers(test),
    net_dushin_lethe_server:get_peers(test, [Fred#peer.name]),
    net_dushin_lethe_server:leave(test, Fred#peer.name),
    Message = net_dushin_lethe_server:post_message(test, #message{}),
    net_dushin_lethe_server:get_messages(test),
    net_dushin_lethe_server:get_messages(test, Message#message.timestamp),
    %%
    %% done
    %%
    ?assertMatch(ok, net_dushin_lethe_server:stop()).

channel_timout_test() ->
    %%
    %% Start the server, stop it, and check that it's no longer available
    %%
    ?assertMatch(
        {ok, _Pid}, 
        net_dushin_lethe_server:start(
            [
                {
                    channel_config, 
                    [{channel_timeout_ms, 500}, {sweep_interval_ms, infinity}]
                }
            ]
        )
    ),
    Fred = #peer{name = fred},
    ?assertMatch(ok, net_dushin_lethe_server:join(test, Fred)),
    FirstMessage = net_dushin_lethe_server:post_message(test, #message{blob=first}),
    ?assert(lists:member(FirstMessage, net_dushin_lethe_server:get_messages(test))),
    %%
    %% Wait for the channel to expire
    %%
    sleep(1000),
    ?assertMatch([], net_dushin_lethe_server:get_peers(test)),
    ?assertMatch([], net_dushin_lethe_server:get_messages(test)),
    %%
    %% done
    %%
    ?assertMatch(ok, net_dushin_lethe_server:stop()).


%%
%% internal operations
%%

sleep(Ms) ->
    receive after Ms -> ok end.


