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

{
    application, net_dushin_lethe,
    [
        {description, "Lethe Messaging Server"},
        {vsn, "0.1-SNAPSHOT"},
        {
            modules, 
            [
                net_dushin_lethe_launcher, net_dushin_lethe, net_dushin_lethe_sup, net_dushin_lethe_server, net_dushin_lethe_channel,
                net_dushin_lethe_timer, net_dushin_lethe_rpc, net_dushin_lethe_lists, net_dushin_lethe_uuid,
                net_dushin_lethe_handler
            ]
        },
        {registered, [net_dushin_lethe_server]},
        {applications, [kernel, stdlib]},
        {
            mod, 
            {
                net_dushin_lethe, 
                [
                    {
                        lethe_args, 
                        [
                            % [{channel_config, [{peer_timeout_ms, 50000000}]}]
                        ]
                    }, 
                    {yaws_args, []}
                ]
            }
        },
        {start_phases, []}
    ]
}.
