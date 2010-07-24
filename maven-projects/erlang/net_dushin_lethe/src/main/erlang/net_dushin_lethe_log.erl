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

-behavior(gen_server).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export(
    [
        start/0, start/1, stop/0, set/2,
        log/4,
        %%
        %% gen_server implementation
        %%
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3

    ]
).

-record(
    state,
    {
        logger,
        tab
    }
).


%%
%% API Functions
%%

start() -> start([]).

%
% {logger, fun(Module, Level, Fmt, Args)}
% {filter, [{module1, [severe, info]}, {module2, [debug]}]}
%

start(Config) ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        Config,
        []
    ).

stop() ->
    gen_server:call(
        ?MODULE,
        stop
    ).

set(Module, Levels) ->
    gen_server:call(
        ?MODULE,
        {set, Module, Levels}
    ).

log(Module, Level, Fmt, Args) ->
    gen_server:cast(
        ?MODULE,
        {log, Module, Level, Fmt, Args}
    ).

%%
%% gen_server implementation
%%

init(Config) -> 
    State = #state {
        logger = net_dushin_lethe_lists:find_value(
            Config, 
            logger,
            fun(Module, Level, Format, Args) -> default_log(Module, Level, Format, Args) end
        ),
        tab = ets:new(
            ?MODULE,
            [set, private]
        )
    },
    lists:foreach(
        fun({Module, Levels}) ->
            ets:insert(State#state.tab, {Module, Levels})
        end,
        net_dushin_lethe_lists:find_value(Config, filter, [])
    ),
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({set, Module, Levels}, _From, State) ->
    ets:insert(State#state.tab, {Module, Levels}),
    {reply, ok, State}.

handle_cast({log, Module, Level, Format, Args}, State) ->
    Logger = State#state.logger,
    case is_log(State#state.tab, Module, Level) of
        true ->
            Logger(Module, Level, Format, Args);
        _ ->
            ok
    end,
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%
%% Local Functions
%%

is_log(Ets, Module, Level) ->
    case ets:lookup(Ets, Module) of
        [] ->
            false;
        [{Module, Levels}|[]] ->
            lists:any(fun(X) -> case X of Level -> true; _ -> false end end, Levels);
        % garbage
        _ ->
            false
    end.

default_log(Module, Level, Fmt, Args) ->
    io:format(date_time() ++ " - ~p - ~p: " ++ Fmt ++ "~n", [Module | [Level | Args]]).


date_time() ->
    {Year, Month, Day} = erlang:date(),
    {Hour, Minute, Second} = erlang:time(),
    % "[" ++ Year ++ "-" ++ Month ++ "-" ++ Day ++ " " ++ Hour ++ ":" ++ Minute ++ ":" ++ Second ++ "]".
    io_lib:format("[~p-~p-~p ~p:~p:~p]", [Year, Month, Day, Hour, Minute, Second]).

