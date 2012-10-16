-module(emud_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
            {[], emud_http_websocket, []}
        ]}
    ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    ranch:start_listener(emud_http_listener, 100,
        cowboy_tcp, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).

stop(_State) ->
    ranch:stop_listener(emud_http_listener),
    ok.
