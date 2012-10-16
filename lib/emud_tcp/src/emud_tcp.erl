-module(emud_tcp).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    ranch:start_listener(emud_tcp_listener, 100,
        ranch_tcp, [{port, 8081}],
        emud_tcp_protocol, []
    ).

stop(_State) ->
    ranch:stop_listener(emud_tcp_listener),
    ok.
