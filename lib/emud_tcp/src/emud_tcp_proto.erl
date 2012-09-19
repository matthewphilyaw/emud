-module(emud_tcp_proto).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(emud_tcp_listener, 100,
        cowboy_tcp_transport, [{port, 8081}],
        emud_tcp_protocol, []
    ).

stop(_State) ->
    cowboy:stop_listener(emud_tcp_listener),
    ok.
