-module(emud_tcp_protocol).
-include_lib("emud/include/emud.hrl").
-behaviour(cowboy_protocol).

-record(state, {
        sess_id,
        session,
        socket  :: inet:socket(),
        transport :: module(),
        timeout :: timeout(),
        buffer = <<>> :: binary()
    }).

-export([start_link/4, init/4, listen/1]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    global:register_name(emud_data, Pid),
    {ok, Pid}.

init(LPid, Socket, Trans, _Opts) ->
    cowboy:accept_ack(LPid),
    {ok, SessId, Sess} = emud_srv:connect(),
    State = #state{socket=Socket, timeout=60000, transport=Trans, sess_id=SessId, session=Sess},
    spawn(?MODULE, listen, [State]),
    recv(State).

parse(State=#state{buffer=Buffer, session=Sess, sess_id=SessId}) ->
    case erlang:decode_packet(4, Buffer, []) of
        {ok, Message, Rest} ->
            Cmd = emud_tcp_data:decode_cmd({text, Message}),
            emud_sess:handle_cmd(Sess, Cmd#cmd{sessid=SessId}),
            listen(State#state{buffer= << Rest/binary >>});
        {more, _Length} -> listen(State);
        {error, _Reason} -> terminate(State) %% Neeed to close transport.
    end.

listen(State=#state{socket=S, transport=Trans, timeout=T, buffer=Buffer}) ->
    case Trans:recv(S, 0, T) of
        {ok, Data} -> 
            parse(State#state{buffer = << Buffer/binary, Data/binary >>});
        {error, _Reason} -> terminate(State)
    end.

recv(State=#state{socket=S, transport=Trans}) ->
    receive
       {emud_msg, _Ref, Msg} -> 
            Emsg = emud_tcp_data:encode_msg(Msg),
            Length = byte_size(Emsg),
            Trans:send(S, [<< Length:32 >>, << Emsg/binary >> ])
    end,
    recv(State).

terminate(State=#state{socket=Socket, transport=Trans}) ->
    Trans:close(Socket),
    ok.
