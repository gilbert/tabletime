:- module(ws, [on_connect/1]).

on_connect(WebSocket) :-
  listen(proto_counter, counter_updated(proto_counter), (
    counter(proto_counter, N),
    ws_send(WebSocket, text(N))
  )),
  echo(WebSocket).

echo(WebSocket) :-
  ws_receive(WebSocket, Message),
    (   Message.opcode == close
    ->  true
    ;   string_concat('Hey, you said ', Message.data , MessageRes),
        ws_send(WebSocket, text(MessageRes)),
        echo(WebSocket)
    ).

create_ticker(CounterId, ThreadId) :-
  assertz(counter(CounterId, 10)),
  thread_create(tick(CounterId), ThreadId).

tick(CounterId) :-
  retract(counter(CounterId, N0)),
  N1 is N0 + 1,
  assertz(counter(CounterId, N1)),
  broadcast(counter_updated(CounterId)),
  sleep(1), % Time in seconds
  tick(CounterId).

:- create_ticker(proto_counter, _).
