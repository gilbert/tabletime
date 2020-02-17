:- dynamic(counter/2).

create_ticker(CounterId, ThreadId) :-
  assertz(counter(CounterId, 10)),
  thread_create(tick(CounterId), ThreadId).

tick(CounterId) :-
  retract(counter(CounterId, N0)),
  N1 is N0 + 1,
  assertz(counter(CounterId, N1)),
  sleep(1), % Time in seconds
  tick(CounterId).
