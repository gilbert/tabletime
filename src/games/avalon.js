const {createPrologInstance} = require('../prolog')

exports.create = function() {
  const session = createPrologInstance()
  const parsed = session.consult(`

:- use_module(library(lists)).
:- dynamic(player/1).
:- dynamic(game_stage/1).
:- dynamic(assigned_role/2).
:- dynamic(king/1).
:- dynamic(nominee/1).
:- dynamic(vote/2).

:- op(995, xfx, if).
:- op(995, xfx, else).
:- op(995, xfx, unless).

check_ready('This game only supports 5 to 10 players.') :-
  player_count(N),
  (N < 5; N > 10).

role(civilian).
role(merlin).
role(percible).

role(minion).
role(minion).
role(mordrid).

game_stage(round(0, setup)).

new_game_stage(S) :- retractall(game_stage(_)), assertz(game_stage(S)).

random_role_pairings(Pairs) :-
  findall(P, player(P), Players),
  findall(R, role(R), Rs0),
  random_permutation(Rs0, Rs1),

  length(Players, PlayerCount),
  take(PlayerCount, Rs1, Roles),

  zip(Players, Roles, Pairs).

assign_roles :-
  random_role_pairings(Pairs),
  forall(member([P,R], Pairs), assertz(assigned_role(P,R))).

assign_next_king :- player(P), \+ king(P), assertz(king(P)), !.
assign_next_king :-
  once(player(_)), % Ensure there is at least one player so we don't infinite loop.
  retractall(king(_)),
  assign_next_king.

current_king(P) :- once(king(P)).

start_game :-
  assign_roles,
  assign_next_king,
  new_round.

new_round :-
  game_stage(round(N0, _)),
  retractall(vote(_,_)),
  retractall(quest_nominee(_)),
  N is N0 + 1,
  new_game_stage(round(N, nominate)).

%
% Data Rules
%
required_nominations(5, 1, 2).
required_nominations(5, 2, 3).
required_nominations(5, 3, 2).
required_nominations(5, 4, 3).
required_nominations(5, 5, 3).
required_nominations(6, 1, 2).
required_nominations(6, 2, 3).
required_nominations(6, 3, 4).
required_nominations(6, 4, 3).
required_nominations(6, 5, 4).
required_nominations(7, 1, 2).
required_nominations(7, 2, 3).
required_nominations(7, 3, 3).
required_nominations(7, 4, 4).
required_nominations(7, 5, 4).
required_nominations(8, 1, 3).
required_nominations(8, 2, 4).
required_nominations(8, 3, 4).
required_nominations(8, 4, 5).
required_nominations(8, 5, 5).
required_nominations(9, 1, 3).
required_nominations(9, 2, 4).
required_nominations(9, 3, 4).
required_nominations(9, 4, 5).
required_nominations(9, 5, 5).
required_nominations(10, 1, 3).
required_nominations(10, 2, 4).
required_nominations(10, 3, 4).
required_nominations(10, 4, 5).
required_nominations(10, 5, 5).

fails_required(PlayerCount, 4, N) :- PlayerCount >= 7, N = 2.
fails_required(_, _, 1).

nominations_req(Round, N) :-
  player_count(PC),
  required_nominations(PC, Round, N).

nomination_count(N) :-
  count(nominee(_), N).

player_count(N) :-
  count(player(_), N).

%
% Player Actions
%
nominate(Actor, Target) -->
  {
    game_stage(round(Round, nominate))
  },
  "You are not the king" unless current_king(Actor),
  "You have already nominated that player" if nominee(Target),
  {
    nominations_req(Round, Req),
    nomination_count(Current)
  },
  "You have aready issued all your nominations" unless Current < Req.

%
% Helpers
%
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

count(Predicate, N) :-
  findall(_, Predicate, Solutions),
  length(Solutions, N).

else(Cond, Message), [Result] --> [nil], !, { call(Cond) -> Result = nil ; Result = Message }.
else(_, _), [NotNil] --> [NotNil].

unless(Message, Cond) --> else(Cond, Message).
if(Message, Cond) --> else(\+ Cond, Message).

%
% Tau-only stuff (written for SWI)
%
% take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
% take(_, [], []).
% take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).
take(0, _, []).
take(N, [H|T], [H|TT]):- succ(NN,N), take(NN, T, TT).
  `)

  if (parsed !== true) {
    throw new Error(parsed)
  }

  session.get_warnings().forEach(msg => console.warn(msg.toString()))

  return {
    async addPlayer(player) {
      await session.queryAll(`assertz(player(${player})).`)
    },
    async removePlayer(player) {
      await session.queryAll(`retract(player(${player})).`)
    },
    async getPlayers() {
      const solutions = await session.queryAll(`player(P).`)
      return solutions.map(s => s.P)
    },
    async checkReady() {
      const solutions = await session.queryAll(`check_ready(Error).`)
      return solutions.map(s => s.Error)
    }
  }
}
