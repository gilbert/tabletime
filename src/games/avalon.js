const {createPrologInstance} = require('../prolog')

exports.create = function() {
  const session = createPrologInstance()
  const parsed = session.consult(`

:- use_module(library(lists)).
:-use_module(library(js)).

:- dynamic(game_stage/1).
:- dynamic(assigned_role/2).
:- dynamic(king/1).
:- dynamic(nominee/1).
:- dynamic(vote/2).

check_ready_start('This game only supports 5 to 10 players.') :-
  player_count(N),
  (N < 5; N > 10).

check_ready_start(Error) :-
  player_count(PC),
  count(config(roles, multi, _), RC),
  PC =\\= RC,
  atomic_list_concat(['Please choose exactly ', PC, ' roles.'], Error).

check_ready_start(Error) :-
  player_count(PC),
  required_evil(PC, Target),
  count((config(roles, multi, Role), evil_role(Role)), EC),
  EC =\\= Target,
  atomic_list_concat(['Please choose exactly ', Target, ' evil roles.'], Error).

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
% Config
%
good_role(R) :- member(R, [
  merlin,
  percival,
  servant_1,
  servant_2,
  servant_3,
  servant_4,
  servant_5
]).

evil_role(R) :- member(R, [
  assassin,
  morgana,
  mordred,
  oberon,
  minion_1,
  minion_2,
  minion_3
]).

config_option(roles, Role) :- good_role(Role) ; evil_role(Role).

required_evil(5, 2).
required_evil(6, 2).
required_evil(7, 3).
required_evil(8, 3).
required_evil(9, 3).
required_evil(10, 4).

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

%
% Data Rules
%
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
validate_action(nominate, (Actor, _), 'You are not the king.') :- \\+ current_king(Actor).
validate_action(nominate, (_, Target), 'Target is already nominated.') :- nominee(Target).
validate_action(nominate, _, 'You are done nominating.') :-
  nominations_req(Round, Req),
  nomination_count(Current),
  Current = Req.

%
% Helpers
%
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

count(Predicate, N) :-
  findall(_, Predicate, Solutions),
  length(Solutions, N).

%
% Tabletime Engine
%
:- dynamic(player/1).
:- dynamic(config/3).
:- dynamic(config_invalid/3).

add_config(Name, Value) :-
  config_option(Name, Value),
  assertz(config(Name, multi, Value)).

remove_config(Name, Value) :-
  retract(config(Name, multi, Value)).

set_config(Name, Value) :-
  config_option(Name, Value),
  retract(config(Name, single, _)),
  assertz(config(Name, single, Value)).

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

    async getConfigOptions(name) {
      const solutions = await session.queryAll(`config_option(${name}, Value).`)
      return solutions.map(s => s.Value)
    },
    async getConfig(name) {
      const solutions = await session.queryAll(`config(${name}, single, Value).`)
      return solutions.map(s => s.Value)
    },
    async getMultiConfig(name) {
      const solutions = await session.queryAll(`config(${name}, multi, Value).`)
      return solutions.map(s => s.Value)
    },

    async addConfig(name, value) {
      const errors = await session.queryAll(`
        config_invalid(Error, ${name}, ${value}) -> true;
        add_config(${name}, ${value}) -> true;
        atomic_list_concat(['Invalid config ("', ${name}, '", "${value}")'], Error).
      `)
      return errors.map(s => s.Error).filter(x => x)
    },

    async checkReadyToStart() {
      const errors = await session.queryAll(`check_ready_start(Error).`)
      return errors.map(s => s.Error)
    },

    async debug(query) {
      const result = await session.queryAll(query)
      console.log(`?- ${query}\n`, result)
      return result
    }
  }
}
