const {createPrologInstance, escapeQueryTemplate} = require('../prolog')

exports.create = function() {
  const session = createPrologInstance()
  const parsed = session.consult(`

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(js)).

:- dynamic(phase/1).
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

start_game :-
  (config(random_seed, single, S) -> set_random(S); true),
  set_phase(round(0, setup)),
  assign_role_cards,
  assign_other_starting_cards,
  assign_next_king,
  new_round.

set_phase(S) :- retractall(phase(_)), assertz(phase(S)).

assign_other_starting_cards :-
  forall(player(P), (
    assertz( card(vote, approve, down, [P, hand]) ),
    assertz( card(vote, reject, down, [P, hand]) )
  )).

random_role_pairings(Pairs) :-
  findall(P, player(P), Players),
  findall(R, config(roles, multi, R), Rs0),
  random_permutation(Rs0, Rs1),

  length(Players, PlayerCount),
  take(PlayerCount, Rs1, Roles),

  zip(Players, Roles, Pairs).

assign_role_cards :-
  random_role_pairings(Pairs),
  forall(
    member([P,R], Pairs),
    assertz( card(role, R, down, [P, assigned_role]) )
  ).

assign_next_king :- player(P), \\+ king(P), asserta(king(P)), !.
assign_next_king :-
  once(player(_)), % Ensure there is at least one player so we don't infinite loop.
  retractall(king(_)),
  assign_next_king.

current_king(P) :- once(king(P)).

new_round :-
  phase(round(N0, _)),
  retractall(vote(_,_)),
  retractall(quest_nominee(_)),
  N is N0 + 1,
  set_phase(round(N, nominate)).

%
% Config
%
config_option(roles, Role) :- good_role(Role) ; evil_role(Role).
config_option(role_permutation, Roles) :- member(R, Roles), config_option(roles, R).

%
% Static Data
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

required_evil(5, 2).
required_evil(6, 2).
required_evil(7, 3).
required_evil(8, 3).
required_evil(9, 3).
required_evil(10, 4).

party_size(5, 1, 2).
party_size(5, 2, 3).
party_size(5, 3, 2).
party_size(5, 4, 3).
party_size(5, 5, 3).
party_size(6, 1, 2).
party_size(6, 2, 3).
party_size(6, 3, 4).
party_size(6, 4, 3).
party_size(6, 5, 4).
party_size(7, 1, 2).
party_size(7, 2, 3).
party_size(7, 3, 3).
party_size(7, 4, 4).
party_size(7, 5, 4).
party_size(8, 1, 3).
party_size(8, 2, 4).
party_size(8, 3, 4).
party_size(8, 4, 5).
party_size(8, 5, 5).
party_size(9, 1, 3).
party_size(9, 2, 4).
party_size(9, 3, 4).
party_size(9, 4, 5).
party_size(9, 5, 5).
party_size(10, 1, 3).
party_size(10, 2, 4).
party_size(10, 3, 4).
party_size(10, 4, 5).
party_size(10, 5, 5).

%
% Data Rules
%
fails_required(PlayerCount, 4, 2) :- PlayerCount >= 7.
fails_required(_, _, 1).

required_nominations(Round, N) :-
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
  phase(round(Round, _)),
  required_nominations(Round, Req),
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
%% card(CardType, Name, CardFace, ZonePath).
:- dynamic(card/4).

config_option(random_seed, S) :- atomic(S).

add_config(Name, Value) :-
  config_option(Name, Value),
  assertz(config(Name, multi, Value)).

remove_config(Name, Value) :-
  retract(config(Name, multi, Value)).

set_config(Name, Value) :-
  config_option(Name, Value),
  retractall(config(Name, single, _)),
  assertz(config(Name, single, Value)).
  `)

  if (parsed !== true) {
    throw new Error(parsed)
  }

  session.get_warnings().forEach(msg => console.warn(msg.toString()))

  return {
    async addPlayer(player) {
      await session.query_all`assertz(player(${player})).`
    },
    async removePlayer(player) {
      await session.query_all`retract(player(${player})).`
    },
    async getPlayers() {
      const solutions = await session.query_all`player(P).`
      return solutions.map(s => s.P)
    },

    async getConfigOptions(name) {
      const solutions = await session.query_all`config_option(${name}, Value).`
      return solutions.map(s => s.Value)
    },
    async getConfig(name) {
      const solutions = await session.query_all`config(${name}, single, Value).`
      return solutions.map(s => s.Value)
    },
    async getMultiConfig(name) {
      const solutions = await session.query_all`config(${name}, multi, Value).`
      return solutions.map(s => s.Value)
    },

    async addConfig(name, value) {
      const errors = await session.query_all`
        config_invalid(Error, ${name}, ${value}) -> true;
        add_config(${name}, ${value}) -> true;
        atomic_list_concat(['Invalid config (', ${name}, ', ', ${value}, ')'], Error).
      `
      return errors.map(s => s.Error).filter(x => x)
    },

    async setConfig(name, value) {
      const errors = await session.query_all`
        config_invalid(Error, ${name}, ${value}) -> true;
        set_config(${name}, ${value}) -> true;
        atomic_list_concat(['Invalid config (', ${name}, ', ', ${value}, ')'], Error).
      `
      return errors.map(s => s.Error).filter(x => x)
    },

    async checkReadyToStart() {
      const errors = await session.query_all`check_ready_start(Error).`
      return errors.map(s => s.Error)
    },

    async start() {
      const solutions = await session.query_all`start_game.`
      if (solutions.length > 1) {
        console.warn(`[game.start] Warning: Multiple solutions`, solutions)
      }
      return solutions.length > 0
    },

    async getState() {
      const cards = (await session.query_all`
        card(Type, Name, Face, Path).
      `).map(row => {
        return {
          zone: row.Path.join('/'),
          type: row.Type,
          name: row.Name,
          face: row.Face,
        }
      })

      cards.sort(byCardContent)

      return { cards }
    },

    async debug(strings, ...values) {
      const result = await session.query_all(strings, ...values)

      const query = escapeQueryTemplate(strings, values)
      console.log(`?- ${query}\n`, result)
      return result
    }
  }
}

function byCardContent(a,b) {
  return (a.zone+a.type+a.name+a.face).localeCompare(b.zone+b.type+b.name+b.face)
}

exports.playerBoardSpec = {
  piles: {
    assigned_role: {
      pos: [0,0],
      size: [200, 300],
    },
    vote: {
      pos: [200, 0],
      size: [200, 300],
    },
    quest: {
      pos: [400, 0],
      size: [200, 300],
    },
  }
}
