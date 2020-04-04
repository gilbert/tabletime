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
  setup_board,
  create_vote_cards,
  new_round.

set_phase(S) :- retractall(phase(_)), assertz(phase(S)).

setup_board :-
  player_count(PC),
  new_token(reject, [shared, board, reject_count, 0]),
  new_token(king, [shared, standby, king]),

  forall( between(1, 5, _), (
    new_card(mission, 'NONE', up, [shared, standby, mission_tokens])
  )),

  findall(Sizes, party_size(PC, _, Sizes), Sizes),
  max_list(Sizes, LargestPartySize),
  forall( between(1, LargestPartySize, _), (
    new_card(quest, success, up, [shared, standby, quest_cards]),
    new_card(quest, fail, up, [shared, standby, quest_cards]),
    new_token(nomination, [shared, standby, nomination_tokens])
  )).

create_vote_cards :-
  forall( player(P), (
    new_card(vote, approve, down, [player, P, hand]),
    new_card(vote, reject, down, [player, P, hand])
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
    new_card(role, R, down, [player, P, assigned_role])
  ).

assign_next_king :-
  % Grab the next player that hasn't been king this goaround
  player(Player), \\+ king(Player), !,

  % Mark them as having been king
  asserta(king(Player)),

  % Hand them the token
  retract( token(king, _, Id) ),
  assertz( token(king, [player, Player, status], Id) ).

% This case occurs when all players have been king
assign_next_king :-
  once(player(_)), % Ensure there is at least one player so we don't infinite loop.
  retractall(king(_)),
  assign_next_king.

current_king(P) :- once(king(P)).

new_round :-
  phase(round(N0, _)),
  pickup_vote_cards,
  assign_next_king,
  N is N0 + 1,
  set_nomination_tokens(N),
  set_phase(round(N, nominate)).

pickup_vote_cards :-
  forall( card(vote, Decision, _, [player, Player, vote], Id), (
    retract( card(_, _, _, _, Id) ),
    assertz( card(vote, Decision, down, [player, Player, hand], Id) )
  )).

set_nomination_tokens(Round) :-
  player_count(PC),
  current_king(King),

  % Take back all tokens from previously nominated players to shared area
  forall( token(nomination, [player, _, status], Id), (
    retract( token(_, _, Id) ),
    assertz( token(nomination, [shared, standby, nomination_tokens], Id) )
  )),

  % Give the king a number of tokens equal to the current round's party size
  party_size(PC, Round, PartySize),
  forall( between(1, PartySize, _), (
    once(retract( token(nomination, [shared, standby, nomination_tokens], Id) )),
    assertz( token(nomination, [player, King, unused_nominations], Id) )
  )).

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

% required_evil(PlayerCount, RequiredEvil)
required_evil(5, 2).
required_evil(6, 2).
required_evil(7, 3).
required_evil(8, 3).
required_evil(9, 3).
required_evil(10, 4).

% party_size(PlayerCount, Round, QuestPartySize)
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
% Access Rules
%
zone_visible(Player, [player, Player, hand]).

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
:- dynamic(object_id/1).
%% card(Type, Name, CardFace, ZonePath, Id).
:- dynamic(card/5).
%% token(Type, ZonePath, Id).
:- dynamic(token/3).

% zone_visible(Onlooker, Zone).
zone_visible(_, [shared, _]).
zone_visible(_, _) :- false.

object_id(100).
new_object_id(Id) :-
  object_id(Id),
  NextId is Id + 1,
  retractall( object_id(_) ),
  assertz( object_id(NextId) ).

new_card(Type, Name, CardFace, ZonePath) :-
  new_object_id(Id),
  assertz(card(Type, Name, CardFace, ZonePath, Id)).

new_token(Type, ZonePath) :-
  new_object_id(Id),
  assertz(token(Type, ZonePath, Id)).

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
        card(Type, Name, Face, Path, Id).
      `).map(row => {
        const card = {
          id:   row.Id,
          type: row.Type,
          face: row.Face,
          zone: row.Path.join('/'),
        }
        if (row.Name !== 'NONE') {
          card.name = row.Name
        }
        return card
      })

      cards.sort(byCardContent)

      const tokens = (await session.query_all`
        token(Type, Path, Id).
      `).map(row => {
        return {
          id:   row.Id,
          type: row.Type,
          zone: row.Path.join('/'),
        }
      })

      tokens.sort(byTokenContent)

      return { cards, tokens }
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
function byTokenContent(a,b) {
  return (a.zone+a.type).localeCompare(b.zone+b.type)
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
