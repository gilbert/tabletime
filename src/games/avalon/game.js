const {createPrologInstance, escapeQueryTemplate, pl} = require('../../prolog')

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

todo(Todo) :-
  phase([round, _, nominate]),
  current_king(King),
  count(available_action(King, nominate, [_]), Left),
  Left > 0,
  pluralize(Left, 'more player', LeftStr),
  atomic_list_concat(['King must nominate ', LeftStr], Todo).

todo(Todo) :-
  phase([round, _, vote]),
  player(P),
  \\+ card(vote, _, _, [player, P, vote], _),
  atomic_list_concat(['{{', P, '}} must cast a vote'], Todo).

% % % % % % %
% Game Setup
%
start_game :-
  (config(random_seed, single, S) -> set_random(S); true),
  set_phase([round, 0, setup]),
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
    new_card(quest, success, down, [shared, standby, quest_cards]),
    new_card(quest, fail, down, [shared, standby, quest_cards]),
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
  phase([round, N0, _]),
  pickup_vote_cards,
  assign_next_king,
  N is N0 + 1,
  set_nomination_tokens(N),
  set_phase([round, N, nominate]).

pickup_vote_cards :-
  forall( card(vote, Decision, _, [player, Player, vote], Id), (
    retract( card(_, _, _, _, Id) ),
    assertz( card(vote, Decision, down, [player, Player, hand], Id) )
  )).

%shuffle_vote_cards :-
%  findall(Id, card(vote, _, _, _, Id), VoteCards),
%
%  random_permutation(VoteCards, Shuffled),
%
%  forall(Shuffled = card(vote, Decision, _, [player, Player, hand], Id), (
%    retract( card(_, _, _, _, Id) ),
%    assertz( card(vote, Decision, down, [player, Player, hand], Id) )
%  )).

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

% % % % %
% Config
%
config_option(roles, Role) :- good_role(Role) ; evil_role(Role).
config_option(role_permutation, Roles) :- member(R, Roles), config_option(roles, R).

% % % % % % %
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

% % % % % % %
% Data Rules
%
fails_required(PlayerCount, 4, 2) :- PlayerCount >= 7.
fails_required(_, _, 1).

player_count(N) :-
  count(player(_), N).

% % % % % % % %
% Access Rules
%
%
% draggable/2 SHOULD validate permissions.
% This provides better UX as the user can't drag something they can't interact with.
%
% In constract, droppable/5 SHOULD NOT validate permissions.
% It only constructs an action; available_action/3 will validate elsewhere.
%

%
% Nomination tokens
%
draggable(Actor, Id) :-
  phase([round, _, nominate]),
  current_king(Actor),
  token(nomination, [player, Actor, unused_nominations], Id).

droppable(_, DraggableId, DropZone, nominate, [DraggableId, Target]) :-
  token(nomination, _, DraggableId),
  player(Target),
  DropZone = [player, Target, status],
  \\+ token(nomination, DropZone, _).

%
% Vote cards
%
draggable(Actor, Id) :-
  phase([round, _, vote]),
  available_action(Actor, vote, [Id]).

droppable(Actor, DraggableId, DropZone, vote, [DraggableId, commit]) :-
  card(vote, _, _, _, DraggableId),
  DropZone = [player, Actor, vote].

zone_hidden(P1, [player, P2, hand]) :- P1 \\== P2.

% % % % % % % % %
% Player Actions
%
action(next_phase, 0, button).
action(nominate, 2, drag).
action(vote, 2, drag).

action_label(next_phase, 'Proceed to Voting Phase') :- phase([round, _, nominate]).

%
% Next Phase
%
available_action(Actor, next_phase, []) :-
  current_king(Actor),
  \\+ todo(_).

act(next_phase, _, []) :-
  phase([round, N, nominate]),
  set_phase([round, N, vote]).

%
% Nominate
%
available_action(Actor, nominate, []) :-
  phase([round, _, nominate]),
  current_king(Actor),
  once( token(nomination, [player, Actor, unused_nominations], _) ).

available_action(Actor, nominate, [Source]) :-
  available_action(Actor, nominate, []),
  token(nomination, [player, Actor, unused_nominations], Source).

available_action(Actor, nominate, [Source, Target]) :-
  available_action(Actor, nominate, [Source]),
  player(Target),
  \\+ token(nomination, [player, Target, status], _).

act(nominate, _, [Source, Target]) :-
  move_token(Source, [player, Target, status]).

%
% Vote
%
available_action(Actor, vote, []) :-
  phase([round, _, vote]),
  \\+ card(vote, _, _, [player, Actor, vote], _).

available_action(Actor, vote, [Source]) :-
  available_action(Actor, vote, []),
  card(vote, _, _, [player, Actor, hand], Source).

available_action(Actor, vote, [Source, commit]) :-
  available_action(Actor, vote, [Source]).

act(vote, Actor, [Source, commit]) :-
  move_card(Source, [player, Actor, vote], down).


%
% Helpers
%
zip([], [], []).
zip([X|Xs], [Y|Ys], [[X,Y]|Zs]) :- zip(Xs,Ys,Zs).

count(Predicate, N) :-
  findall(_, Predicate, Solutions),
  length(Solutions, N).

pluralize(N, Thing, Out) :- pluralize(N, Thing, 's', Out).
pluralize(1, Thing, _, Out) :-
  atomic_list_concat([1, ' ', Thing], Out), !.
pluralize(N, Thing, Suffix, Out) :-
  atomic_list_concat([N, ' ', Thing, Suffix], Out).


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

% zone_hidden(Onlooker, Zone).
zone_hidden(_, _) :- false.

try_act(Actor, Action, Args) :-
  action(Action, ReqLen, _),
  length(Args, ReqLen),
  available_action(Actor, Action, Args),
  act(Action, Actor, Args).

% act(Actor, Action, Args)
act(_, _, _) :- false.
action(_, _, _) :- false.
todo(_) :- false.
action_label(Action, Action).

% draggable(Actor, Id)
draggable(_, _) :- false.

object_id(100).
new_object_id(Id) :-
  object_id(Id),
  NextId is Id + 1,
  retractall( object_id(_) ),
  assertz( object_id(NextId) ).

new_card(Type, Name, CardFace, ZonePath) :-
  new_object_id(Id),
  assertz(card(Type, Name, CardFace, ZonePath, Id)).

move_card(Id, NewZone, Face) :-
  retract( card(Type, Name, _, _, Id) ),
  assertz( card(Type, Name, Face, NewZone, Id) ).

new_token(Type, ZonePath) :-
  new_object_id(Id),
  assertz(token(Type, ZonePath, Id)).

move_token(Id, NewZone) :-
  retract( token(Type, _, Id) ),
  assertz( token(Type, NewZone, Id) ).

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
    throw new Error(parsed.args[0])
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
      const phases = await session.query_all`phase(Phase).`
      if (phases.length > 1) {
        console.warn(`[game.start] Warning: Multiple phases`, phases)
      }
      const phase = phases[0].Phase

      const cards = (await session.query_all`
        card(Type, Name, Face, Zone, Id).
      `).map(row => {
        const card = {
          id:   row.Id,
          type: row.Type,
          face: row.Face,
          zone: row.Zone.join('/'),
        }
        if (row.Name !== 'NONE') {
          card.name = row.Name
        }
        return card
      })

      cards.sort(byCardContent)

      const tokens = (await session.query_all`
        token(Type, Zone, Id).
      `).map(row => {
        return {
          id:   row.Id,
          type: row.Type,
          zone: row.Zone.join('/'),
        }
      })

      tokens.sort(byTokenContent)

      return { phase, cards, tokens }
    },

    async getAvailableActions(player, action=null, args) {
      args = (args || []).slice()
      if (action) {
        args.push(pl.escape.var('Target'))
        return (await session.query_all`
          available_action(${player}, ${action}, ${args}),
          action(${action}, _, Type).
        `)
          .map(row => row.Target)
      }
      else {
        return (await session.query_all`
          available_action(${player}, Action, ${args}),
          action(Action, _, Type),
          once( action_label(Action, Label) ).
        `)
          .map(row => ({ name: row.Action, type: row.Type, label: row.Label }))
      }
    },

    async getDraggables(player) {
      const solutions = await session.query_all`draggable(${player}, Id).`
      const result = {}
      solutions.forEach(row => {
        result[row.Id] = true
      })
      return result
    },

    async getDroppables(player, draggableId) {
      // TODO: Consider dragging onto other cards / tokens
      const solutions = await session.query_all`
        droppable(${player}, ${draggableId}, DropZone, Action, Args).
      `
      const zones = {}
      solutions.forEach(row => {
        zones[row.DropZone.join('/')] = [row.Action, row.Args]
      })
      return { zones }
    },

    async act(player, action, args) {
      const successes = await session.query_all`
        try_act(${player}, ${action}, ${args}).
      `
      if (successes.length > 1) {
        console.warn(`[game.act] Warning: Multiple solutions for (${
          player
        }, ${action}, [${args.join(', ')}])`, successes)
      }
      return successes.length > 0
    },

    async getCurrentPhaseTodos() {
      const solutions = await session.query_all`todo(Todo).`
      return solutions.map(s => s.Todo)
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
