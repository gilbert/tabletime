const {createGame} = require('../../shared/logic-engine')

exports.create = function() {
  return createGame(`
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(js)).

check_ready_start('This game only supports 3 to 5 players.') :-
  player_count(N),
  (N < 3; N > 5).

% % % % % % %
% Game Setup
%
setup_game :-
  % TODO: Inheritance cards
  set_phase(setup).

start_game :-
  (config(random_seed, single, S) -> set_random(S); true),
  setup_players,
  set_phase(choose_action).

setup_players :-
  forall(player(Player), setup_player(Player)).

setup_player(Player) :-
  forall(spendable(Card, _, _),
    new_card(Player, Card, down, [player, Player, hand])
  ),
  forall(attack_card(Player, Opponent),
    new_card(Player, [attack, Opponent], down, [player, Player, hand])
  ),
  for(1, 3, N, (
    new_card(Player, [royalty, N], up, [player, Player, court])
  )),
  true.

%
% Actions
%
action(choose_action, 2, drag).

available_action(Actor, choose_action, []) :-
  phase(choose_action),
  \\+ chosen_card(Actor).

available_action(Actor, choose_action, [Source]) :-
  available_action(Actor, choose_action, []),
  card(Actor, Type, _, [player, Actor, hand], Source),
  can_play_card(Type).

available_action(Actor, choose_action, [Source, commit]) :-
  available_action(Actor, choose_action, [Source]).

act(Actor, choose_action, [Id, commit]) :-
  move_card(Id, [shared, Actor, chosen], down),
  all_chosen -> set_next_execute_step ; true.

todo(Todo) :-
  phase(choose_action),
  player(Player),
  \\+ card(_, _, _, [shared, Player, chosen], _),
  atomic_list_concat(['{{', Player, '}} must play a card'], Todo).

todo(Todo) :-
  phase([execute, jester]),
  chosen_card(Player, jester),
  atomic_list_concat(['{{', Player, '}} must add their Jester to their court'], Todo).

todo(Todo) :-
  phase([execute, seer]),
  chosen_card(Player, jester),
  atomic_list_concat(['{{', Player, '}} must activate their Seer'], Todo).

todo(Todo) :-
  phase([execute, hit, Targets]),
  member(Player, Targets),
  atomic_list_concat(['{{', Player, '}} must take their damage'], Todo).

todo(Todo) :-
  phase([execute, marshal_hit, Targets]),
  members(Player, Targets),
  atomic_list_concat(['{{', Player, '}} must take their marshal damage'], Todo).

todo(Todo) :-
  phase([execute]),
  member([Player, gold], Chosen),
  atomic_list_concat(['{{', Player, '}} must buy with Gold'], Todo), !.

todo(Todo) :-
  phase(execute),
  member([Player, banner], Chosen),
  atomic_list_concat(['{{', Player, '}} must establish their Banner'], Todo), !.

todo(Todo) :-
  phase(execute),
  member([Player, fanatic], Chosen),
  atomic_list_concat(['{{', Player, '}} must activate their Fanatic'], Todo), !.

% % % % %
% Config
%
config_option(roles, Role) :- good_role(Role) ; evil_role(Role).
config_option(role_permutation, Roles) :- member(R, Roles), config_option(roles, R).

% % % % % % %
% Static Data
%
%% spendable(Name, VpInHand, VpInPlay).
spendable(gold, 5, 0).
spendable(seer, 5, 0).
spendable(thief, 3, 0).
spendable(banner, 1, 6).
spendable(jester, 5, 2).
spendable(fanatic, 0, 0).
spendable(marshals, 8, 0).

attack_card(Attacker, Defender) :-
  player(Defender),
  Attacker \\== Defender.

%% bird(BuildingNumber)
bird(1).
bird(4).
bird(7).
bird(8).
bird(9).
bird(10).
bird(13).
bird(14).
bird(16).
bird(21).
bird(23).
bird(24).

%% objective(Number, Vp) :- PlayableConditions.
objective(obj1, 3).
objective(obj2, 3).
objective(obj3, 2) :- true. % TODO
objective(obj4, 2) :-
  token(Building, [shared, board, market], _),
  building(Building, ResourceCost, _),
  length(ResourceCost, TypeCount),
  TypeCount >= 2.
objective(obj5, 3).
objective(obj6, 2).
objective(obj7, 3).
objective(obj8, 2) :- true. % TODO
objective(obj9, 3).
objective(obj10, 3).
objective(obj11, 2) :-
  token(Building, [shared, board, market], _),
  building(Building, ResourceCost, _),
  member(stone(X), ResourceCost),
  X >= 3.
objective(obj12, 2) :-
  token(Building, [shared, board, market], _),
  building(Building, ResourceCost, _),
  member(wood(X), ResourceCost),
  X >= 3.
objective(obj13, 2) :- true. % HOW??
objective(obj14, 3) :- true. % TODO
objective(obj15, 2) :- true. % TODO
objective(obj16, 2) :- true. % TODO
objective(obj17, 2).
objective(obj18, 2).


%% starting_objectives(PlayerCount, ObjectiveCount).
starting_objectives(2, 4).
starting_objectives(3, 3).
starting_objectives(4, 2).

%% starting_workers(PlayerCount, WorkerCount).
starting_workers(2, 5).
starting_workers(3, 4).
starting_workers(4, 3).

%% starting_houses(PlayerCount, HouseCount).
starting_houses(2, 7).
starting_houses(3, 6).
starting_houses(4, 6).

vp_marker(Player, TokenName) :- atomic_list_concat([vp, '_', Player], TokenName).

% % % % % % % % % % % %
% Game Rules & Helpers
%
fails_required(PlayerCount, 4, 2) :- PlayerCount >= 7.
fails_required(_, _, 1).

chosen_card(Player) :- chosen_card(Player, _, _).

chosen_card(Player, Type) :- chosen_card(Player, Type, _).

chosen_card(Player, Action, CardId) :-
  card(Player, Action, _, [shared, Player, chosen], CardId).

all_chosen :-
  forall(player(P), chosen_card(P)).

can_play_card([attack, Player]) :- player(Player), dead(Player), !, false.
can_play_card(_).

set_next_execute_step :-
  phase(Current),
  fixpoint(next_resolve_step, choose_action, Next),
  set_phase(Next).

next_resolve_step(choose_action, [execute, jester]).
next_resolve_step([execute, jester], [execute, seer]) :- \\+ chosen_card(_, jester).

% LAST TIME: Complete this logic
%next_resolve_step([execute, seer], [execute, attack, Targets]) :-
%  forall(chosen_card(_, [attack, Target])).

dead(Player) :-
  \\+ card(_, _, _, [player, Player, court], _).

fixpoint(Pred, Input, Input) :- call(Pred, Input, Input), !.
fixpoint(Pred, Input, Final) :-
  call(Pred, Input, Middle),
  fixpoint(Pred, Middle, Final).

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
% Player Cards
%
draggable(Player, Id) :-
  phase(choose_action),
  \\+ chosen_card(Player),
  card(Player, _, _, [player, Player, hand], Id).

droppable(Player, DraggableId, DropZone, choose_action, [DraggableId, commit]) :-
  \\+ chosen_card(Player),
  card(Player, _, _, _, DraggableId),
  DropZone = [shared, Player, chosen].

can_peek(Player, CardId) :-
  card(_, _, _, [player, Player | _], CardId).

% % % % % % % % %
% Player Actions
%
action(next_phase, 0, button).
action(nominate, 2, drag).
action(vote, 2, drag).

% action_label(next_phase, 'Proceed to Voting Phase') :- phase([round, _, nominate]).

%
% End Turn
%
available_action(_, end_turn, []) :-
  false.

act(_, end_turn, []) :-
  phase([round, N, nominate]),
  set_phase([round, N, vote]).

  `)
}

exports.testSetup = async function testSetup(game) {
  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')

  await game.start()

  // await game.act('p1', 'nominate', [114, 'p1']) // TEMP
  // await game.act('p1', 'nominate', [117, 'p2']) // TEMP
  // await game.act('p1', 'next_phase', []) // TEMP
  return true
}
