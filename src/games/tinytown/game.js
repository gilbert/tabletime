const {createGame} = require('../../shared/logic-engine')

exports.create = function() {
  return createGame(`
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(js)).

check_ready_start('This game only supports 2 to 4 players.') :-
  player_count(N),
  (N < 2; N > 4).

% % % % % % %
% Game Setup
%
setup_game :-
  set_phase([setup]),
  times(5,  new_token(wheat_field, [shared, board, wheat_fields]) ),
  times(4,  new_token(vp_60, [shared, vp_60]) ),
  times(16, new_token(coin_1, [shared, resources, coin_1]) ),
  times(8,  new_token(coin_3, [shared, resources, coin_3]) ),

  forall(member(Resource, [wheat, fish, wood, stone]),
    times(15, new_token(Resource, [shared, resources, Resource]) )
  ),

  new_token(round, [shared, board, round, 1]),
  new_token(first_player, [shared, first_player]),

  % TODO: Choose random buildings based on config
  forall(bird(BuildingId),
    new_token(BuildingId, [shared, board, market])
  ),

  forall(objective(Obj, _),
    new_card(objective, Obj, down, [shared, objective_cards])
  ).

start_game :-
  (config(random_seed, single, S) -> set_random(S); true),
  setup_players.

setup_players :-
  forall(player(Player), setup_player(Player)),
  assign_next_turn_player,
  current_turn_player(FirstPlayer),
  token(first_player, _, TokenId),
  move_token(TokenId, [player, FirstPlayer, first_player_token]).

setup_player(Player) :-
  times(3, (
    once( token(coin_1, [shared, resources, coin_1], CoinId) ),
    move_token(CoinId, [player, Player, coin_1])
  )),
  player_count(PC),
  starting_objectives(PC, ObjCount),
  times(ObjCount, (
    once( card(objective, _, _, [shared, objective_cards], CardId) ),
    move_card(CardId, [player, Player, objective_cards])
  )),

  starting_workers(PC, WorkerCount),
  times(WorkerCount, new_token(worker, Player, [player, Player, workers]) ),

  starting_houses(PC, HouseCount),
  times(HouseCount, new_token(house, Player, [player, Player, houses]) ),

  new_token(vp, Player, [shared, board, vp, 0]),
  true.


% % % % %
% Config
%
config_option(roles, Role) :- good_role(Role) ; evil_role(Role).
config_option(role_permutation, Roles) :- member(R, Roles), config_option(roles, R).

% % % % % % %
% Static Data
%
%% building(Number, Cost, Vp).
building(b1, [wood(2)], 4).
building(b2, [wood(3)], 5).
building(b3, [wood(4)], 6).
building(b4, [wood(2)], 4).
building(b5, [wood(2)], 4).
building(b6, [wood(2)], 4).
building(b7, [wood(3)], 5).
building(b8, [wood(3)], 5).
building(b9, [wood(4)], 6).
building(b10, [stone(4)], 10).
building(b11, [wood(2), stone(2)], 7).
building(b12, [coin(6)], 2).
building(b13, [wood(1), stone(1)], 4).
building(b14, [stone(4)], 8).
building(b15, [wood(3), stone(3)], 0).
building(b16, [wood(1), stone(1)], 4).
building(b17, [stone(4)], 8).
building(b18, [stone(6)], 11).
building(b19, [stone(2)], 5).
building(b20, [stone(2), wheat(2)], 7).
building(b21, [stone(6)], 0).
building(b22, [wood(1), stone(1)], 4).
building(b23, [stone(2)], 5).
building(b24, [stone(4)], 8).

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

% % % % % % %
% Data Rules
%
fails_required(PlayerCount, 4, 2) :- PlayerCount >= 7.
fails_required(_, _, 1).

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
  false,
  phase([round, _, nominate]),
  current_turn_player(Actor),
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

can_peek(Player, CardId) :-
  card(vote, _, _, [player, Player | _], CardId).

can_peek(Player, CardId) :-
  card(role, _, _, [player, Player, assigned_role], CardId).

% % % % % % % % %
% Player Actions
%
action(next_phase, 0, button).
action(nominate, 2, drag).
action(vote, 2, drag).

action_label(next_phase, 'Proceed to Voting Phase') :- phase([round, _, nominate]).

%
% End Turn
%
available_action(Actor, end_turn, []) :-
  false.

act(end_turn, _, []) :-
  phase([round, N, nominate]),
  set_phase([round, N, vote]).

  `)
}

exports.testSetup = async function testSetup(game) {
  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')
  // await game.addPlayer('p4')
}
