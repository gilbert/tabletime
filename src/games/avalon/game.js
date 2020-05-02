const {createGame} = require('../../shared/logic-engine')

exports.create = function() {
  return createGame(`
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(js)).

:- dynamic(king/1).

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
setup_game :-
  set_phase([round, 0, setup]).

start_game :-
  (config(random_seed, single, S) -> set_random(S); true),
  assign_role_cards,
  setup_board,
  create_vote_cards,
  new_round.

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
  `)
}

exports.testSetup = async function testSetup(game) {
  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')
  await game.addPlayer('p4')
  await game.addPlayer('p5')

  await game.addConfig('roles', 'merlin')
  await game.addConfig('roles', 'servant_1')
  await game.addConfig('roles', 'servant_2')
  await game.addConfig('roles', 'assassin')
  await game.addConfig('roles', 'minion_1')
}
