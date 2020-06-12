const pl = require('tau-prolog')
require('tau-prolog/modules/lists')(pl)
require('tau-prolog/modules/statistics')(pl)
require('tau-prolog/modules/js')(pl)
require('tau-prolog/modules/system')(pl)
require('tau-prolog/modules/random')(pl)

exports.createGame = function createGame(code) {
  const session = pl.create()
  session.createRandom = createRandom
  const parsed = session.consult(`${code}\n${gameEngineCode}`)

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

    async setup() {
      const solutions = await session.query_all`setup_game.`
      if (solutions.length > 1) {
        console.warn(`[game.setup] Warning: Multiple solutions`, solutions)
      }
      return solutions.length > 0
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
          type: identifierTermToString(row.Type),
          face: row.Face,
          zone: row.Zone.join('/'),
        }
        if (row.Name !== 'NONE') {
          card.name = identifierTermToString(row.Name)
        }
        return card
      })

      cards.sort(byCardContent)

      const tokens = (await session.query_all`
        token(Type, Zone, Id).
      `).map(row => {
        return {
          id:   row.Id,
          type: identifierTermToString(row.Type),
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

    async getPeekables(player) {
      const solutions = await session.query_all`can_peek(${player}, Id).`
      const result = {}
      solutions.forEach(row => {
        result[row.Id] = true
      })
      return result
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

      const query = pl.utils.escapeQueryTemplate(strings, values)
      console.log(`?- ${query}\n`, result)
      return result
    }
  }
}

//
// Seeded random logic
//
function createRandom(seed) {
  var seed = xmur3(seed);
  return sfc32(seed(), seed(), seed(), seed());
}

// A seeded random function maker
function sfc32(a, b, c, d) {
  return function() {
    a >>>= 0; b >>>= 0; c >>>= 0; d >>>= 0;
    var t = (a + b) | 0;
    a = b ^ b >>> 9;
    b = c + (c << 3) | 0;
    c = (c << 21 | c >>> 11);
    d = d + 1 | 0;
    t = t + d | 0;
    c = c + t | 0;
    return (t >>> 0) / 4294967296;
  }
}

// A hash function
function xmur3(str) {
  for(var i = 0, h = 1779033703 ^ str.length; i < str.length; i++)
      h = Math.imul(h ^ str.charCodeAt(i), 3432918353),
      h = h << 13 | h >>> 19;
  return function() {
      h = Math.imul(h ^ h >>> 16, 2246822507);
      h = Math.imul(h ^ h >>> 13, 3266489909);
      return (h ^= h >>> 16) >>> 0;
  }
}

function byCardContent(a,b) {
  return (a.zone+a.type+a.name+a.face).localeCompare(b.zone+b.type+b.name+b.face)
}
function byTokenContent(a,b) {
  return (a.zone+a.type).localeCompare(b.zone+b.type)
}

const gameEngineCode = `
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

times(Count, Code)       :- forall(between(1, Count, _), Code).
for(Start, End, Code)    :- forall(between(Start, End, _), Code).
for(Start, End, N, Code) :- forall(between(Start, End, N), Code).

count_all([], []).
count_all([X|Xs], Counts) :-
  count_all(Xs, Counts0),
  \+ member([X,_], Counts0),
  Counts = [[X,1] | Counts0].

count_all([X|Xs], Counts) :-
  count_all(Xs, Counts0),
  member([X,N0], Counts0),
  exclude(member(X), Counts0, Counts1),
  N is N0 + 1,
  Counts = [[X,N] | Counts1].


cmp_default(X, X).

maxes([X|Xs], Rs) :-
  maxes(cmp_default, Xs, [X], Rs).

maxes(Cmp, [X|Xs], Rs) :-
  maxes(Cmp, Xs, [X], Rs).

maxes(_, [], Rs, Rs).
maxes(Cmp, [X|Xs], [CurrentMax|CMs], Rs) :-
  call(Cmp, X, X_cmp),
  call(Cmp, CurrentMax, CM_cmp),
  (
    X_cmp > CM_cmp -> maxes(Xs, [X], Rs);
    X_cmp = CM_cmp -> maxes(Xs, [CurrentMax,X|CMs], Rs);
    maxes(Xs, [CurrentMax|CMs], Rs)
  ).


%
% Tabletime Engine
%
:- dynamic(player/1).
:- dynamic(phase/1).
:- dynamic(player_turn/1).
:- dynamic(config/3).
:- dynamic(config_invalid/3).
:- dynamic(object_id/1).
%% card(Type, Name, CardFace, Zone, Id).
:- dynamic(card/5).
%% token(Type, Zone, Id).
:- dynamic(token/3).
%% token(Type, Player, Zone, Id).
:- dynamic(token/4).

set_phase(S) :- retractall(phase(_)), assertz(phase(S)).

player_count(N) :- count(player(_), N).

% zone_hidden(Player, CardId).
can_peek(_, _) :- false.

try_act(Actor, Action, Args) :-
  action(Action, ReqLen, _),
  length(Args, ReqLen),
  available_action(Actor, Action, Args),
  act(Actor, Action, Args).

% TODO: Make this consistent
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

new_card(Type, Name, CardFace, Zone) :-
  new_object_id(Id),
  assertz(card(Type, Name, CardFace, Zone, Id)).

move_card(Id, NewZone) :-
  retract( card(Type, Name, CardFace, _, Id) ),
  assertz( card(Type, Name, CardFace, NewZone, Id) ).

move_card(Id, NewZone, CardFace) :-
  retract( card(Type, Name, _, _, Id) ),
  assertz( card(Type, Name, CardFace, NewZone, Id) ).

new_token(Type, Zone) :-
  new_object_id(Id),
  assertz(token(Type, Zone, Id)).

new_token(Type, Player, Zone) :-
  new_object_id(Id),
  assertz(token(Type, Player, Zone, Id)).

move_token(Id, NewZone) :-
  retract( token(Type, _, Id) ),
  assertz( token(Type, NewZone, Id) ),
  !.
move_token(Id, NewZone) :-
  retract( token(Type, Player, _, Id) ),
  assertz( token(Type, Player, NewZone, Id) ),
  !.

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

assign_next_turn_player :-
  % Grab the next player that hasn't gone this goaround
  player(Player), \\+ player_turn(Player), !,

  % Mark them as current
  asserta(player_turn(Player)).

% This case occurs when all players have gone
assign_next_turn_player :-
  once(player(_)), % Ensure there is at least one player so we don't infinite loop.
  retractall(player_turn(_)),
  assign_next_turn_player.

current_turn_player(P) :- once(player_turn(P)).
`

function identifierTermToString(term) {
  return Array.isArray(term) ? term.join('/') : term
}
