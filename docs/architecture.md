# Tabletime Architecture

Tabletime is a tabletop prototyping surface first, with game-specific rules layered on top. Sequence is the reference game for the first pass because it needs a fixed board, cards in hand, a draw pile, player chips, and turn-oriented actions.

## Core Model

- `table`: viewport-independent table dimensions, zoom, background, and bounds.
- `components`: physical things on the table such as decks, discard piles, boards, tokens, counters, trays, and notes.
- `zones`: logical containers such as a player hand, draw pile, discard pile, supply, or board grid.
- `pieces`: cards, chips, pawns, markers, tiles, dice, and custom game objects.
- `rules`: optional game-specific commands that mutate state, for example `drawCard`, `shuffleDeck`, `scoreSequence`, and `advanceTurn`.
- `presence`: player identity, pointer position, selection, and transient drag state.

Durable game state should be serializable JSON. UI-only state such as hover, drag offsets, panel selection, and local viewport scroll stays client-side.

## Component Boundaries

- `Tabletop`: scrollable, top-down workspace that owns panning/drag affordances and table bounds.
- `TableObject`: positioned item with common lock, select, and drag behavior.
- `BoardGrid`: fixed grid or arbitrary coordinate board. Sequence uses a locked 10x10 card grid, but pieces can still be moved freely on top of it.
- `Card`: reusable card renderer for hand, board cells, piles, and discard views.
- `Deck`: table object with draw, shuffle, and count commands.
- `Hand`: local player zone rendered as a curved hand, independent from the board.
- `PresenceLayer`: realtime cursors and selection indicators.

## Card Interaction Plan

Cards should use the same physical piece model as chips, then add card-specific commands where the table needs them. The first interaction layer should be zone-aware:

- Drag a card from a hand, deck, discard pile, configured zone, or loose table position.
- Drop it only into zones that accept that card's source, type, ownership, and orientation requirements.
- Rubber-band the card back to its origin when dropped on a zone that does not accept it.
- Store loose cards with absolute `x/y`, `rotation`, `faceUp`, `owner`, `source`, `type`, and `locked` fields when a zone permits loose cards.
- Drag existing loose cards without grid snapping by default.
- Support contextual actions and keyboard shortcuts: `f` flips, `r` toggles portrait/landscape orientation, `l` locks, `t` takes a card to hand, and `Backspace` deletes a token by returning it to its source pile.

Zones should be optional containers, not mandatory board game rule engines. They can still have generic rules and settings that streamline play: accepted piece kinds, accepted card sources, accepted deck/type tags, default face-up or face-down state, whether loose placement is allowed, whether dropped pieces should be ordered, and whether a failed drop returns to the source zone. A hand zone owns private ordering and display. A deck zone owns ordered hidden cards plus draw/shuffle commands. A discard zone owns visible ordered cards and can be configured to accept hand cards face-up. The top-level table zone can be configured to reject hand cards so a hand card dropped onto the table or Sequence board rubber-bands back to the hand. A board grid may expose optional drop targets, but snapping and validation should be opt-in per game.

For Sequence, this means the discard zone accepts hand cards face-up, while the table and Sequence board zones reject hand cards. A later rule layer can interpret `card.play` as "move a card from hand to discard" and separately let the player place a chip. That should not be the default card behavior in Tabletime; the base system should preserve generic movement, flipping, rotation, locking, zone acceptance, and source-pile return without encoding Sequence's scoring or card-specific rules.

## Multiplayer Path

The initial prototype can mutate local module-scope state. The networked version should send commands, not DOM events:

```txt
client command -> server validates against rules -> server appends event -> clients receive snapshot/event
```

Suggested event examples:

- `piece.create`
- `piece.move`
- `deck.shuffle`
- `deck.draw`
- `card.play`
- `piece.returnToSource`
- `presence.pointer`

Realtime sync can start with WebSockets and an in-memory room store, then move to persistence once game save/load is needed. Presence events should be ephemeral and throttled; game commands should be ordered and replayable.

## Regression Testing

New user-facing features should include Happy DOM regression coverage when they can be exercised without real browser geometry. Prefer server-backed Happy DOM tests for seat state, snapshots, command flow, privacy, and non-geometric UI behavior. Keep drag/drop geometry, pointer hit testing, and visual animation checks in real-browser tests.

## Sequence Reference Slice

The first implemented slice uses Sequence-like data:

- a 10x10 board with four free corners and two copies of every non-jack card,
- a two-deck draw pile,
- a seven-card local hand,
- red/blue chip supplies that create freely draggable table pieces,
- actions for draw, shuffle, and freeform chip movement.

The Sequence board is reference content, not the default interaction model. Tabletime should behave more like a physical tabletop: components and pieces have positions, locks, ownership, and optional zones; rule-aware snapping or validation should be opt-in behavior layered on top of the freeform piece model.
