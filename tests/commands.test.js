import t from 'cofound/test'

import {
  CARD_LANDSCAPE_HEIGHT,
  CARD_LANDSCAPE_WIDTH,
  CARD_PORTRAIT_HEIGHT,
  CARD_PORTRAIT_WIDTH
} from '../constants.js'
import { applyCommand, COMMAND, createCommand } from '../game/commands.js'
import { createInitialGameState } from '../game/setup.js'

function assert(condition, message) {
  if (!condition) throw new Error(message)
}

function joinSeat(state, playerId, actor) {
  const result = applyCommand(state, createCommand(COMMAND.SEAT_JOIN, { playerId }), { actor })
  assert(result.ok, `Expected ${actor.playerName} to join ${playerId}.`)
}

t`server commands require seated actors for component manipulation`(() => {
  const state = createInitialGameState({ random: () => 0.5 })
  const alice = { clientId: 'alice-client', playerName: 'alice' }
  const spectator = { clientId: 'spectator-client', playerName: 'spectator' }

  joinSeat(state, 'red', alice)
  applyCommand(state, createCommand(COMMAND.START), { actor: alice, random: () => 0.5 })

  for (const command of [
    createCommand(COMMAND.SHUFFLE_DRAW),
    createCommand(COMMAND.OBJECT_MOVE, { objectId: 'deck', x: 10, y: 10 }),
    createCommand(COMMAND.CHIP_CREATE, { chipId: 'chip-test', playerId: 'red', playerName: 'Red', x: 20, y: 20 })
  ]) {
    const result = applyCommand(state, command, { actor: spectator, random: () => 0.5 })
    assert(!result.ok, `${command.type} should reject unseated actors.`)
    assert(result.message === 'Join a seat before manipulating components.', `${command.type} should explain the seat requirement.`)
  }
})

t`card dimensions use the playing-card ratio`(() => {
  assert(CARD_PORTRAIT_WIDTH * 7 === CARD_PORTRAIT_HEIGHT * 5, 'Portrait cards should be 5:7.')
  assert(CARD_LANDSCAPE_WIDTH * 5 === CARD_LANDSCAPE_HEIGHT * 7, 'Landscape cards should be 7:5.')
})

t`blokus setup disables hands and creates polyomino supplies`(() => {
  const state = createInitialGameState({ gameId: 'blokus' })
  const alice = { clientId: 'alice-client', playerName: 'alice' }
  const bob = { clientId: 'bob-client', playerName: 'bob' }

  assert(state.gameId === 'blokus', 'Blokus state should keep the selected game id.')
  assert(state.features.hands === false, 'Blokus should disable hands.')
  assert(state.drawPile.length === 0, 'Blokus should not create a draw pile.')
  assert(state.discardPile.length === 0, 'Blokus should not create a discard pile.')
  assert(state.objects.length === 1 && state.objects[0].boardKind === 'blokus', 'Blokus should create a Blokus board.')
  assert(state.pieces.length === 84, 'Blokus should create 21 polyominoes for each of 4 players.')
  assert(state.pieces.every(piece => piece.inSupply === false && Number.isFinite(piece.x) && Number.isFinite(piece.y)), 'Blokus pieces should start on the table.')
  assert(state.pieces.some(piece => piece.rotation > 0), 'Blokus layout should support initial piece rotation.')
  assert(state.pieces.some(piece => piece.flipped), 'Blokus layout should support initial piece flipping.')

  joinSeat(state, 'blue', alice)
  joinSeat(state, 'gold', bob)
  const start = applyCommand(state, createCommand(COMMAND.START), { actor: alice })
  assert(start.ok, 'Blokus should start with two seated players.')
  assert(Object.keys(state.handsByPlayerId).length === 0, 'Starting Blokus should not deal hands.')
})

t`blokus pieces are owned by their seated player`(() => {
  const state = createInitialGameState({ gameId: 'blokus' })
  const alice = { clientId: 'alice-client', playerName: 'alice' }
  const bob = { clientId: 'bob-client', playerName: 'bob' }
  const spectator = { clientId: 'spectator-client', playerName: 'spectator' }
  const bluePiece = state.pieces.find(piece => piece.playerId === 'blue')

  joinSeat(state, 'blue', alice)
  joinSeat(state, 'gold', bob)

  const spectatorMove = applyCommand(state, createCommand(COMMAND.PIECE_MOVE, {
    pieceId: bluePiece.id,
    x: 100,
    y: 100,
    width: 24,
    height: 24
  }), { actor: spectator })
  assert(!spectatorMove.ok, 'Spectators should not move Blokus pieces.')

  const otherPlayerMove = applyCommand(state, createCommand(COMMAND.PIECE_MOVE, {
    pieceId: bluePiece.id,
    x: 100,
    y: 100,
    width: 24,
    height: 24
  }), { actor: bob })
  assert(!otherPlayerMove.ok, 'Players should not move another player color.')

  const ownerMove = applyCommand(state, createCommand(COMMAND.PIECE_MOVE, {
    pieceId: bluePiece.id,
    x: 100,
    y: 100,
    width: 24,
    height: 24
  }), { actor: alice })
  assert(ownerMove.ok, 'Owners should move their own pieces.')
  assert(bluePiece.x === 100 && bluePiece.y === 100, 'Moved pieces should update table position.')
})

t`game change resets room state to the selected game`(() => {
  const state = createInitialGameState({ random: () => 0.5 })
  const alice = { clientId: 'alice-client', playerName: 'alice' }

  joinSeat(state, 'red', alice)
  const result = applyCommand(state, createCommand(COMMAND.GAME_CHANGE, { gameId: 'blokus' }), {
    actor: alice,
    random: () => 0.5
  })

  assert(result.ok, 'Game change should be accepted.')
  assert(state.gameId === 'blokus', 'Game change should update room state.')
  assert(state.features.hands === false, 'Changed Blokus rooms should not have hands.')
  assert(state.pieces.length === 84, 'Changed Blokus rooms should create Blokus pieces.')
  assert(state.seats.every(seat => !seat.clientId), 'Game change should reset seats.')
})
