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
