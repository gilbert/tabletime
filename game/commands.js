import {
  CARD_LANDSCAPE_HEIGHT,
  CARD_LANDSCAPE_WIDTH,
  CARD_PORTRAIT_HEIGHT,
  CARD_PORTRAIT_WIDTH,
  CHIP_SIZE,
  HAND_SIZE,
  TABLE_HEIGHT,
  TABLE_OBJECT_INSET,
  TABLE_PIECE_INSET,
  TABLE_WIDTH
} from '../constants.js'
import { cardDropRule, CARD_DRAG_TYPE, CARD_DROP_ACTION, ZONE } from '../zones.js'
import { cloneGameState, createInitialGameState, normalizeGameState, players, shuffle, suits } from './setup.js'

export const COMMAND = Object.freeze({
  RESET: 'game.reset',
  START: 'game.start',
  SEAT_JOIN: 'seat.join',
  SEAT_LEAVE: 'seat.leave',
  SWITCH_PLAYER: 'player.switch',
  DRAW: 'deck.draw',
  SHUFFLE_DRAW: 'deck.shuffle',
  OBJECT_MOVE: 'object.move',
  OBJECT_LOCK: 'object.lock',
  CHIP_CREATE: 'chip.create',
  CHIP_MOVE: 'chip.move',
  CHIP_LOCK: 'chip.lock',
  CHIP_RETURN: 'chip.return',
  CARD_HAND_TO_DISCARD: 'card.handToDiscard',
  CARD_DISCARD_TO_HAND: 'card.discardToHand',
  CARD_DISCARD_TO_TABLE: 'card.discardToTable',
  CARD_TABLE_TO_HAND: 'card.tableToHand',
  CARD_TABLE_TO_DISCARD: 'card.tableToDiscard',
  CARD_TABLE_MOVE: 'card.tableMove',
  CARD_FLIP: 'card.flip',
  CARD_ROTATE: 'card.rotate',
  CARD_LOCK: 'card.lock'
})

export function applyCommand(state, command, { random = Math.random, actor = null } = {}) {
  if (!command?.type) return reject('Unknown command.')
  normalizeGameState(state)

  const result = applyCommandMutation(state, command, { random, actor })
  if (result.ok && result.message && shouldLogCommand(state, command.type)) addLog(state, result.message)
  return result
}

function applyCommandMutation(state, command, { random, actor }) {
  switch (command.type) {
    case COMMAND.RESET:
      replaceState(state, createInitialGameState({ random, gameId: state.gameId }))
      state.log = ['Prototype reset.']
      return accept('Prototype reset.')

    case COMMAND.START:
      return startGame(state)

    case COMMAND.SEAT_JOIN:
      return joinSeat(state, command.payload, actor)

    case COMMAND.SEAT_LEAVE:
      return leaveSeat(state, command.payload, actor)

    case COMMAND.SWITCH_PLAYER:
      return switchPlayer(state, command.payload)

    case COMMAND.DRAW:
      return drawToActorHand(state, command.payload?.count || 1, random, actor)

    case COMMAND.SHUFFLE_DRAW:
      state.drawPile = shuffle(state.drawPile, random)
      return accept('Draw deck shuffled.')

    case COMMAND.OBJECT_MOVE:
      return moveObject(state, command.payload)

    case COMMAND.OBJECT_LOCK:
      return toggleObjectLock(state, command.payload)

    case COMMAND.CHIP_CREATE:
      return createChip(state, command.payload)

    case COMMAND.CHIP_MOVE:
      return moveChip(state, command.payload)

    case COMMAND.CHIP_LOCK:
      return toggleChipLock(state, command.payload)

    case COMMAND.CHIP_RETURN:
      return returnChip(state, command.payload)

    case COMMAND.CARD_HAND_TO_DISCARD:
      return handCardToDiscard(state, command.payload, actor)

    case COMMAND.CARD_DISCARD_TO_HAND:
      return discardCardToHand(state, command.payload)

    case COMMAND.CARD_DISCARD_TO_TABLE:
      return discardCardToTable(state, command.payload)

    case COMMAND.CARD_TABLE_TO_HAND:
      return tableCardToHand(state, command.payload)

    case COMMAND.CARD_TABLE_TO_DISCARD:
      return tableCardToDiscard(state, command.payload)

    case COMMAND.CARD_TABLE_MOVE:
      return moveTableCard(state, command.payload)

    case COMMAND.CARD_FLIP:
      return updateTableCard(state, command.payload?.pieceId, piece => {
        piece.faceUp = !piece.faceUp
        return `${cardLabel(piece.card)} flipped ${piece.faceUp ? 'face-up' : 'face-down'}.`
      })

    case COMMAND.CARD_ROTATE:
      return updateTableCard(state, command.payload?.pieceId, piece => {
        piece.orientation = piece.orientation === 'landscape' ? 'portrait' : 'landscape'
        return `${cardLabel(piece.card)} rotated ${piece.orientation}.`
      })

    case COMMAND.CARD_LOCK:
      return updateTableCard(state, command.payload?.pieceId, piece => {
        piece.locked = !piece.locked
        return `${cardLabel(piece.card)} ${piece.locked ? 'locked' : 'unlocked'}.`
      })

    default:
      return reject(`Unsupported command: ${command.type}`)
  }
}

export function createCommand(type, payload = {}) {
  return { type, payload }
}

export function applySnapshot(target, snapshot) {
  replaceState(target, cloneGameState(snapshot))
}

function switchPlayer(state, payload = {}) {
  const player = players.find(item => item.id === payload.playerId)
  state.activePlayerId = payload.playerId || state.activePlayerId
  return accept(`${player?.name || payload.playerName || 'Player'} is active.`)
}

function startGame(state) {
  ensureSeats(state)
  ensureHands(state)
  if (state.started) return reject('Game is already started.')
  const occupiedSeats = state.seats.filter(seat => seat.clientId)
  const occupiedSeatCount = occupiedSeats.length
  const minPlayersToStart = state.minPlayersToStart || 1
  if (occupiedSeatCount < minPlayersToStart) {
    return reject(`At least ${minPlayersToStart} players must be seated to start.`)
  }

  state.handsByPlayerId = Object.fromEntries(occupiedSeats.map(seat => [seat.playerId, []]))
  for (let cardIndex = 0; cardIndex < HAND_SIZE; cardIndex++) {
    for (const seat of occupiedSeats) {
      const card = state.drawPile.pop()
      if (card) state.handsByPlayerId[seat.playerId].push(card)
    }
  }

  state.activePlayerId = occupiedSeats[0]?.playerId || state.activePlayerId
  state.started = true
  return accept('Game started.')
}

function joinSeat(state, payload = {}, actor = null) {
  ensureSeats(state)

  if (state.started) return reject('Seats are locked after start.')
  if (!actor?.clientId) return reject('Connection identity is required to join a seat.')

  const seat = state.seats.find(item => item.playerId === payload.playerId)
  if (!seat) return reject('Seat not found.')

  if (seat.clientId && seat.clientId !== actor.clientId) {
    return reject(`${seat.playerName} seat is already occupied.`)
  }

  for (const item of state.seats) {
    if (item.clientId === actor.clientId && item.playerId !== seat.playerId) {
      item.clientId = null
      item.clientName = null
    }
  }

  const clientName = actor.playerName || 'Player'
  const alreadySeated = seat.clientId === actor.clientId
  seat.clientId = actor.clientId
  seat.clientName = clientName

  return accept(alreadySeated
    ? `${clientName} is already seated as ${seat.playerName}.`
    : `${clientName} joined ${seat.playerName}.`)
}

function leaveSeat(state, payload = {}, actor = null) {
  ensureSeats(state)

  if (state.started) return reject('Seats are locked after start.')
  if (!actor?.clientId) return reject('Connection identity is required to leave a seat.')

  const seat = state.seats.find(item => item.playerId === payload.playerId)
  if (!seat) return reject('Seat not found.')
  if (seat.clientId !== actor.clientId) return reject('You are not seated there.')

  const clientName = seat.clientName || actor.playerName || 'Player'
  seat.clientId = null
  seat.clientName = null

  return accept(`${clientName} left ${seat.playerName}.`)
}

function drawToActorHand(state, count, random, actor) {
  ensureSeats(state)
  ensureHands(state)
  if (!state.started) return reject('Start the game before drawing.')
  if (!actor?.clientId) return reject('Connection identity is required to draw.')

  const seat = state.seats.find(item => item.clientId === actor.clientId)
  if (!seat) return reject('Join a seat before drawing.')

  const hand = state.handsByPlayerId[seat.playerId]
  if (!Array.isArray(hand)) return reject('Hand not found for this seat.')

  let drawn = 0
  const limit = Math.max(1, Number(count) || 1)

  for (let i = 0; i < limit; i++) {
    if (hand.length >= HAND_SIZE) {
      return drawn
        ? accept(`${seat.clientName || actor.playerName || seat.playerName} drew ${drawn} card${drawn === 1 ? '' : 's'}.`)
        : reject('Hand is full.')
    }

    if (!state.drawPile.length) recycleDiscard(state, random)
    if (!state.drawPile.length) {
      return drawn
        ? accept(`${seat.clientName || actor.playerName || seat.playerName} drew ${drawn} card${drawn === 1 ? '' : 's'}.`)
        : reject('Draw deck is empty.')
    }

    const card = state.drawPile.pop()
    if (card) {
      hand.push(card)
      drawn++
    }
  }

  return accept(`${seat.clientName || actor.playerName || seat.playerName} drew ${drawn} card${drawn === 1 ? '' : 's'}.`)
}

function recycleDiscard(state, random) {
  if (!state.discardPile.length) return
  state.drawPile = shuffle(state.discardPile, random)
  state.discardPile = []
  addLog(state, 'Discard was shuffled back into the draw deck.')
}

function moveObject(state, payload = {}) {
  const object = objectById(state, payload.objectId)
  if (!object) return reject('Object not found.')
  if (object.locked) return reject(`${object.title} is locked.`)

  object.x = clamp(payload.x, TABLE_OBJECT_INSET, TABLE_WIDTH - TABLE_OBJECT_INSET)
  object.y = clamp(payload.y, TABLE_OBJECT_INSET, TABLE_HEIGHT - TABLE_OBJECT_INSET)
  return accept(`${object.title} moved.`)
}

function toggleObjectLock(state, payload = {}) {
  const object = objectById(state, payload.objectId)
  if (!object) return reject('Object not found.')

  object.locked = !object.locked
  return accept(`${object.title} ${object.locked ? 'locked' : 'unlocked'}.`)
}

function createChip(state, payload = {}) {
  if (!payload.playerId) return reject('Chip player is required.')
  if (state.chips.some(chip => chip.id === payload.chipId)) return reject('Chip already exists.')

  const position = clampPiecePosition(payload.x, payload.y, CHIP_SIZE, CHIP_SIZE)
  state.chips.push({
    id: payload.chipId,
    playerId: payload.playerId,
    x: position.x,
    y: position.y,
    locked: false
  })
  return accept(`${payload.playerName || 'Player'} chip created.`)
}

function moveChip(state, payload = {}) {
  const chip = chipById(state, payload.chipId)
  if (!chip) return reject('Chip not found.')
  if (chip.locked) return reject('Chip is locked.')

  const position = clampPiecePosition(payload.x, payload.y, payload.width || CHIP_SIZE, payload.height || CHIP_SIZE)
  chip.x = position.x
  chip.y = position.y
  return accept('Chip placed.')
}

function toggleChipLock(state, payload = {}) {
  const chip = chipById(state, payload.chipId)
  if (!chip) return reject('Chip not found.')

  chip.locked = !chip.locked
  return accept(`Chip ${chip.locked ? 'locked' : 'unlocked'}.`)
}

function returnChip(state, payload = {}) {
  const chip = chipById(state, payload.chipId)
  if (!chip) return reject('Chip not found.')

  state.chips = state.chips.filter(item => item.id !== chip.id)
  return accept('Chip returned to supply.')
}

function handCardToDiscard(state, payload = {}, actor = null) {
  ensureSeats(state)
  ensureHands(state)
  const rule = cardDropRule(payload.zoneId || ZONE.DISCARD, CARD_DRAG_TYPE.HAND)
  if (rule?.action !== CARD_DROP_ACTION.MOVE_TO_DISCARD) return reject('Discard does not accept that card.')
  if (!state.started) return reject('Start the game before playing a card.')
  if (!actor?.clientId) return reject('Connection identity is required to play a card.')

  const seat = state.seats.find(item => item.clientId === actor.clientId)
  if (!seat) return reject('Join a seat before playing a card.')

  const hand = state.handsByPlayerId[seat.playerId]
  if (!Array.isArray(hand)) return reject('Hand not found for this seat.')

  const card = hand.find(item => item.id === payload.cardId)
  if (!card) return reject('Card not found in your hand.')

  state.handsByPlayerId[seat.playerId] = hand.filter(item => item.id !== card.id)
  state.discardPile.push({ ...card, faceUp: rule.cardFaceUpOnDrop ?? true })
  return accept(`${seat.clientName || actor.playerName || seat.playerName} moved ${cardLabel(card)} to discard.`)
}

function cardLabel(card) {
  const suit = suits.find(item => item.id === card?.suit)
  return `${card?.rank || ''}${suit?.symbol || card?.suit || ''}`
}

function discardCardToHand(state, payload = {}) {
  return reject('Discard cards cannot be moved into hands.')
}

function discardCardToTable(state, payload = {}) {
  const rule = cardDropRule(payload.zoneId || ZONE.TABLE, CARD_DRAG_TYPE.DISCARD)
  if (rule?.action !== CARD_DROP_ACTION.PLACE_ON_TABLE) return reject('Table does not accept that card.')

  const card = topDiscardCard(state)
  if (!card || card.id !== payload.cardId) return reject('Only the top discard card can be moved.')
  if (!payload.pieceId) return reject('Table card id is required.')
  if (state.tableCards.some(piece => piece.id === payload.pieceId)) return reject('Table card already exists.')

  state.discardPile.pop()
  state.tableCards.push(makeTableCardPiece(card, payload))
  return accept(`${cardLabel(card)} moved from discard to table.`)
}

function tableCardToHand(state, payload = {}) {
  return reject('Table cards cannot be moved into hands.')
}

function tableCardToDiscard(state, payload = {}) {
  const rule = cardDropRule(payload.zoneId || ZONE.DISCARD, CARD_DRAG_TYPE.TABLE)
  if (rule?.action !== CARD_DROP_ACTION.MOVE_TO_DISCARD) return reject('Discard does not accept that card.')

  const piece = tableCardById(state, payload.pieceId)
  if (!piece) return reject('Table card not found.')
  if (piece.locked) return reject('Card is locked.')

  state.tableCards = state.tableCards.filter(item => item.id !== piece.id)
  state.discardPile.push({ ...piece.card, faceUp: rule.cardFaceUpOnDrop ?? true })
  return accept(`${cardLabel(piece.card)} moved from table to discard.`)
}

function moveTableCard(state, payload = {}) {
  const piece = tableCardById(state, payload.pieceId)
  if (!piece) return reject('Table card not found.')
  if (piece.locked) return reject('Card is locked.')

  const size = cardPieceSize(piece)
  const position = clampPiecePosition(payload.x, payload.y, payload.width || size.width, payload.height || size.height)
  piece.x = position.x
  piece.y = position.y
  return accept(`${cardLabel(piece.card)} moved.`)
}

function updateTableCard(state, pieceId, update) {
  const piece = tableCardById(state, pieceId)
  if (!piece) return reject('Table card not found.')

  return accept(update(piece))
}

function makeTableCardPiece(card, payload) {
  const orientation = payload.orientation || 'portrait'
  const size = cardPieceSize({ orientation })
  const position = clampPiecePosition(payload.x, payload.y, payload.width || size.width, payload.height || size.height)

  return {
    id: payload.pieceId,
    card,
    x: position.x,
    y: position.y,
    rotation: payload.rotation || 0,
    orientation,
    faceUp: payload.faceUp ?? true,
    owner: payload.owner || null,
    source: payload.source || 'discard-tray',
    type: payload.cardType || 'standard-card',
    locked: false
  }
}

function cardPieceSize(piece) {
  return piece.orientation === 'landscape'
    ? { width: CARD_LANDSCAPE_WIDTH, height: CARD_LANDSCAPE_HEIGHT }
    : { width: CARD_PORTRAIT_WIDTH, height: CARD_PORTRAIT_HEIGHT }
}

function objectById(state, id) {
  return state.objects.find(object => object.id === id)
}

function chipById(state, id) {
  return state.chips.find(chip => chip.id === id)
}

function tableCardById(state, id) {
  return state.tableCards.find(piece => piece.id === id)
}

function topDiscardCard(state) {
  return state.discardPile[state.discardPile.length - 1]
}

function addLog(state, message) {
  ensureLogConfig(state)
  state.log = [...(state.log || []), message].slice(-state.logConfig.maxEntries)
}

function shouldLogCommand(state, commandType) {
  if (commandType === COMMAND.RESET) return false
  ensureLogConfig(state)
  return state.logConfig.commandTypes.includes(commandType)
}

function accept(message) {
  return { ok: true, message }
}

function reject(message) {
  return { ok: false, message }
}

function replaceState(target, source) {
  for (const key of Object.keys(target)) delete target[key]
  Object.assign(target, source)
}

function ensureSeats(state) {
  if (typeof state.started !== 'boolean') state.started = false
  if (!state.gameId) state.gameId = 'sequence'
  if (!state.minPlayersToStart) state.minPlayersToStart = 2
  ensureLogConfig(state)
  if (!Array.isArray(state.seats)) {
    state.seats = players.map(player => ({
      playerId: player.id,
      playerName: player.name,
      color: player.color,
      clientId: null,
      clientName: null
    }))
    return
  }

  for (const player of players) {
    if (state.seats.some(seat => seat.playerId === player.id)) continue
    state.seats.push({
      playerId: player.id,
      playerName: player.name,
      color: player.color,
      clientId: null,
      clientName: null
    })
  }

  ensureHands(state)
}

function ensureHands(state) {
  normalizeGameState(state)
}

function ensureLogConfig(state) {
  if (!state.logConfig) {
    state.logConfig = {
      maxEntries: 100,
      commandTypes: [
        COMMAND.START,
        COMMAND.SEAT_JOIN,
        COMMAND.SEAT_LEAVE,
        COMMAND.DRAW,
        COMMAND.SHUFFLE_DRAW,
        COMMAND.CARD_HAND_TO_DISCARD,
        COMMAND.CARD_DISCARD_TO_TABLE,
        COMMAND.CARD_TABLE_TO_DISCARD,
        COMMAND.CARD_FLIP,
        COMMAND.CARD_ROTATE,
        COMMAND.CARD_LOCK,
        COMMAND.CHIP_LOCK,
        COMMAND.CHIP_RETURN,
        COMMAND.OBJECT_LOCK
      ]
    }
  }

  state.logConfig.maxEntries = Number(state.logConfig.maxEntries) || 100
  if (!Array.isArray(state.logConfig.commandTypes)) state.logConfig.commandTypes = []
}

function clampPiecePosition(x, y, width, height) {
  return {
    x: clamp(x, TABLE_PIECE_INSET, TABLE_WIDTH - width - TABLE_PIECE_INSET),
    y: clamp(y, TABLE_PIECE_INSET, TABLE_HEIGHT - height - TABLE_PIECE_INSET)
  }
}

function clamp(value, min, max) {
  return Math.min(Math.max(Number(value) || 0, min), max)
}
