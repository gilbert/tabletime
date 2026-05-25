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
import { cloneGameState, createInitialGameState, getGameConfig, normalizeGameState, players, shuffle, suits } from './setup.js'

export const COMMAND = Object.freeze({
  RESET: 'game.reset',
  START: 'game.start',
  GAME_CHANGE: 'game.change',
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
  CARD_LOCK: 'card.lock',
  PIECE_CREATE: 'piece.create',
  PIECE_MOVE: 'piece.move',
  PIECE_ROTATE: 'piece.rotate',
  PIECE_FLIP: 'piece.flip',
  PIECE_LOCK: 'piece.lock',
  PIECE_RETURN: 'piece.return'
})

const COMPONENT_COMMANDS_REQUIRING_SEAT = new Set([
  COMMAND.SHUFFLE_DRAW,
  COMMAND.OBJECT_MOVE,
  COMMAND.OBJECT_LOCK,
  COMMAND.CHIP_CREATE,
  COMMAND.CHIP_MOVE,
  COMMAND.CHIP_LOCK,
  COMMAND.CHIP_RETURN,
  COMMAND.CARD_DISCARD_TO_TABLE,
  COMMAND.CARD_TABLE_TO_HAND,
  COMMAND.CARD_TABLE_TO_DISCARD,
  COMMAND.CARD_TABLE_MOVE,
  COMMAND.CARD_FLIP,
  COMMAND.CARD_ROTATE,
  COMMAND.CARD_LOCK,
  COMMAND.PIECE_CREATE,
  COMMAND.PIECE_MOVE,
  COMMAND.PIECE_ROTATE,
  COMMAND.PIECE_FLIP,
  COMMAND.PIECE_LOCK,
  COMMAND.PIECE_RETURN
])

export function applyCommand(state, command, { random = Math.random, actor = null } = {}) {
  if (!command?.type) return reject('Unknown command.')
  normalizeGameState(state)

  const result = applyCommandMutation(state, command, { random, actor })
  if (result.ok && result.message && shouldLogCommand(state, command.type)) addLog(state, result.message)
  return result
}

function applyCommandMutation(state, command, { random, actor }) {
  if (COMPONENT_COMMANDS_REQUIRING_SEAT.has(command.type)) {
    const blocked = rejectUnseatedComponentActor(state, actor)
    if (blocked) return blocked
  }

  switch (command.type) {
    case COMMAND.RESET:
      replaceState(state, createInitialGameState({ random, gameId: state.gameId }))
      state.log = ['Prototype reset.']
      return accept('Prototype reset.')

    case COMMAND.START:
      return startGame(state)

    case COMMAND.GAME_CHANGE:
      return changeGame(state, command.payload, random)

    case COMMAND.SEAT_JOIN:
      return joinSeat(state, command.payload, actor)

    case COMMAND.SEAT_LEAVE:
      return leaveSeat(state, command.payload, actor)

    case COMMAND.SWITCH_PLAYER:
      return switchPlayer(state, command.payload)

    case COMMAND.DRAW:
      return drawToActorHand(state, command.payload?.count || 1, random, actor)

    case COMMAND.SHUFFLE_DRAW:
      if (!featureEnabled(state, 'deck')) return reject('Draw deck is not available for this game.')
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
      return discardCardToHand(state, command.payload, actor)

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

    case COMMAND.PIECE_CREATE:
      return createPiece(state, command.payload, actor)

    case COMMAND.PIECE_MOVE:
      return movePiece(state, command.payload, actor)

    case COMMAND.PIECE_ROTATE:
      return rotatePiece(state, command.payload, actor)

    case COMMAND.PIECE_FLIP:
      return flipPiece(state, command.payload, actor)

    case COMMAND.PIECE_LOCK:
      return togglePieceLock(state, command.payload, actor)

    case COMMAND.PIECE_RETURN:
      return returnPiece(state, command.payload, actor)

    default:
      return reject(`Unsupported command: ${command.type}`)
  }
}

function rejectUnseatedComponentActor(state, actor) {
  ensureSeats(state)
  if (!actor?.clientId) return reject('Connection identity is required to manipulate components.')
  if (!state.seats.some(seat => seat.clientId === actor.clientId)) {
    return reject('Join a seat before manipulating components.')
  }
  return null
}

export function createCommand(type, payload = {}) {
  return { type, payload }
}

export function applySnapshot(target, snapshot) {
  replaceState(target, cloneGameState(snapshot))
}

function switchPlayer(state, payload = {}) {
  const player = playersForState(state).find(item => item.id === payload.playerId)
  state.activePlayerId = payload.playerId || state.activePlayerId
  return accept(`${player?.name || payload.playerName || 'Player'} is active.`)
}

function changeGame(state, payload = {}, random) {
  const gameConfig = getGameConfig(payload.gameId)
  if (!payload.gameId || gameConfig.id !== payload.gameId) return reject('Game not found.')
  if (state.gameId === gameConfig.id) return reject(`${gameConfig.name} is already selected.`)

  replaceState(state, createInitialGameState({ random, gameId: gameConfig.id }))
  return accept(`Switched to ${gameConfig.name}.`)
}

function startGame(state) {
  ensureSeats(state)
  if (state.started) return reject('Game is already started.')
  const occupiedSeats = state.seats.filter(seat => seat.clientId)
  const occupiedSeatCount = occupiedSeats.length
  const minPlayersToStart = state.minPlayersToStart || 1
  if (occupiedSeatCount < minPlayersToStart) {
    return reject(`At least ${minPlayersToStart} players must be seated to start.`)
  }

  if (featureEnabled(state, 'hands')) {
    ensureHands(state)
    state.handsByPlayerId = Object.fromEntries(occupiedSeats.map(seat => [seat.playerId, []]))
    for (let cardIndex = 0; cardIndex < HAND_SIZE; cardIndex++) {
      for (const seat of occupiedSeats) {
        const card = state.drawPile.pop()
        if (card) state.handsByPlayerId[seat.playerId].push(card)
      }
    }
  } else {
    state.handsByPlayerId = {}
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
  if (!featureEnabled(state, 'hands') || !featureEnabled(state, 'deck')) {
    return reject('Drawing is not available for this game.')
  }
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
  if (!featureEnabled(state, 'hands') || !featureEnabled(state, 'discard')) {
    return reject('Hands are not available for this game.')
  }
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

function discardCardToHand(state, payload = {}, actor = null) {
  ensureSeats(state)
  ensureHands(state)
  if (!featureEnabled(state, 'hands') || !featureEnabled(state, 'discard')) {
    return reject('Hands are not available for this game.')
  }
  const rule = cardDropRule(payload.zoneId || ZONE.HAND, CARD_DRAG_TYPE.DISCARD)
  if (rule?.action !== CARD_DROP_ACTION.MOVE_TO_HAND) return reject('Hand does not accept that card.')
  if (!state.started) return reject('Start the game before taking a card.')
  if (!actor?.clientId) return reject('Connection identity is required to take a card.')

  const seat = state.seats.find(item => item.clientId === actor.clientId)
  if (!seat) return reject('Join a seat before taking a card.')

  const hand = state.handsByPlayerId[seat.playerId]
  if (!Array.isArray(hand)) return reject('Hand not found for this seat.')
  if (hand.length >= HAND_SIZE) return reject('Hand is full.')

  const card = topDiscardCard(state)
  if (!card || card.id !== payload.cardId) return reject('Only the top discard card can be moved.')

  const handCard = { ...card }
  delete handCard.faceUp
  state.discardPile.pop()
  hand.push(handCard)
  return accept(`${seat.clientName || actor.playerName || seat.playerName} took ${cardLabel(card)} from discard.`)
}

function discardCardToTable(state, payload = {}) {
  if (!featureEnabled(state, 'discard') || !featureEnabled(state, 'cards')) {
    return reject('Discard is not available for this game.')
  }
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
  if (!featureEnabled(state, 'discard') || !featureEnabled(state, 'cards')) {
    return reject('Discard is not available for this game.')
  }
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

function createPiece(state, payload = {}, actor = null) {
  if (!featureEnabled(state, 'polyominoes')) return reject('Pieces are not available for this game.')
  if (!payload.pieceId) return reject('Piece id is required.')
  if (pieceById(state, payload.pieceId)) return reject('Piece already exists.')

  const seat = actorSeat(state, actor)
  if (!seat) return reject('Join a seat before manipulating components.')
  if (payload.playerId && payload.playerId !== seat.playerId) return reject('You can only create your own pieces.')

  const position = clampPiecePosition(payload.x, payload.y, payload.width || 1, payload.height || 1)
  state.pieces.push({
    id: payload.pieceId,
    kind: payload.kind || 'polyomino',
    name: payload.name || 'Piece',
    playerId: seat.playerId,
    cells: Array.isArray(payload.cells) ? payload.cells : [{ x: 0, y: 0 }],
    x: position.x,
    y: position.y,
    rotation: normalizedQuarterTurn(payload.rotation),
    flipped: Boolean(payload.flipped),
    locked: false,
    inSupply: false,
    source: payload.source || 'piece-tray'
  })
  return accept(`${seat.playerName} piece placed.`)
}

function movePiece(state, payload = {}, actor = null) {
  if (!featureEnabled(state, 'polyominoes')) return reject('Pieces are not available for this game.')
  const piece = pieceById(state, payload.pieceId)
  if (!piece) return reject('Piece not found.')
  const seat = actorSeat(state, actor)
  const ownership = rejectPieceOwnership(piece, seat)
  if (ownership) return ownership
  if (piece.locked) return reject('Piece is locked.')

  const size = {
    width: Math.max(1, Number(payload.width) || pieceWidth(piece)),
    height: Math.max(1, Number(payload.height) || pieceHeight(piece))
  }
  const position = clampPiecePosition(payload.x, payload.y, size.width, size.height)
  piece.x = position.x
  piece.y = position.y
  piece.inSupply = false
  return accept(`${pieceLabel(state, piece)} placed.`)
}

function rotatePiece(state, payload = {}, actor = null) {
  return updateOwnedPiece(state, payload?.pieceId, actor, piece => {
    piece.rotation = (normalizedQuarterTurn(piece.rotation) + 1) % 4
    return `${pieceLabel(state, piece)} rotated.`
  })
}

function flipPiece(state, payload = {}, actor = null) {
  return updateOwnedPiece(state, payload?.pieceId, actor, piece => {
    piece.flipped = !piece.flipped
    return `${pieceLabel(state, piece)} flipped.`
  })
}

function togglePieceLock(state, payload = {}, actor = null) {
  return updateOwnedPiece(state, payload?.pieceId, actor, piece => {
    piece.locked = !piece.locked
    return `${pieceLabel(state, piece)} ${piece.locked ? 'locked' : 'unlocked'}.`
  }, { allowLocked: true })
}

function returnPiece(state, payload = {}, actor = null) {
  return updateOwnedPiece(state, payload?.pieceId, actor, () => {
    return 'Pieces stay on the table.'
  }, { allowLocked: true })
}

function updateOwnedPiece(state, pieceId, actor, update, { allowLocked = false } = {}) {
  if (!featureEnabled(state, 'polyominoes')) return reject('Pieces are not available for this game.')
  const piece = pieceById(state, pieceId)
  if (!piece) return reject('Piece not found.')
  const seat = actorSeat(state, actor)
  const ownership = rejectPieceOwnership(piece, seat)
  if (ownership) return ownership
  if (piece.locked && !allowLocked) return reject('Piece is locked.')
  return accept(update(piece))
}

function rejectPieceOwnership(piece, seat) {
  if (!seat) return reject('Join a seat before manipulating components.')
  if (piece.playerId !== seat.playerId) return reject('You can only manipulate your own pieces.')
  return null
}

function actorSeat(state, actor) {
  if (!actor?.clientId) return null
  return state.seats?.find(seat => seat.clientId === actor.clientId) || null
}

function pieceLabel(state, piece) {
  const player = playersForState(state).find(item => item.id === piece?.playerId)
  return `${player?.name || 'Player'} ${piece?.name || 'piece'}`
}

function normalizedQuarterTurn(value) {
  const turn = Number(value) || 0
  return ((turn % 4) + 4) % 4
}

function pieceWidth(piece) {
  return pieceBounds(piece).width * pieceCellSize(piece)
}

function pieceHeight(piece) {
  return pieceBounds(piece).height * pieceCellSize(piece)
}

function pieceBounds(piece) {
  const cells = transformedCells(piece)
  const maxX = Math.max(0, ...cells.map(cell => cell.x))
  const maxY = Math.max(0, ...cells.map(cell => cell.y))
  return { width: maxX + 1, height: maxY + 1 }
}

function transformedCells(piece) {
  const rotation = normalizedQuarterTurn(piece.rotation)
  const raw = (piece.cells || []).map(cell => {
    let x = Number(cell.x) || 0
    let y = Number(cell.y) || 0
    if (piece.flipped) x = -x

    for (let i = 0; i < rotation; i++) {
      ;[x, y] = [-y, x]
    }

    return { x, y }
  })

  const minX = Math.min(0, ...raw.map(cell => cell.x))
  const minY = Math.min(0, ...raw.map(cell => cell.y))
  return raw.map(cell => ({ x: cell.x - minX, y: cell.y - minY }))
}

function pieceCellSize(piece) {
  return Number(piece.cellSize) || 24
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

function pieceById(state, id) {
  return state.pieces?.find(piece => piece.id === id)
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
  const gameConfig = getGameConfig(state.gameId)
  state.gameId = gameConfig.id
  if (!state.minPlayersToStart) state.minPlayersToStart = gameConfig.minPlayersToStart || 2
  ensureLogConfig(state)
  if (!Array.isArray(state.seats)) {
    state.seats = gameConfig.players.map(player => ({
      playerId: player.id,
      playerName: player.name,
      color: player.color,
      clientId: null,
      clientName: null
    }))
    return
  }

  for (const player of gameConfig.players) {
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
    const gameConfig = getGameConfig(state.gameId)
    state.logConfig = {
      maxEntries: gameConfig.log?.maxEntries || 100,
      commandTypes: [...(gameConfig.log?.commandTypes || [])]
    }
  }

  state.logConfig.maxEntries = Number(state.logConfig.maxEntries) || 100
  if (!Array.isArray(state.logConfig.commandTypes)) state.logConfig.commandTypes = []
}

function featureEnabled(state, feature) {
  normalizeGameState(state)
  return state.features?.[feature] !== false
}

function playersForState(state) {
  return getGameConfig(state.gameId).players || players
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
