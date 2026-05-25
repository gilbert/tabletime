import { HAND_SIZE } from '../constants.js'
import { gameConfigs, players, ranks, sequenceSpaces, suits } from '../games/index.js'

export { gameConfigs, players, sequenceSpaces, suits }

export function getGameConfig(gameId = 'sequence') {
  return gameConfigs[gameId] || gameConfigs.sequence
}

export function createInitialGameState({ random = Math.random, gameId = 'sequence' } = {}) {
  const gameConfig = getGameConfig(gameId)
  const state = {
    nextId: 1,
    gameId: gameConfig.id,
    gameName: gameConfig.name,
    gameSubtitle: gameConfig.subtitle,
    features: clonePlain(gameConfig.features || {}),
    board: clonePlain(gameConfig.board || null),
    polyominoes: clonePlain(gameConfig.polyominoes || null),
    minPlayersToStart: gameConfig.minPlayersToStart,
    started: false,
    activePlayerId: gameConfig.players[0].id,
    seats: gameConfig.players.map(player => ({
      playerId: player.id,
      playerName: player.name,
      color: player.color,
      clientId: null,
      clientName: null
    })),
    selectedHandCardId: null,
    selectedTableCardId: null,
    selectedDiscardCardId: null,
    selectedChipId: null,
    selectedPieceId: null,
    drawPile: [],
    discardPile: [],
    handsByPlayerId: {},
    chips: [],
    tableCards: [],
    pieces: [],
    objects: gameConfig.objects ? gameConfig.objects() : [],
    logConfig: cloneLogConfig(gameConfig.log),
    log: [`${gameConfig.name} loaded.`]
  }

  gameConfig.setup?.(state, { makeDeck, random, shuffle })

  return state
}

export function makeDeck(state, copies = 1) {
  const cards = []

  for (let copy = 0; copy < copies; copy++) {
    for (const suit of suits) {
      for (const rank of ranks) {
        cards.push(makeCard(state, rank, suit.id, copy))
      }
    }
  }

  return cards
}

export function makeCard(state, rank, suit, copy = 0) {
  return {
    id: allocateEntityId(state, 'card'),
    code: `${rank}${suit}`,
    rank,
    suit,
    copy
  }
}

export function allocateEntityId(state, prefix) {
  const id = `${prefix}-${state.nextId || 1}`
  state.nextId = (state.nextId || 1) + 1
  return id
}

export function shuffle(cards, random = Math.random) {
  const next = cards.slice()

  for (let i = next.length - 1; i > 0; i--) {
    const j = Math.floor(random() * (i + 1))
    ;[next[i], next[j]] = [next[j], next[i]]
  }

  return next
}

export function cloneGameState(state) {
  return normalizeGameState(JSON.parse(JSON.stringify(state)))
}

export function normalizeGameState(state) {
  if (!state || typeof state !== 'object' || Array.isArray(state)) return state
  const gameConfig = getGameConfig(state.gameId)
  state.gameId = gameConfig.id
  state.gameName = state.gameName || gameConfig.name
  state.gameSubtitle = state.gameSubtitle || gameConfig.subtitle
  state.features = state.features && typeof state.features === 'object'
    ? { ...(gameConfig.features || {}), ...state.features }
    : clonePlain(gameConfig.features || {})
  state.board = state.board || clonePlain(gameConfig.board || null)
  state.polyominoes = state.polyominoes || clonePlain(gameConfig.polyominoes || null)
  if (!Array.isArray(state.pieces)) state.pieces = []

  if (!state.handsByPlayerId || typeof state.handsByPlayerId !== 'object' || Array.isArray(state.handsByPlayerId)) {
    state.handsByPlayerId = {}
  }

  if (Array.isArray(state.hand) && state.hand.length) {
    state.drawPile = [...(state.drawPile || []), ...state.hand]
  }
  delete state.hand

  for (const [playerId, hand] of Object.entries(state.handsByPlayerId)) {
    if (Array.isArray(hand)) {
      state.handsByPlayerId[playerId] = hand.slice(0, HAND_SIZE)
      continue
    }

    if (hand && typeof hand === 'object') {
      state.handsByPlayerId[playerId] = {
        playerId: hand.playerId || playerId,
        playerName: hand.playerName || null,
        clientName: hand.clientName || null,
        color: hand.color || null,
        count: Math.max(0, Number(hand.count) || 0),
        hidden: hand.hidden !== false
      }
      continue
    }

    delete state.handsByPlayerId[playerId]
  }

  return state
}

function clonePlain(value) {
  return value == null ? value : JSON.parse(JSON.stringify(value))
}

function cloneLogConfig(logConfig) {
  return {
    maxEntries: logConfig?.maxEntries || 100,
    commandTypes: [...(logConfig?.commandTypes || [])]
  }
}
