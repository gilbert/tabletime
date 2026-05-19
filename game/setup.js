import { BOARD_SIZE, HAND_SIZE } from '../constants.js'

export const players = [
  { id: 'red', name: 'Red', color: '#c73538' },
  { id: 'blue', name: 'Blue', color: '#2267b8' },
  { id: 'green', name: 'Green', color: '#2f855a' },
  { id: 'gold', name: 'Gold', color: '#b7791f' }
]

export const gameConfigs = Object.freeze({
  sequence: {
    id: 'sequence',
    name: 'Sequence',
    minPlayersToStart: 2,
    log: {
      maxEntries: 100,
      commandTypes: [
        'game.start',
        'seat.join',
        'seat.leave',
        'deck.draw',
        'deck.shuffle',
        'card.handToDiscard',
        'card.discardToTable',
        'card.tableToDiscard',
        'card.flip',
        'card.rotate',
        'card.lock',
        'chip.lock',
        'chip.return',
        'object.lock'
      ]
    }
  },
  blokus: {
    id: 'blokus',
    name: 'Blokus',
    minPlayersToStart: 2,
    log: {
      maxEntries: 100,
      commandTypes: [
        'game.start',
        'seat.join',
        'seat.leave'
      ]
    }
  }
})

export const suits = [
  { id: 'S', name: 'Spades', symbol: '♠', color: '#1f2937' },
  { id: 'H', name: 'Hearts', symbol: '♥', color: '#c73538' },
  { id: 'D', name: 'Diamonds', symbol: '♦', color: '#c73538' },
  { id: 'C', name: 'Clubs', symbol: '♣', color: '#1f2937' }
]

export const ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']

const cornerIndexes = new Set([0, BOARD_SIZE - 1, BOARD_SIZE * (BOARD_SIZE - 1), BOARD_SIZE * BOARD_SIZE - 1])

export const sequenceSpaces = buildSequenceSpaces()

export function createInitialGameState({ random = Math.random, gameId = 'sequence' } = {}) {
  const gameConfig = gameConfigs[gameId] || gameConfigs.sequence
  const state = {
    nextId: 1,
    gameId: gameConfig.id,
    minPlayersToStart: gameConfig.minPlayersToStart,
    started: false,
    activePlayerId: players[0].id,
    seats: players.map(player => ({
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
    drawPile: [],
    discardPile: [],
    handsByPlayerId: {},
    chips: [],
    tableCards: [],
    objects: initialObjects(),
    logConfig: cloneLogConfig(gameConfig.log),
    log: ['Prototype loaded with a Sequence reference setup.']
  }

  state.drawPile = shuffle(makeDeck(state, 2), random)

  return state
}

export function initialObjects() {
  return [
    {
      id: 'sequence-board',
      type: 'board',
      title: 'Sequence Board',
      x: 410,
      y: 150,
      locked: true
    },
    {
      id: 'draw-deck',
      type: 'deck',
      title: 'Draw Deck',
      x: 118,
      y: 220,
      locked: false
    },
    {
      id: 'discard-tray',
      type: 'discard',
      title: 'Discard',
      x: 1275,
      y: 235,
      locked: false
    },
    {
      id: 'chip-supply',
      type: 'supply',
      title: 'Chip Supply',
      x: 125,
      y: 560,
      locked: false
    }
  ]
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

function cloneLogConfig(logConfig) {
  return {
    maxEntries: logConfig?.maxEntries || 100,
    commandTypes: [...(logConfig?.commandTypes || [])]
  }
}

function buildSequenceSpaces() {
  const nonJacks = []

  for (let copy = 0; copy < 2; copy++) {
    for (const suit of suits) {
      for (const rank of ranks) {
        if (rank !== 'J') nonJacks.push({ rank, suit: suit.id, code: `${rank}${suit.id}`, copy })
      }
    }
  }

  let cardIndex = 0

  return Array.from({ length: BOARD_SIZE * BOARD_SIZE }, (_, index) => {
    const row = Math.floor(index / BOARD_SIZE)
    const col = index % BOARD_SIZE

    if (cornerIndexes.has(index)) {
      return { id: `space-${row}-${col}`, row, col, free: true, code: 'FREE' }
    }

    return {
      id: `space-${row}-${col}`,
      row,
      col,
      free: false,
      ...nonJacks[cardIndex++]
    }
  })
}
