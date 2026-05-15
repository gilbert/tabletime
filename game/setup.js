import { BOARD_SIZE, HAND_SIZE } from '../constants.js'

export const players = [
  { id: 'red', name: 'Red', color: '#c73538' },
  { id: 'blue', name: 'Blue', color: '#2267b8' }
]

export const suits = [
  { id: 'S', name: 'Spades', color: '#1f2937' },
  { id: 'H', name: 'Hearts', color: '#c73538' },
  { id: 'D', name: 'Diamonds', color: '#c73538' },
  { id: 'C', name: 'Clubs', color: '#1f2937' }
]

export const ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']

const cornerIndexes = new Set([0, BOARD_SIZE - 1, BOARD_SIZE * (BOARD_SIZE - 1), BOARD_SIZE * BOARD_SIZE - 1])

export const sequenceSpaces = buildSequenceSpaces()

export function createInitialGameState({ random = Math.random } = {}) {
  const state = {
    nextId: 1,
    activePlayerId: players[0].id,
    selectedHandCardId: null,
    selectedTableCardId: null,
    selectedDiscardCardId: null,
    selectedChipId: null,
    drawPile: [],
    discardPile: [],
    hand: [],
    chips: [],
    tableCards: [],
    objects: initialObjects(),
    log: ['Prototype loaded with a Sequence reference setup.']
  }

  state.drawPile = shuffle(makeDeck(state, 2), random)

  for (let i = 0; i < HAND_SIZE; i++) {
    const card = state.drawPile.pop()
    if (card) state.hand.push(card)
  }

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
  return JSON.parse(JSON.stringify(state))
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
