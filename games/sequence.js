import { BOARD_SIZE } from '../constants.js'

export const players = [
  { id: 'red', name: 'Red', color: '#c73538' },
  { id: 'blue', name: 'Blue', color: '#2267b8' },
  { id: 'green', name: 'Green', color: '#2f855a' },
  { id: 'gold', name: 'Gold', color: '#b7791f' }
]

export const suits = [
  { id: 'S', name: 'Spades', symbol: '♠', color: '#1f2937' },
  { id: 'H', name: 'Hearts', symbol: '♥', color: '#c73538' },
  { id: 'D', name: 'Diamonds', symbol: '♦', color: '#c73538' },
  { id: 'C', name: 'Clubs', symbol: '♣', color: '#1f2937' }
]

export const ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']

const cornerIndexes = new Set([0, BOARD_SIZE - 1, BOARD_SIZE * (BOARD_SIZE - 1), BOARD_SIZE * BOARD_SIZE - 1])

export const sequenceSpaces = buildSequenceSpaces()

export const sequenceGame = {
  id: 'sequence',
  name: 'Sequence',
  subtitle: 'Sequence reference prototype',
  minPlayersToStart: 2,
  players,
  features: {
    hands: true,
    deck: true,
    discard: true,
    chips: true,
    cards: true,
    remoteHands: true,
    polyominoes: false
  },
  log: {
    maxEntries: 100,
    commandTypes: [
      'game.start',
      'game.change',
      'seat.join',
      'seat.leave',
      'deck.draw',
      'deck.shuffle',
      'card.handToDiscard',
      'card.discardToHand',
      'card.discardToTable',
      'card.tableToDiscard',
      'card.flip',
      'card.rotate',
      'card.lock',
      'chip.lock',
      'chip.return',
      'object.lock'
    ]
  },
  objects: initialObjects,
  setup(state, { makeDeck, random, shuffle }) {
    state.drawPile = shuffle(makeDeck(state, 2), random)
  }
}

export function initialObjects() {
  return [
    {
      id: 'sequence-board',
      type: 'board',
      boardKind: 'sequence',
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
