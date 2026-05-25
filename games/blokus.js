export const blokusPlayers = [
  { id: 'blue', name: 'Blue', color: '#2267b8' },
  { id: 'gold', name: 'Yellow', color: '#d8a422' },
  { id: 'red', name: 'Red', color: '#c73538' },
  { id: 'green', name: 'Green', color: '#2f855a' }
]

export const BLOKUS_BOARD_SIZE = 20
export const BLOKUS_CELL_SIZE = 26
export const BLOKUS_PIECE_CELL_SIZE = 24

const playerLayouts = {
  blue: { x: 36, y: 58 },
  gold: { x: 1188, y: 58 },
  red: { x: 36, y: 710 },
  green: { x: 1188, y: 710 }
}

const PIECE_LAYOUT_UNIT = BLOKUS_PIECE_CELL_SIZE

const pieceLayout = [
  [0, 0, 0],
  [0, 2, 0],
  [2, 0, 3],
  [5, 0, 0],
  [10, 0, 0],
  [15, 0, 0],
  [4, 4, 1],
  [8, 4, 0],
  [12, 4, 0],
  [16, 4, 1],
  [20, 4, 0],
  [0, 8, 0],
  [4, 8, 0],
  [8, 8, 1],
  [12, 8, 0],
  [16, 8, 1],
  [0, 12, 0],
  [4, 12, 0],
  [8, 12, 1],
  [12, 12, 0],
  [16, 12, 0]
]

const pieceDefinitions = [
  { id: 'mono', name: 'Monomino', cells: [[0, 0]] },
  { id: 'domino', name: 'Domino', cells: [[0, 0], [1, 0]] },
  { id: 'tri-i', name: 'I Triomino', cells: [[0, 0], [1, 0], [2, 0]] },
  { id: 'tri-v', name: 'V Triomino', cells: [[0, 0], [0, 1], [1, 1]] },
  { id: 'tet-i', name: 'I Tetromino', cells: [[0, 0], [1, 0], [2, 0], [3, 0]] },
  { id: 'tet-o', name: 'O Tetromino', cells: [[0, 0], [1, 0], [0, 1], [1, 1]] },
  { id: 'tet-t', name: 'T Tetromino', cells: [[0, 0], [1, 0], [2, 0], [1, 1]] },
  { id: 'tet-l', name: 'L Tetromino', cells: [[0, 0], [0, 1], [0, 2], [1, 2]] },
  { id: 'tet-z', name: 'Z Tetromino', cells: [[0, 0], [1, 0], [1, 1], [2, 1]] },
  { id: 'pent-i', name: 'I Pentomino', cells: [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]] },
  { id: 'pent-l', name: 'L Pentomino', cells: [[0, 0], [0, 1], [0, 2], [0, 3], [1, 3]] },
  { id: 'pent-v', name: 'V Pentomino', cells: [[0, 0], [0, 1], [0, 2], [1, 2], [2, 2]] },
  { id: 'pent-t', name: 'T Pentomino', cells: [[0, 0], [1, 0], [2, 0], [1, 1], [1, 2]] },
  { id: 'pent-u', name: 'U Pentomino', cells: [[0, 0], [2, 0], [0, 1], [1, 1], [2, 1]] },
  { id: 'pent-z', name: 'Z Pentomino', cells: [[0, 0], [1, 0], [1, 1], [2, 1], [3, 1]] },
  { id: 'pent-n', name: 'N Pentomino', cells: [[1, 0], [2, 0], [0, 1], [1, 1], [0, 2]] },
  { id: 'pent-y', name: 'Y Pentomino', cells: [[0, 0], [0, 1], [0, 2], [0, 3], [1, 1]] },
  { id: 'pent-p', name: 'P Pentomino', cells: [[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]] },
  { id: 'pent-w', name: 'W Pentomino', cells: [[0, 0], [0, 1], [1, 1], [1, 2], [2, 2]] },
  { id: 'pent-x', name: 'X Pentomino', cells: [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]] },
  { id: 'pent-f', name: 'F Pentomino', cells: [[1, 0], [2, 0], [0, 1], [1, 1], [1, 2]] }
]

export const blokusGame = {
  id: 'blokus',
  name: 'Blokus',
  subtitle: 'Freeform Blokus table',
  minPlayersToStart: 2,
  players: blokusPlayers,
  features: {
    hands: false,
    deck: false,
    discard: false,
    chips: false,
    cards: false,
    remoteHands: false,
    polyominoes: true
  },
  board: {
    size: BLOKUS_BOARD_SIZE,
    cellSize: BLOKUS_CELL_SIZE
  },
  polyominoes: {
    cellSize: BLOKUS_PIECE_CELL_SIZE,
    definitions: pieceDefinitions
  },
  log: {
    maxEntries: 100,
    commandTypes: [
      'game.start',
      'game.change',
      'seat.join',
      'seat.leave',
      'piece.move',
      'piece.rotate',
      'piece.flip',
      'piece.lock',
      'piece.return'
    ]
  },
  objects: initialObjects,
  setup(state) {
    state.pieces = makeInitialPieces()
  }
}

export function initialObjects() {
  return [
    {
      id: 'blokus-board',
      type: 'board',
      boardKind: 'blokus',
      title: 'Blokus Board',
      x: 560,
      y: 105,
      locked: true
    }
  ]
}

function makeInitialPieces() {
  const pieces = []

  for (const player of blokusPlayers) {
    const layout = playerLayouts[player.id]
    for (const [index, definition] of pieceDefinitions.entries()) {
      const [gridX, gridY, rotation = 0] = pieceLayout[index]
      pieces.push({
        id: `piece-${player.id}-${definition.id}`,
        kind: 'polyomino',
        definitionId: definition.id,
        name: definition.name,
        playerId: player.id,
        cells: definition.cells.map(([x, y]) => ({ x, y })),
        x: layout.x + gridX * PIECE_LAYOUT_UNIT,
        y: layout.y + gridY * PIECE_LAYOUT_UNIT,
        rotation,
        flipped: false,
        locked: false,
        inSupply: false,
        source: 'table'
      })
    }
  }

  return pieces
}
