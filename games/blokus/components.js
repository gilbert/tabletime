import s from 'cofound'
import { BoardCell, BoardGrid, BoardWrap } from '../../components.js'
import { registerObjectComponent } from '../components.js'

const blokusBoard = s(({ state }) => {
  const size = state.board?.size || 20
  const cellSize = state.board?.cellSize || 26
  const cornerIndexes = new Set([0, size - 1, size * (size - 1), size * size - 1])

  return BoardWrap({
    style: `--board-size: ${size}; --cell-size: ${cellSize}px; --board-gap: 2px`
  },
    BoardGrid(
      Array.from({ length: size * size }, (_, index) => BoardCell({
        key: `blokus-${index}`,
        'data-board-kind': 'blokus',
        'data-corner': cornerIndexes.has(index) ? 'true' : 'false'
      }))
    )
  )
})

registerObjectComponent('blokus/board', blokusBoard)
