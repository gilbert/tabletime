import s from 'cofound'
import {
  BoardCell,
  BoardGrid,
  BoardWrap,
  CardFace,
  CardRank,
  CardSuit,
  FreeCorner
} from '../../components.js'
import { ZONE } from '../../zones.js'
import { registerObjectComponent } from '../components.js'
import { sequenceSpaces, suits } from './config.js'

const sequenceBoard = s(() =>
  BoardWrap({
    'data-drop-zone': ZONE.BOARD,
    style: `--board-size: 10; --cell-size: 72px; --board-gap: 4px`
  },
    BoardGrid(
      sequenceSpaces.map(space => boardSpace({ key: space.id, space }))
    )
  )
)

const boardSpace = s(({ space }) => {
  return BoardCell({
    'data-free': space.free
  },
    space.free
      ? FreeCorner('FREE')
      : CardFace(
        CardRank({ style: `color: ${suitMeta(space.suit).color}` }, space.rank),
        CardSuit({ style: `color: ${suitMeta(space.suit).color}` }, suitGlyph(space.suit))
      )
  )
})

function suitMeta(suitId) {
  return suits.find(suit => suit.id === suitId) || suits[0]
}

function suitGlyph(suitId) {
  const suit = suitMeta(suitId)
  return suit.symbol || suit.id
}

registerObjectComponent('sequence/board', sequenceBoard)
