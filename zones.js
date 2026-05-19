export const ZONE = Object.freeze({
  TABLE: 'table',
  BOARD: 'board',
  DISCARD: 'discard',
  HAND: 'hand'
})

export const CARD_DRAG_TYPE = Object.freeze({
  HAND: 'hand-card',
  DISCARD: 'discard-card',
  TABLE: 'table-card'
})

export const CARD_DROP_ACTION = Object.freeze({
  MOVE_TO_DISCARD: 'move-to-discard',
  MOVE_TO_HAND: 'move-to-hand',
  PLACE_ON_TABLE: 'place-on-table'
})

export const ZONES = Object.freeze({
  [ZONE.TABLE]: {
    id: ZONE.TABLE,
    acceptsCards: {
      [CARD_DRAG_TYPE.DISCARD]: { action: CARD_DROP_ACTION.PLACE_ON_TABLE },
      [CARD_DRAG_TYPE.TABLE]: { action: CARD_DROP_ACTION.PLACE_ON_TABLE }
    }
  },
  [ZONE.BOARD]: {
    id: ZONE.BOARD,
    acceptsCards: {
      [CARD_DRAG_TYPE.TABLE]: { action: CARD_DROP_ACTION.PLACE_ON_TABLE }
    }
  },
  [ZONE.DISCARD]: {
    id: ZONE.DISCARD,
    acceptsCards: {
      [CARD_DRAG_TYPE.HAND]: { action: CARD_DROP_ACTION.MOVE_TO_DISCARD, cardFaceUpOnDrop: true },
      [CARD_DRAG_TYPE.TABLE]: { action: CARD_DROP_ACTION.MOVE_TO_DISCARD, cardFaceUpOnDrop: true }
    }
  },
  [ZONE.HAND]: {
    id: ZONE.HAND,
    acceptsCards: {
      [CARD_DRAG_TYPE.DISCARD]: { action: CARD_DROP_ACTION.MOVE_TO_HAND }
    }
  }
})

export function cardDropRule(zoneId, dragType) {
  return ZONES[zoneId]?.acceptsCards?.[dragType] || null
}

export function zoneAcceptsCardDrop(zoneId, dragType) {
  return Boolean(cardDropRule(zoneId, dragType))
}
