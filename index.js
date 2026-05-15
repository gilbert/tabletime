import s from 'cofound'
import {
  BOARD_SIZE,
  CARD_LANDSCAPE_HEIGHT,
  CARD_LANDSCAPE_WIDTH,
  CARD_PORTRAIT_HEIGHT,
  CARD_PORTRAIT_WIDTH,
  CHIP_SIZE,
  HAND_SIZE,
  SNAP_BACK_DURATION_MS,
  SNAP_BACK_TRANSITION,
  TABLE_HEIGHT,
  TABLE_OBJECT_INSET,
  TABLE_PIECE_INSET,
  TABLE_WIDTH
} from './constants.js'
import {
  ActionRail,
  AppShell,
  BoardCell,
  BoardGrid,
  BoardWrap,
  BottomText,
  Brand,
  CardBack,
  CardFace,
  CardRank,
  CardSuit,
  CenterRank,
  CornerText,
  DeckBody,
  DeckStack,
  DiscardBody,
  FreeCorner,
  HandBar,
  HandCardButton,
  HandCardInner,
  HandCards,
  HandStatus,
  LockBadge,
  LogItem,
  LogList,
  Mark,
  Metric,
  MetricGrid,
  MetricLabel,
  MetricValue,
  MiniButton,
  ObjectActions,
  ObjectHeader,
  PanelSection,
  PanelTitle,
  Pill,
  SecondaryButton,
  SidePanel,
  StackCount,
  StatusStrip,
  Subtitle,
  SupplyBody,
  SupplyChip,
  SupplyRow,
  TableCardButton,
  TableChip,
  TableObjectShell,
  TableSurface,
  Title,
  TitleBlock,
  ToolbarButton,
  TopBar,
  Workspace,
  installGlobalStyles
} from './components.js'
import { CARD_DRAG_TYPE, CARD_DROP_ACTION, ZONE, cardDropRule, zoneAcceptsCardDrop } from './zones.js'

installGlobalStyles()

const players = [
  { id: 'red', name: 'Red', color: '#c73538' },
  { id: 'blue', name: 'Blue', color: '#2267b8' }
]

const suits = [
  { id: 'S', name: 'Spades', color: '#1f2937' },
  { id: 'H', name: 'Hearts', color: '#c73538' },
  { id: 'D', name: 'Diamonds', color: '#c73538' },
  { id: 'C', name: 'Clubs', color: '#1f2937' }
]

const ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']
const cornerIndexes = new Set([0, BOARD_SIZE - 1, BOARD_SIZE * (BOARD_SIZE - 1), BOARD_SIZE * BOARD_SIZE - 1])

let nextId = 1
let dragState = null
let dragGhost = null
let drawAnimationActive = false
let pointerListenersAttached = false
let redrawQueued = false

const state = {
  activePlayerId: players[0].id,
  selectedHandCardId: null,
  selectedTableCardId: null,
  selectedDiscardCardId: null,
  selectedChipId: null,
  drawPile: shuffle(makeDeck(2)),
  discardPile: [],
  hand: [],
  chips: [],
  tableCards: [],
  objects: [
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
  ],
  log: ['Prototype loaded with a Sequence reference setup.']
}

const sequenceSpaces = buildSequenceSpaces()
drawToHand(HAND_SIZE, false)

function makeDeck(copies = 1) {
  const cards = []

  for (let copy = 0; copy < copies; copy++) {
    for (const suit of suits) {
      for (const rank of ranks) {
        cards.push(makeCard(rank, suit.id, copy))
      }
    }
  }

  return cards
}

function makeCard(rank, suit, copy = 0) {
  return {
    id: `card-${nextId++}`,
    code: `${rank}${suit}`,
    rank,
    suit,
    copy
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

function shuffle(cards) {
  const next = cards.slice()

  for (let i = next.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1))
    ;[next[i], next[j]] = [next[j], next[i]]
  }

  return next
}

function activePlayer() {
  return players.find(player => player.id === state.activePlayerId) || players[0]
}

function suitMeta(suitId) {
  return suits.find(suit => suit.id === suitId) || suits[0]
}

function objectById(id) {
  return state.objects.find(object => object.id === id)
}

function selectedHandCard() {
  return state.hand.find(card => card.id === state.selectedHandCardId)
}

function chipById(id) {
  return state.chips.find(chip => chip.id === id)
}

function tableCardById(id) {
  return state.tableCards.find(piece => piece.id === id)
}

function selectedTableCard() {
  return tableCardById(state.selectedTableCardId)
}

function selectedDiscardCard() {
  return state.discardPile.find(card => card.id === state.selectedDiscardCardId)
}

function playerById(id) {
  return players.find(player => player.id === id) || players[0]
}

function drawToHand(count = 1, shouldLog = true) {
  for (let i = 0; i < count; i++) {
    if (!state.drawPile.length) recycleDiscard()
    const card = state.drawPile.pop()
    if (card) state.hand.push(card)
  }

  if (shouldLog) addLog(`Drew ${count} card${count === 1 ? '' : 's'}.`)
}

function drawOne() {
  if (drawAnimationActive) return

  if (state.hand.length >= HAND_SIZE) {
    addLog('Hand is full.')
    return
  }

  if (!state.drawPile.length) recycleDiscard()
  if (!state.drawPile.length) {
    addLog('Draw deck is empty.')
    return
  }

  if (s.is.server) {
    drawToHand(1)
    return
  }

  drawAnimationActive = true
  animateDrawToHand().finally(() => {
    drawAnimationActive = false
    drawToHand(1)
    scheduleRedraw()
  })
}

function recycleDiscard() {
  if (!state.discardPile.length) return
  state.drawPile = shuffle(state.discardPile)
  state.discardPile = []
  addLog('Discard was shuffled back into the draw deck.')
}

function shuffleDrawPile() {
  state.drawPile = shuffle(state.drawPile)
  addLog('Draw deck shuffled.')
}

function selectHandCard(cardId) {
  state.selectedHandCardId = state.selectedHandCardId === cardId ? null : cardId
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
}

function switchPlayer(playerId) {
  state.activePlayerId = playerId
  addLog(`${activePlayer().name} is active.`)
}

function selectTableCard(pieceId) {
  state.selectedTableCardId = state.selectedTableCardId === pieceId ? null : pieceId
  state.selectedHandCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
}

function selectDiscardCard(cardId) {
  state.selectedDiscardCardId = state.selectedDiscardCardId === cardId ? null : cardId
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedChipId = null
}

function selectChip(chipId) {
  state.selectedChipId = state.selectedChipId === chipId ? null : chipId
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
}

function resetPrototype() {
  state.activePlayerId = players[0].id
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
  state.drawPile = shuffle(makeDeck(2))
  state.discardPile = []
  state.hand = []
  state.chips = []
  state.tableCards = []
  state.log = ['Prototype reset.']
  drawToHand(HAND_SIZE, false)
}

function addLog(message) {
  state.log = [message, ...state.log].slice(0, 5)
}

function scheduleRedraw() {
  if (redrawQueued) return

  redrawQueued = true
  requestAnimationFrame(() => {
    redrawQueued = false
    s.redraw()
  })
}

function attachPointerListeners() {
  if (pointerListenersAttached) return

  pointerListenersAttached = true

  window.addEventListener('pointermove', event => {
    if (!dragState) return
    if (dragState.returning) return

    const surface = document.querySelector('[data-table-surface="true"]')
    if (!surface) return

    const rect = surface.getBoundingClientRect()
    const movement = Math.hypot(event.clientX - dragState.startX, event.clientY - dragState.startY)
    if (movement > 4) dragState.moved = true

    if (dragState.type === 'object') {
      const object = objectById(dragState.objectId)
      if (!object) return

      object.x = clamp(
        event.clientX - rect.left - dragState.offsetX,
        TABLE_OBJECT_INSET,
        TABLE_WIDTH - dragState.width - TABLE_OBJECT_INSET
      )
      object.y = clamp(
        event.clientY - rect.top - dragState.offsetY,
        TABLE_OBJECT_INSET,
        TABLE_HEIGHT - dragState.height - TABLE_OBJECT_INSET
      )
    }

    if (dragState.type === 'chip') {
      const chip = chipById(dragState.chipId)
      if (!chip) return

      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      chip.x = event.clientX - rect.left - dragState.offsetX
      chip.y = event.clientY - rect.top - dragState.offsetY
    }

    if (dragState.type === CARD_DRAG_TYPE.TABLE) {
      const piece = tableCardById(dragState.pieceId)
      if (!piece || piece.locked) return

      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      piece.x = event.clientX - rect.left - dragState.offsetX
      piece.y = event.clientY - rect.top - dragState.offsetY
    }

    if (dragState.type === CARD_DRAG_TYPE.HAND || dragState.type === CARD_DRAG_TYPE.DISCARD) {
      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      moveDragGhost(event.clientX, event.clientY)
    }

    scheduleRedraw()
  })

  window.addEventListener('pointerup', event => {
    if (!dragState) return
    const currentDrag = dragState

    if (currentDrag.type === 'object') {
      if (currentDrag.moved) addLog(`${objectById(currentDrag.objectId)?.title || 'Object'} moved.`)
    }

    if (currentDrag.type === 'chip') {
      if (currentDrag.moved) finishChipDrop(event, currentDrag)
    }

    if (currentDrag.type === CARD_DRAG_TYPE.TABLE) {
      if (!currentDrag.moved) selectTableCard(currentDrag.pieceId)
      else finishTableCardDrop(event, currentDrag)
    }

    if (currentDrag.type === CARD_DRAG_TYPE.HAND) {
      if (!currentDrag.moved) {
        selectHandCard(currentDrag.cardId)
      } else {
        finishHandCardDrop(event, currentDrag)
      }
    }

    if (currentDrag.type === CARD_DRAG_TYPE.DISCARD) {
      if (!currentDrag.moved) {
        selectDiscardCard(currentDrag.cardId)
      } else {
        finishDiscardCardDrop(event, currentDrag)
      }
    }

    if (dragState === currentDrag && !currentDrag.returning) {
      finishDrag()
    }
  })

  window.addEventListener('keydown', handleKeyDown)
  window.addEventListener('resize', scheduleRedraw)
}

function startObjectDrag(event, object) {
  if (object.locked || event.target.closest('[data-no-drag="true"]')) return

  event.preventDefault()
  const shell = event.currentTarget.closest('[data-table-object="true"]')
  if (!shell) return

  const rect = shell.getBoundingClientRect()
  dragState = {
    type: 'object',
    objectId: object.id,
    startX: event.clientX,
    startY: event.clientY,
    moved: false,
    offsetX: event.clientX - rect.left,
    offsetY: event.clientY - rect.top,
    width: rect.width,
    height: rect.height
  }
}

function startChipDrag(event, chip) {
  event.preventDefault()
  event.stopPropagation()
  selectChip(chip.id)

  if (chip.locked) {
    scheduleRedraw()
    return
  }

  const rect = event.currentTarget.getBoundingClientRect()
  dragState = {
    type: 'chip',
    chipId: chip.id,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: event.clientX - rect.left,
    offsetY: event.clientY - rect.top,
    width: rect.width,
    height: rect.height
  }
}

function createChipFromSupply(event, playerId) {
  event.preventDefault()
  event.stopPropagation()

  const point = tablePointFromEvent(event)
  if (!point) return

  const chip = {
    id: `chip-${nextId++}`,
    playerId,
    x: clamp(point.x - CHIP_SIZE / 2, TABLE_PIECE_INSET, TABLE_WIDTH - CHIP_SIZE - TABLE_PIECE_INSET),
    y: clamp(point.y - CHIP_SIZE / 2, TABLE_PIECE_INSET, TABLE_HEIGHT - CHIP_SIZE - TABLE_PIECE_INSET)
  }

  state.chips.push(chip)
  dragState = {
    type: 'chip',
    chipId: chip.id,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: CHIP_SIZE / 2,
    offsetY: CHIP_SIZE / 2,
    width: CHIP_SIZE,
    height: CHIP_SIZE
  }

  scheduleRedraw()
}

function startHandCardDrag(event, card) {
  event.preventDefault()
  event.stopPropagation()

  const rect = event.currentTarget.getBoundingClientRect()
  const originRect = rectFromDomRect(rect)
  dragState = {
    type: CARD_DRAG_TYPE.HAND,
    cardId: card.id,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: event.clientX - rect.left,
    offsetY: event.clientY - rect.top,
    width: rect.width,
    height: rect.height,
    rotation: 0,
    originRect
  }
  createDragGhost(event.currentTarget, dragState)
}

function startDiscardCardDrag(event, card) {
  event.preventDefault()
  event.stopPropagation()

  const rect = event.currentTarget.getBoundingClientRect()
  const originRect = rectFromDomRect(rect)
  dragState = {
    type: CARD_DRAG_TYPE.DISCARD,
    cardId: card.id,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: event.clientX - rect.left,
    offsetY: event.clientY - rect.top,
    width: rect.width,
    height: rect.height,
    rotation: 0,
    originRect
  }
  createDragGhost(event.currentTarget, dragState)
}

function startTableCardDrag(event, piece) {
  event.preventDefault()
  event.stopPropagation()
  selectTableCard(piece.id)

  if (piece.locked) {
    scheduleRedraw()
    return
  }

  const rect = event.currentTarget.getBoundingClientRect()
  dragState = {
    type: CARD_DRAG_TYPE.TABLE,
    pieceId: piece.id,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: event.clientX - rect.left,
    offsetY: event.clientY - rect.top,
    width: rect.width,
    height: rect.height
  }
}

function finishHandCardDrop(event, drag) {
  const zoneId = dropZoneFromPoint(event)
  const card = state.hand.find(item => item.id === drag.cardId)
  if (!card) return

  const rule = cardDropRule(zoneId, drag.type)

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_DISCARD) {
    state.hand = state.hand.filter(item => item.id !== card.id)
    state.discardPile.push({ ...card, faceUp: rule.cardFaceUpOnDrop ?? true })
    state.selectedHandCardId = null
    state.selectedDiscardCardId = card.id
    removeDragGhost()
    addLog(`${card.code} moved to discard.`)
    return
  }

  addLog(`${card.code} returned to hand.`)
  returnDragGhostToOrigin(drag)
}

function finishDiscardCardDrop(event, drag) {
  const zoneId = dropZoneFromPoint(event)
  const card = state.discardPile[state.discardPile.length - 1]
  if (!card || card.id !== drag.cardId) return

  const rule = cardDropRule(zoneId, drag.type)

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_HAND) {
    if (state.hand.length >= HAND_SIZE) {
      addLog('Hand is full.')
      returnDragGhostToOrigin(drag)
      return
    }

    state.discardPile.pop()
    state.hand.push(card)
    state.selectedDiscardCardId = null
    state.selectedHandCardId = card.id
    removeDragGhost()
    addLog(`${card.code} moved from discard to hand.`)
    return
  }

  if (rule?.action === CARD_DROP_ACTION.PLACE_ON_TABLE) {
    const point = tablePointFromEvent(event)
    if (!point) return

    state.discardPile.pop()
    state.tableCards.push({
      id: `piece-${nextId++}`,
      card,
      ...clampPiecePosition(point.x - drag.offsetX, point.y - drag.offsetY, drag.width, drag.height),
      rotation: 0,
      orientation: 'portrait',
      faceUp: true,
      owner: null,
      source: 'discard-tray',
      type: 'standard-card',
      locked: false
    })
    state.selectedDiscardCardId = null
    state.selectedTableCardId = state.tableCards[state.tableCards.length - 1].id
    addLog(`${card.code} moved from discard to table.`)
    return
  }

  addLog(`${card.code} returned to discard.`)
  returnDragGhostToOrigin(drag)
}

function finishChipDrop(event, drag) {
  const chip = chipById(drag.chipId)
  if (!chip) return

  const point = tablePointFromEvent(event)
  if (!point) return

  const raw = {
    x: point.x - drag.offsetX,
    y: point.y - drag.offsetY
  }
  const clamped = clampPiecePosition(raw.x, raw.y, drag.width, drag.height)
  chip.x = clamped.x
  chip.y = clamped.y
  addLog(`${playerById(chip.playerId).name} chip placed.`)

  if (positionWasClamped(raw, clamped)) {
    animateDraggedPieceToTablePosition(drag, clamped)
  }
}

function finishTableCardDrop(event, drag) {
  const piece = tableCardById(drag.pieceId)
  if (!piece) return

  const zoneId = dropZoneFromPoint(event, piece.id)

  const rule = cardDropRule(zoneId, drag.type)

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_HAND) {
    if (state.hand.length >= HAND_SIZE) {
      addLog('Hand is full.')
      returnDragGhostToOrigin(drag)
      return
    }

    state.tableCards = state.tableCards.filter(item => item.id !== piece.id)
    state.hand.push(piece.card)
    state.selectedTableCardId = null
    state.selectedHandCardId = piece.card.id
    removeDragGhost()
    addLog(`${piece.card.code} moved from table to hand.`)
    return
  }

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_DISCARD) {
    state.tableCards = state.tableCards.filter(item => item.id !== piece.id)
    state.discardPile.push({ ...piece.card, faceUp: rule.cardFaceUpOnDrop ?? true })
    state.selectedTableCardId = null
    state.selectedDiscardCardId = piece.card.id
    removeDragGhost()
    addLog(`${piece.card.code} moved from table to discard.`)
    return
  }

  const point = tablePointFromEvent(event)
  if (!point) return

  const raw = {
    x: point.x - drag.offsetX,
    y: point.y - drag.offsetY
  }
  const clamped = clampPiecePosition(raw.x, raw.y, drag.width, drag.height)
  piece.x = clamped.x
  piece.y = clamped.y
  addLog(`${piece.card.code} moved.`)

  if (positionWasClamped(raw, clamped)) {
    animateDraggedPieceToTablePosition(drag, clamped)
  }
}

function dropZoneFromPoint(event, skipPieceId = null) {
  const elements = document.elementsFromPoint(event.clientX, event.clientY)

  for (const element of elements) {
    if (skipPieceId && element.closest?.(`[data-piece-id="${skipPieceId}"]`)) continue
    const zone = element.closest?.('[data-drop-zone]')
    if (zone) return zone.dataset.dropZone
  }

  return null
}

function createDragGhost(source, drag) {
  removeDragGhost()

  const clone = source.cloneNode(true)
  const computed = getComputedStyle(source)
  const layoutRect = ghostLayoutRect(source, drag.originRect)

  clone.removeAttribute('id')
  clone.setAttribute('aria-hidden', 'true')
  clone.style.position = 'fixed'
  clone.style.left = layoutRect.left + 'px'
  clone.style.top = layoutRect.top + 'px'
  clone.style.right = 'auto'
  clone.style.bottom = 'auto'
  clone.style.width = layoutRect.width + 'px'
  clone.style.height = layoutRect.height + 'px'
  clone.style.margin = '0'
  clone.style.zIndex = '60'
  clone.style.pointerEvents = 'none'
  clone.style.opacity = '.96'
  clone.style.transition = 'none'
  clone.style.transform = computed.transform
  clone.style.transformOrigin = computed.transformOrigin
  clone.style.boxShadow = '0 24px 44px rgba(0,0,0,.42)'

  drag.originRect = layoutRect
  drag.offsetX = drag.startX - layoutRect.left
  drag.offsetY = drag.startY - layoutRect.top
  drag.width = layoutRect.width
  drag.height = layoutRect.height

  document.body.appendChild(clone)
  dragGhost = clone
}

function moveDragGhost(clientX, clientY) {
  if (!dragGhost || !dragState) return

  dragGhost.style.left = clientX - dragState.offsetX + 'px'
  dragGhost.style.top = clientY - dragState.offsetY + 'px'
}

function returnDragGhostToOrigin(drag) {
  if (!dragGhost) {
    finishDrag()
    return
  }

  drag.returning = true
  const current = dragGhost.getBoundingClientRect()

  dragGhost.animate([
    { left: current.left + 'px', top: current.top + 'px', opacity: .96 },
    { left: drag.originRect.left + 'px', top: drag.originRect.top + 'px', opacity: .9 }
  ], {
    duration: 190,
    easing: 'cubic-bezier(.2,.8,.2,1)',
    fill: 'forwards'
  }).finished.finally(() => {
    if (dragState === drag) finishDrag()
  })

  scheduleRedraw()
}

function animateDraggedPieceToTablePosition(drag, position) {
  drag.returning = true
  const target = tableViewportRect(position.x, position.y, drag.width, drag.height)
  drag.snapLeft = target.left
  drag.snapTop = target.top

  scheduleRedraw()

  window.setTimeout(() => {
    if (dragState === drag) finishDrag()
  }, SNAP_BACK_DURATION_MS)
}

function removeDragGhost() {
  dragGhost?.remove()
  dragGhost = null
}

function finishDrag() {
  removeDragGhost()
  dragState = null
  scheduleRedraw()
}

async function animateDrawToHand() {
  const source = document.querySelector('[data-draw-stack="true"]')
  const targetRect = nextHandCardRect()

  if (!source || !targetRect) return

  const sourceRect = source.getBoundingClientRect()
  const ghost = document.createElement('div')

  Object.assign(ghost.style, {
    position: 'fixed',
    left: sourceRect.left + sourceRect.width / 2 - targetRect.width / 2 + 'px',
    top: sourceRect.top + sourceRect.height / 2 - targetRect.height / 2 + 'px',
    width: targetRect.width + 'px',
    height: targetRect.height + 'px',
    zIndex: '55',
    pointerEvents: 'none',
    border: '1px solid rgba(255,255,255,.22)',
    borderRadius: '8px',
    background: '#29313a',
    boxShadow: '0 24px 44px rgba(0,0,0,.42)',
    opacity: '.98'
  })

  ghost.textContent = 'TABLETIME'
  ghost.style.display = 'grid'
  ghost.style.placeItems = 'center'
  ghost.style.color = '#f5f1e8'
  ghost.style.fontSize = '13px'
  ghost.style.fontWeight = '850'

  document.body.appendChild(ghost)

  await ghost.animate([
    {
      left: ghost.style.left,
      top: ghost.style.top,
      opacity: .92,
      transform: 'scale(.92)'
    },
    {
      left: targetRect.left + 'px',
      top: targetRect.top + 'px',
      opacity: 1,
      transform: 'scale(1)'
    }
  ], {
    duration: 260,
    easing: 'cubic-bezier(.2,.8,.2,1)',
    fill: 'forwards'
  }).finished.finally(() => {
    ghost.remove()
  })
}

function rectFromDomRect(rect) {
  return {
    left: rect.left,
    top: rect.top,
    width: rect.width,
    height: rect.height
  }
}

function nextHandCardRect() {
  const hand = document.querySelector('[data-hand-cards="true"]')
  if (!hand) return null

  const metrics = handMetrics()
  const total = state.hand.length + 1
  const index = state.hand.length
  const spacing = total <= 1 ? 0 : metrics.spacing
  const totalWidth = (total - 1) * spacing + metrics.width
  const progress = total <= 1 ? 0.5 : index / (total - 1)
  const lift = total <= 1 ? 0 : Math.sin(progress * Math.PI) * metrics.arcLift
  const handRect = hand.getBoundingClientRect()

  return {
    left: handRect.left + handRect.width / 2 - totalWidth / 2 + index * spacing,
    top: handRect.bottom - metrics.height - lift,
    width: metrics.width,
    height: metrics.height
  }
}

function tableViewportRect(x, y, width, height) {
  const surface = document.querySelector('[data-table-surface="true"]')
  const rect = surface?.getBoundingClientRect()

  return {
    left: (rect?.left || 0) + x,
    top: (rect?.top || 0) + y,
    width,
    height
  }
}

function chipLeft(chip) {
  if (dragState?.type !== 'chip' || dragState.chipId !== chip.id) return chip.x
  if (dragState.returning) return dragState.snapLeft
  return dragState.clientX - dragState.offsetX
}

function chipTop(chip) {
  if (dragState?.type !== 'chip' || dragState.chipId !== chip.id) return chip.y
  if (dragState.returning) return dragState.snapTop
  return dragState.clientY - dragState.offsetY
}

function tableCardLeft(piece) {
  if (dragState?.type !== CARD_DRAG_TYPE.TABLE || dragState.pieceId !== piece.id) return piece.x
  if (dragState.returning) return dragState.snapLeft
  return dragState.clientX - dragState.offsetX
}

function tableCardTop(piece) {
  if (dragState?.type !== CARD_DRAG_TYPE.TABLE || dragState.pieceId !== piece.id) return piece.y
  if (dragState.returning) return dragState.snapTop
  return dragState.clientY - dragState.offsetY
}

function clampPiecePosition(x, y, width, height) {
  return {
    x: clamp(x, TABLE_PIECE_INSET, TABLE_WIDTH - width - TABLE_PIECE_INSET),
    y: clamp(y, TABLE_PIECE_INSET, TABLE_HEIGHT - height - TABLE_PIECE_INSET)
  }
}

function positionWasClamped(raw, clamped) {
  return raw.x !== clamped.x || raw.y !== clamped.y
}

function ghostLayoutRect(source, visibleRect) {
  const width = source.offsetWidth || visibleRect.width
  const height = source.offsetHeight || visibleRect.height

  return {
    left: visibleRect.left + (visibleRect.width - width) / 2,
    top: visibleRect.top + (visibleRect.height - height) / 2,
    width,
    height
  }
}

function handleKeyDown(event) {
  if (isTypingTarget(event.target)) return

  const key = event.key.toLowerCase()
  const piece = selectedTableCard()

  if (key === 'f' && piece) {
    piece.faceUp = !piece.faceUp
    addLog(`${piece.card.code} flipped ${piece.faceUp ? 'face-up' : 'face-down'}.`)
    scheduleRedraw()
  }

  if (key === 'r' && piece) {
    piece.orientation = piece.orientation === 'landscape' ? 'portrait' : 'landscape'
    addLog(`${piece.card.code} rotated ${piece.orientation}.`)
    scheduleRedraw()
  }

  if (key === 'l') {
    if (piece) {
      piece.locked = !piece.locked
      addLog(`${piece.card.code} ${piece.locked ? 'locked' : 'unlocked'}.`)
      scheduleRedraw()
    } else if (state.selectedChipId) {
      const chip = chipById(state.selectedChipId)
      if (!chip) return
      chip.locked = !chip.locked
      addLog(`${playerById(chip.playerId).name} chip ${chip.locked ? 'locked' : 'unlocked'}.`)
      scheduleRedraw()
    }
  }

  if (key === 't') {
    if (piece) {
      takeTableCardToHand(piece)
      scheduleRedraw()
    } else if (selectedDiscardCard()) {
      takeDiscardCardToHand()
      scheduleRedraw()
    }
  }

  if (event.key === 'Backspace' && state.selectedChipId) {
    const chip = chipById(state.selectedChipId)
    if (!chip) return
    event.preventDefault()
    state.chips = state.chips.filter(item => item.id !== chip.id)
    state.selectedChipId = null
    addLog(`${playerById(chip.playerId).name} chip returned to supply.`)
    scheduleRedraw()
  }
}

function takeTableCardToHand(piece) {
  if (state.hand.length >= HAND_SIZE) {
    addLog('Hand is full.')
    return
  }

  state.tableCards = state.tableCards.filter(item => item.id !== piece.id)
  state.hand.push(piece.card)
  state.selectedTableCardId = null
  state.selectedHandCardId = piece.card.id
  addLog(`${piece.card.code} taken to hand.`)
}

function takeDiscardCardToHand() {
  const card = selectedDiscardCard()
  const top = state.discardPile[state.discardPile.length - 1]
  if (!card || !top || card.id !== top.id) return

  if (state.hand.length >= HAND_SIZE) {
    addLog('Hand is full.')
    return
  }

  state.discardPile.pop()
  state.hand.push(card)
  state.selectedDiscardCardId = null
  state.selectedHandCardId = card.id
  addLog(`${card.code} taken to hand.`)
}

function isTypingTarget(target) {
  return ['INPUT', 'TEXTAREA', 'SELECT'].includes(target?.tagName) || target?.isContentEditable
}

function tablePointFromEvent(event) {
  const surface = document.querySelector('[data-table-surface="true"]')
  if (!surface) return null

  const rect = surface.getBoundingClientRect()
  return {
    x: event.clientX - rect.left,
    y: event.clientY - rect.top
  }
}

function clamp(value, min, max) {
  return Math.min(Math.max(value, min), max)
}

const App = s(({}, [], { doc }) => {
  doc.title('Tabletime')
  doc.lang('en')
  doc.head([
    s`meta`({ name: 'viewport', content: 'width=device-width, initial-scale=1' })
  ])

  return AppShell(
    topBar(),
    Workspace(
      TableSurface({
        'data-table-surface': 'true',
        'data-drop-zone': ZONE.TABLE,
        style: `--table-width: ${TABLE_WIDTH}px; --table-height: ${TABLE_HEIGHT}px`,
        dom: attachPointerListeners
      },
        state.objects.map(object => tableObject({ key: object.id, object })),
        state.tableCards.map(piece => tableCard({ key: piece.id, piece })),
        state.chips.map(chip => tableChip({ key: chip.id, chip })),
        sidePanel()
      )
    ),
    handBar()
  )
})

const topBar = s(() =>
  TopBar(
    Brand(
      Mark(),
      TitleBlock(
        Title('Tabletime'),
        Subtitle('Sequence reference prototype')
      )
    ),
    StatusStrip(
      Pill('Freeform tabletop'),
      Pill(`${state.drawPile.length} deck`),
      Pill(`${state.chips.length} chips`),
      Pill(`${state.tableCards.length} loose cards`),
      SecondaryButton({ onclick: resetPrototype }, 'Reset')
    )
  )
)

const tableObject = s(({ object }) =>
  TableObjectShell`
    left ${object.x + 'px'}
    top ${object.y + 'px'}
  `({
    'data-table-object': 'true'
  },
    ObjectHeader`
      cursor ${object.locked ? 'default' : dragState?.type === 'object' && dragState.objectId === object.id ? 'grabbing' : 'grab'}
    `({
      onpointerdown: event => startObjectDrag(event, object)
    },
      object.title,
      LockBadge(object.locked ? 'locked' : 'movable')
    ),
    object.type === 'board' && sequenceBoard(),
    object.type === 'deck' && deckObject(),
    object.type === 'discard' && discardObject(),
    object.type === 'supply' && supplyObject()
  )
)

const sequenceBoard = s(() =>
  BoardWrap({
    'data-drop-zone': ZONE.BOARD
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
        CardSuit({ style: `color: ${suitMeta(space.suit).color}` }, space.suit)
      )
  )
})

const tableChip = s(({ chip }) =>
  TableChip`
    position ${dragState?.type === 'chip' && dragState.chipId === chip.id ? 'fixed' : 'absolute'}
    left ${chipLeft(chip) + 'px'}
    top ${chipTop(chip) + 'px'}
    z-index ${dragState?.type === 'chip' && dragState.chipId === chip.id ? 60 : 5}
    transition ${dragState?.type === 'chip' && dragState.chipId === chip.id && dragState.returning ? SNAP_BACK_TRANSITION : 'none'}
    --chip-color ${playerById(chip.playerId).color}
  `({
    'aria-label': `${playerById(chip.playerId).name} chip`,
    'data-dragging': dragState?.type === 'chip' && dragState.chipId === chip.id ? 'true' : 'false',
    'data-selected': state.selectedChipId === chip.id ? 'true' : 'false',
    'data-locked': chip.locked ? 'true' : 'false',
    onpointerdown: event => startChipDrag(event, chip)
  })
)

const tableCard = s(({ piece }) => {
  const size = cardPieceSize(piece)

  return TableCardButton`
    position ${dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id ? 'fixed' : 'absolute'}
    left ${tableCardLeft(piece) + 'px'}
    top ${tableCardTop(piece) + 'px'}
    z-index ${dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id ? 60 : 6}
    transition ${dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id && dragState.returning ? SNAP_BACK_TRANSITION : 'none'}
    --piece-width ${size.width + 'px'}
    --piece-height ${size.height + 'px'}
    --piece-rotate ${(piece.rotation || 0) + 'deg'}
  `({
    'aria-label': `${piece.card.code} table card`,
    'data-piece-id': piece.id,
    'data-dragging': dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id ? 'true' : 'false',
    'data-selected': state.selectedTableCardId === piece.id ? 'true' : 'false',
    'data-locked': piece.locked ? 'true' : 'false',
    onpointerdown: event => startTableCardDrag(event, piece)
  },
    cardVisual({ card: piece.card, faceUp: piece.faceUp })
  )
})

const deckObject = s(() =>
  DeckBody(
    DeckStack({
      'data-draw-stack': 'true'
    },
      StackCount(state.drawPile.length)
    ),
    ObjectActions(
      MiniButton({ 'data-no-drag': 'true', onclick: drawOne }, 'Draw'),
      MiniButton({ 'data-no-drag': 'true', onclick: shuffleDrawPile }, 'Shuffle')
    )
  )
)

const discardObject = s(() => {
  const top = state.discardPile[state.discardPile.length - 1]

  return DiscardBody({
    'data-drop-zone': ZONE.DISCARD,
    'data-drop-ready': zoneAcceptsCardDrop(ZONE.DISCARD, dragState?.type) ? 'true' : 'false'
  },
    top ? compactCard({
      card: top,
      selected: state.selectedDiscardCardId === top.id,
      onpointerdown: event => startDiscardCardDrag(event, top)
    }) : DeckStack`background rgba(255,255,255,.05); box-shadow none`('Empty'),
    Pill(`${state.discardPile.length} discarded`)
  )
})

const supplyObject = s(() =>
  SupplyBody(
    players.map(player =>
      SupplyRow({
        key: player.id,
        'data-no-drag': 'true'
      },
        SupplyChip({
          style: `background: ${player.color}`,
          onpointerdown: event => createChipFromSupply(event, player.id)
        }),
        player.name
      )
    )
  )
)

const compactCard = s(({ card, selected = false, onpointerdown }) =>
  DeckStack`
    background var(--paper)
    color var(--ink)
    box-shadow 0 10px 24px rgba(0,0,0,.28)
    cursor grab
    outline ${selected ? '3px solid #f1d28a' : '0 solid transparent'}
    outline-offset 3px
  `({
    onpointerdown
  },
    cardVisual({ card, faceUp: card.faceUp !== false })
  )
)

const cardVisual = s(({ card, faceUp = true }) =>
  faceUp
    ? CardFace(
      CardRank({ style: `color: ${suitMeta(card.suit).color}` }, card.rank),
      CardSuit({ style: `color: ${suitMeta(card.suit).color}` }, card.suit)
    )
    : CardBack('TABLETIME')
)

const sidePanel = s(() =>
  SidePanel(
    PanelSection(
      PanelTitle('State'),
      MetricGrid(
        Metric(MetricValue(state.hand.length), MetricLabel('hand')),
        Metric(MetricValue(state.drawPile.length), MetricLabel('draw pile')),
        Metric(MetricValue(state.discardPile.length), MetricLabel('discard')),
        Metric(MetricValue(state.chips.length), MetricLabel('table chips')),
        Metric(MetricValue(state.tableCards.length), MetricLabel('loose cards'))
      )
    ),
    PanelSection(
      PanelTitle('Model'),
      LogList(
        ['Tabletop', 'TableObject', 'BoardGrid', 'Deck', 'Hand', 'Presence later'].map(item =>
          LogItem({ key: item }, item)
        )
      )
    ),
    PanelSection(
      PanelTitle('Log'),
      LogList(
        state.log.map((item, index) => LogItem({ key: `${index}-${item}` }, item))
      )
    )
  )
)

const handBar = s(() =>
  HandBar({
    'data-drop-zone': ZONE.HAND
  },
    HandStatus(
      TitleBlock(
        Title('Player Hand'),
        Subtitle(selectedHandCard() ? `${selectedHandCard().code} selected` : 'No card selected')
      )
    ),
    HandCards({
      'data-hand-cards': 'true'
    },
      state.hand.map((card, index) => handCard({ key: card.id, card, index, total: state.hand.length }))
    ),
    ActionRail(
      ToolbarButton({ onclick: drawOne }, 'Draw'),
      SecondaryButton({ onclick: shuffleDrawPile }, 'Shuffle')
    )
  )
)

const handCard = s(({ card, index, total }) => {
  const metrics = handMetrics()
  const spacing = total <= 1 ? 0 : metrics.spacing
  const totalWidth = (total - 1) * spacing + metrics.width
  const x = index * spacing + 'px'
  const progress = total <= 1 ? 0.5 : index / (total - 1)
  const rotate = (progress - 0.5) * metrics.fanDegrees
  const lift = total <= 1 ? 0 : Math.sin(progress * Math.PI) * metrics.arcLift

  return HandCardButton`
    left calc(50% - ${totalWidth / 2 + 'px'} + ${x})
    width ${metrics.width + 'px'}
    height ${metrics.height + 'px'}
    bottom 0
    --card-rotate ${rotate + 'deg'}
    --card-lift ${-lift + 'px'}
    transform translateY(var(--card-lift)) rotate(${rotate + 'deg'})
  `({
    selected: state.selectedHandCardId === card.id,
    'data-dragging': dragState?.type === CARD_DRAG_TYPE.HAND && dragState.cardId === card.id && dragState.moved ? 'true' : 'false',
    onpointerdown: event => startHandCardDrag(event, card)
  },
    HandCardInner(
      CornerText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(card.suit)),
      CenterRank({ style: `color: ${suitMeta(card.suit).color}` }, card.rank),
      BottomText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(card.suit))
    )
  )
})

function handMetrics() {
  const width = s.is.server ? 1200 : window.innerWidth

  if (width < 560) return { width: 62, height: 88, spacing: 34, arcLift: 14, fanDegrees: 20 }
  if (width < 820) return { width: 88, height: 122, spacing: 55, arcLift: 20, fanDegrees: 24 }
  return { width: CARD_PORTRAIT_WIDTH, height: CARD_PORTRAIT_HEIGHT, spacing: 74, arcLift: 26, fanDegrees: 26 }
}

function cardPieceSize(piece) {
  return piece.orientation === 'landscape'
    ? { width: CARD_LANDSCAPE_WIDTH, height: CARD_LANDSCAPE_HEIGHT }
    : { width: CARD_PORTRAIT_WIDTH, height: CARD_PORTRAIT_HEIGHT }
}

s.mount(() => App())
