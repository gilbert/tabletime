import s from 'cofound'

const TABLE_WIDTH = 1740
const TABLE_HEIGHT = 1120
const BOARD_SIZE = 10
const CELL_SIZE = 72
const HAND_SIZE = 7

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

s.css.reset`
  body {
    background: #16191d;
    color: #f5f1e8;
  }

  button {
    border: 0;
  }
`

s.css`
  :root {
    color-scheme: dark;
    --felt: #1f6d4a;
    --felt-dark: #145239;
    --ink: #20242a;
    --paper: #fbf4df;
    --muted: #9ba6b2;
    --line: rgba(255,255,255,.16);
    --shadow: 0 18px 50px rgba(0,0,0,.32);
  }

  html,
  body {
    width: 100%;
    height: 100%;
    overflow: hidden;
  }
`

const AppShell = s`main
  display grid
  grid-template-rows 58px minmax(0, 1fr) 190px
  width 100vw
  height 100svh
  background #16191d
`

const TopBar = s`header
  display flex
  align-items center
  justify-content space-between
  gap 18px
  width 100vw
  overflow hidden
  padding 0 18px
  border-bottom 1px solid rgba(255,255,255,.1)
  background #20242a
  box-shadow 0 8px 24px rgba(0,0,0,.22)
  z-index 10
`

const Brand = s`div
  display flex
  align-items center
  gap 12px
  flex 0 0 auto
`

const Mark = s`div
  width 30px
  height 30px
  border-radius 7px
  background linear-gradient(135deg, #d9b35f, #a94747 48%, #286caa)
  box-shadow inset 0 0 0 1px rgba(255,255,255,.24)
`

const TitleBlock = s`div
  display grid
  gap 2px
`

const Title = s`h1
  font-size 15px
  line-height 1
  font-weight 750
  letter-spacing 0
`

const Subtitle = s`p
  font-size 12px
  line-height 1.2
  color var(--muted)
`

const StatusStrip = s`div
  display flex
  align-items center
  justify-content flex-end
  gap 10px
  min-width 0
  overflow-x auto
  scrollbar-width none

  &::-webkit-scrollbar {
    display none
  }

  @media (max-width: 560px) {
    justify-content flex-start
    gap 8px
  }
`

const Pill = s`span
  display inline-flex
  align-items center
  gap 7px
  min-height 30px
  padding 0 10px
  border 1px solid rgba(255,255,255,.12)
  border-radius 7px
  background rgba(255,255,255,.06)
  color #e9edf1
  font-size 12px
  white-space nowrap
  flex 0 0 auto

  @media (max-width: 560px) {
    padding 0 9px
    font-size 11px
  }
`

const PlayerDot = s`span
  display inline-block
  width 10px
  height 10px
  border-radius 50%
  box-shadow 0 0 0 2px rgba(255,255,255,.18)
`

const ToolbarButton = s`button
  min-height 34px
  padding 0 12px
  border-radius 7px
  background #f1d28a
  color #24201a
  font-size 13px
  font-weight 750
  cursor pointer

  &:hover {
    background #ffe09b
  }

  &:active {
    transform translateY(1px)
  }

  @media (max-width: 560px) {
    min-height 32px
    padding 0 9px
    font-size 12px
  }
`

const SecondaryButton = ToolbarButton`
  background rgba(255,255,255,.1)
  color #f5f1e8
  border 1px solid rgba(255,255,255,.12)

  &:hover {
    background rgba(255,255,255,.16)
  }
`

const Workspace = s`section
  position relative
  min-width 0
  min-height 0
  overflow auto
  background
    radial-gradient(circle at 20% 20%, rgba(255,255,255,.08), transparent 28%),
    linear-gradient(135deg, #1b5d41, #16513a 48%, #124434)
`

const TableSurface = s`div
  position relative
  width ${TABLE_WIDTH + 'px'}
  height ${TABLE_HEIGHT + 'px'}
  background
    linear-gradient(rgba(255,255,255,.03) 1px, transparent 1px),
    linear-gradient(90deg, rgba(255,255,255,.03) 1px, transparent 1px),
    radial-gradient(circle at 50% 40%, rgba(255,255,255,.07), transparent 45%),
    var(--felt)
  background-size 80px 80px, 80px 80px, auto, auto
  box-shadow inset 0 0 0 18px rgba(15, 52, 38, .72), inset 0 0 120px rgba(0,0,0,.35)
`

const TableObjectShell = s`section
  position absolute
  z-index 2
  border 1px solid rgba(255,255,255,.16)
  border-radius 8px
  background rgba(32,36,42,.9)
  box-shadow var(--shadow)
  overflow hidden
  user-select none
  cursor default
`

const ObjectHeader = s`header
  display flex
  align-items center
  justify-content space-between
  gap 10px
  height 34px
  padding 0 10px
  border-bottom 1px solid rgba(255,255,255,.1)
  background rgba(255,255,255,.06)
  color #e8edf1
  font-size 12px
  font-weight 750
  touch-action none
`

const LockBadge = s`span
  color var(--muted)
  font-size 11px
  font-weight 650
`

const BoardWrap = s`div
  padding 12px
`

const BoardGrid = s`div
  display grid
  grid-template-columns repeat(10, 72px)
  grid-template-rows repeat(10, 72px)
  gap 4px
`

const BoardCell = s`div
  position relative
  display grid
  place-items center
  width 72px
  height 72px
  border 1px solid rgba(44, 51, 59, .22)
  border-radius 6px
  background var(--paper)
  color var(--ink)
  box-shadow inset 0 0 0 1px rgba(255,255,255,.38)
  cursor default

  &[data-free="true"] {
    background #d7b365
  }
`

const CardFace = s`div
  display grid
  place-items center
  gap 2px
  width 100%
  height 100%
  padding 6px
`

const CardRank = s`strong
  font-size 18px
  line-height 1
  letter-spacing 0
`

const CardSuit = s`span
  font-size 12px
  line-height 1
  font-weight 800
`

const TableChip = s`button
  position absolute
  z-index 5
  width 38px
  height 38px
  border-radius 50%
  background var(--chip-color)
  border 3px solid rgba(255,255,255,.72)
  box-shadow 0 4px 10px rgba(0,0,0,.35), inset 0 -5px 8px rgba(0,0,0,.22), inset 0 5px 8px rgba(255,255,255,.22)
  cursor grab
  touch-action none

  &:active {
    cursor grabbing
  }

  &[data-dragging="true"] {
    transform scale(1.06)
    box-shadow 0 12px 24px rgba(0,0,0,.4), inset 0 -5px 8px rgba(0,0,0,.22), inset 0 5px 8px rgba(255,255,255,.22)
  }

  &[data-selected="true"] {
    outline 3px solid #f1d28a
    outline-offset 3px
  }

  &[data-locked="true"] {
    cursor default
    filter saturate(.75)
  }
`

const TableCardButton = s`button
  position absolute
  z-index 6
  display grid
  width var(--piece-width)
  height var(--piece-height)
  border 1px solid rgba(37,42,49,.22)
  border-radius 8px
  background var(--paper)
  color var(--ink)
  box-shadow 0 16px 30px rgba(0,0,0,.34)
  cursor grab
  touch-action none
  transform rotate(var(--piece-rotate))
  transform-origin 50% 50%

  &:active {
    cursor grabbing
  }

  &[data-selected="true"] {
    outline 3px solid #f1d28a
    outline-offset 3px
  }

  &[data-locked="true"] {
    cursor default
    filter saturate(.7)
  }

  &[data-dragging="true"] {
    z-index 60
  }
`

const CardBack = s`div
  display grid
  place-items center
  width 100%
  height 100%
  border-radius 7px
  background
    linear-gradient(45deg, rgba(255,255,255,.12) 25%, transparent 25%),
    linear-gradient(-45deg, rgba(255,255,255,.12) 25%, transparent 25%),
    #29313a
  background-size 18px 18px
  color #f5f1e8
  font-size 13px
  font-weight 850
`

const FreeCorner = s`span
  display grid
  place-items center
  width 42px
  height 42px
  border-radius 50%
  border 2px dashed rgba(45,42,32,.42)
  color rgba(45,42,32,.72)
  font-size 11px
  font-weight 850
`

const DeckBody = s`div
  display grid
  gap 12px
  width 180px
  padding 14px
`

const DeckStack = s`div
  position relative
  display grid
  place-items center
  width 112px
  height 152px
  margin 0 auto
  border-radius 7px
  background #29313a
  border 2px solid rgba(255,255,255,.16)
  box-shadow 7px 7px 0 #1d242b, 13px 13px 0 #151b21
  color #f5f1e8
  font-size 13px
  font-weight 800
`

const StackCount = s`span
  display grid
  place-items center
  width 54px
  height 54px
  border-radius 50%
  background #f1d28a
  color #24201a
  box-shadow 0 8px 20px rgba(0,0,0,.32)
`

const ObjectActions = s`div
  display grid
  grid-template-columns 1fr 1fr
  gap 8px
`

const MiniButton = s`button
  height 32px
  border-radius 6px
  background rgba(255,255,255,.1)
  color #f5f1e8
  border 1px solid rgba(255,255,255,.14)
  cursor pointer
  font-size 12px
  font-weight 750

  &:hover {
    background rgba(255,255,255,.16)
  }
`

const DiscardBody = s`div
  display grid
  place-items center
  gap 10px
  width 168px
  padding 14px

  &[data-drop-ready="true"] {
    background rgba(241,210,138,.12)
    box-shadow inset 0 0 0 2px rgba(241,210,138,.7)
  }
`

const SupplyBody = s`div
  display grid
  gap 12px
  width 180px
  padding 14px
`

const SupplyRow = s`div
  display flex
  align-items center
  justify-content space-between
  gap 10px
  height 38px
  padding 0 10px
  border-radius 7px
  background rgba(255,255,255,.08)
  color #f5f1e8
  cursor default
  font-size 13px
  font-weight 750
`

const SupplyChip = s`span
  display inline-block
  width 22px
  height 22px
  border-radius 50%
  border 2px solid rgba(255,255,255,.72)
  box-shadow 0 3px 8px rgba(0,0,0,.3), inset 0 -4px 6px rgba(0,0,0,.2), inset 0 4px 6px rgba(255,255,255,.18)
  cursor grab
  touch-action none
`

const SidePanel = s`aside
  position absolute
  z-index 8
  right 24px
  top 690px
  width 285px
  border 1px solid rgba(255,255,255,.14)
  border-radius 8px
  background rgba(32,36,42,.92)
  box-shadow var(--shadow)
  overflow hidden
`

const PanelSection = s`section
  padding 12px
  border-top 1px solid rgba(255,255,255,.08)

  &:first-child {
    border-top 0
  }
`

const PanelTitle = s`h2
  margin-bottom 8px
  color #f5f1e8
  font-size 12px
  line-height 1
  font-weight 850
`

const MetricGrid = s`div
  display grid
  grid-template-columns 1fr 1fr
  gap 8px
`

const Metric = s`div
  display grid
  gap 4px
  padding 9px
  border-radius 7px
  background rgba(255,255,255,.07)
`

const MetricValue = s`strong
  font-size 18px
  line-height 1
`

const MetricLabel = s`span
  color var(--muted)
  font-size 11px
  line-height 1.2
`

const LogList = s`ol
  display grid
  gap 7px
`

const LogItem = s`li
  color #cdd5dd
  font-size 12px
  line-height 1.3
`

const HandBar = s`section
  position relative
  z-index 12
  width 100vw
  overflow visible
  border-top 1px solid rgba(255,255,255,.1)
  background #20242a
  box-shadow 0 -10px 28px rgba(0,0,0,.24)
`

const HandStatus = s`div
  position absolute
  left 18px
  top 16px
  display grid
  gap 7px
  width 205px
`

const HandCards = s`div
  position absolute
  left 50%
  bottom 14px
  width min(760px, calc(100vw - 36px))
  height 160px
  transform translateX(-50%)
  overflow visible

  @media (max-width: 560px) {
    bottom 4px
    height 124px
  }
`

const HandCardButton = s`button
  position absolute
  bottom 0
  width 104px
  height 142px
  border 1px solid rgba(37,42,49,.22)
  border-radius 8px
  background var(--paper)
  color var(--ink)
  box-shadow 0 14px 28px rgba(0,0,0,.3)
  cursor pointer
  transform-origin 50% 140%
  transition transform 140ms, box-shadow 140ms

  &:hover {
    transform translateY(calc(var(--card-lift) - 18px)) rotate(var(--card-rotate))
    box-shadow 0 22px 38px rgba(0,0,0,.38)
  }

  &[selected] {
    outline 3px solid #f1d28a
    outline-offset 3px
    transform translateY(calc(var(--card-lift) - 24px)) rotate(var(--card-rotate))
  }

  &[data-dragging="true"] {
    opacity .28
  }
`

const HandCardInner = s`div
  display grid
  grid-template-rows auto 1fr auto
  width 100%
  height 100%
  padding 10px
`

const CornerText = s`span
  justify-self start
  display grid
  gap 2px
  font-size 15px
  line-height 1
  font-weight 850
`

const CenterRank = s`strong
  place-self center
  font-size 32px
  line-height 1
  letter-spacing 0
`

const BottomText = CornerText`
  justify-self end
  transform rotate(180deg)
`

const ActionRail = s`div
  position absolute
  right 18px
  top 16px
  display flex
  gap 8px

  @media (max-width: 560px) {
    right 8px
    top 14px
    gap 6px
    flex-direction column
  }
`

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

      object.x = clamp(event.clientX - rect.left - dragState.offsetX, 18, TABLE_WIDTH - dragState.width - 18)
      object.y = clamp(event.clientY - rect.top - dragState.offsetY, 18, TABLE_HEIGHT - dragState.height - 18)
    }

    if (dragState.type === 'chip') {
      const chip = chipById(dragState.chipId)
      if (!chip) return

      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      chip.x = event.clientX - rect.left - dragState.offsetX
      chip.y = event.clientY - rect.top - dragState.offsetY
    }

    if (dragState.type === 'table-card') {
      const piece = tableCardById(dragState.pieceId)
      if (!piece || piece.locked) return

      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      piece.x = event.clientX - rect.left - dragState.offsetX
      piece.y = event.clientY - rect.top - dragState.offsetY
    }

    if (dragState.type === 'hand-card' || dragState.type === 'discard-card') {
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

    if (currentDrag.type === 'table-card') {
      if (!currentDrag.moved) selectTableCard(currentDrag.pieceId)
      else finishTableCardDrop(event, currentDrag)
    }

    if (currentDrag.type === 'hand-card') {
      if (!currentDrag.moved) {
        selectHandCard(currentDrag.cardId)
      } else {
        finishHandCardDrop(event, currentDrag)
      }
    }

    if (currentDrag.type === 'discard-card') {
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
    x: clamp(point.x - 19, 8, TABLE_WIDTH - 46),
    y: clamp(point.y - 19, 8, TABLE_HEIGHT - 46)
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
    offsetX: 19,
    offsetY: 19,
    width: 38,
    height: 38
  }

  scheduleRedraw()
}

function startHandCardDrag(event, card) {
  event.preventDefault()
  event.stopPropagation()

  const rect = event.currentTarget.getBoundingClientRect()
  const originRect = rectFromDomRect(rect)
  dragState = {
    type: 'hand-card',
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
    type: 'discard-card',
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
    type: 'table-card',
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

  if (zoneAcceptsCardDrop(zoneId, drag)) {
    state.hand = state.hand.filter(item => item.id !== card.id)
    state.discardPile.push({ ...card, faceUp: true })
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

  if (zoneId === 'hand') {
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

  if (zoneId === 'table') {
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

  if (zoneId === 'hand') {
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

  if (zoneId === 'discard') {
    state.tableCards = state.tableCards.filter(item => item.id !== piece.id)
    state.discardPile.push({ ...piece.card, faceUp: true })
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

function zoneAcceptsCardDrop(zoneId, drag) {
  if (drag.type === 'hand-card') return zoneId === 'discard'
  return false
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
  }, 190)
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
  if (dragState?.type !== 'table-card' || dragState.pieceId !== piece.id) return piece.x
  if (dragState.returning) return dragState.snapLeft
  return dragState.clientX - dragState.offsetX
}

function tableCardTop(piece) {
  if (dragState?.type !== 'table-card' || dragState.pieceId !== piece.id) return piece.y
  if (dragState.returning) return dragState.snapTop
  return dragState.clientY - dragState.offsetY
}

function clampPiecePosition(x, y, width, height) {
  return {
    x: clamp(x, 8, TABLE_WIDTH - width - 8),
    y: clamp(y, 8, TABLE_HEIGHT - height - 8)
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
        'data-drop-zone': 'table',
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
    'data-drop-zone': 'board'
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
    transition ${dragState?.type === 'chip' && dragState.chipId === chip.id && dragState.returning ? 'left 190ms cubic-bezier(.2,.8,.2,1), top 190ms cubic-bezier(.2,.8,.2,1)' : 'none'}
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
    position ${dragState?.type === 'table-card' && dragState.pieceId === piece.id ? 'fixed' : 'absolute'}
    left ${tableCardLeft(piece) + 'px'}
    top ${tableCardTop(piece) + 'px'}
    z-index ${dragState?.type === 'table-card' && dragState.pieceId === piece.id ? 60 : 6}
    transition ${dragState?.type === 'table-card' && dragState.pieceId === piece.id && dragState.returning ? 'left 190ms cubic-bezier(.2,.8,.2,1), top 190ms cubic-bezier(.2,.8,.2,1)' : 'none'}
    --piece-width ${size.width + 'px'}
    --piece-height ${size.height + 'px'}
    --piece-rotate ${(piece.rotation || 0) + 'deg'}
  `({
    'aria-label': `${piece.card.code} table card`,
    'data-piece-id': piece.id,
    'data-dragging': dragState?.type === 'table-card' && dragState.pieceId === piece.id ? 'true' : 'false',
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
    'data-drop-zone': 'discard',
    'data-drop-ready': dragState?.type === 'hand-card' ? 'true' : 'false'
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
    'data-drop-zone': 'hand'
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
    'data-dragging': dragState?.type === 'hand-card' && dragState.cardId === card.id && dragState.moved ? 'true' : 'false',
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
  return { width: 104, height: 142, spacing: 74, arcLift: 26, fanDegrees: 26 }
}

function cardPieceSize(piece) {
  return piece.orientation === 'landscape'
    ? { width: 142, height: 104 }
    : { width: 104, height: 142 }
}

s.mount(() => App())
