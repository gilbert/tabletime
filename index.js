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
  AuthField,
  AuthOverlay,
  AuthPanel,
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
  FormError,
  FormNote,
  HandBar,
  HandCardButton,
  HandCardInner,
  HandCards,
  HandStatus,
  LockBadge,
  LogItem,
  LogList,
  LogPanelSection,
  MainContent,
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
  PanelActions,
  PresenceCursor,
  SecondaryButton,
  SeatList,
  SeatName,
  SeatOccupant,
  SeatRow,
  SeatSwatch,
  SeatText,
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
import { applyCommand, applySnapshot, COMMAND, createCommand } from './game/commands.js'
import { createInitialGameState, players, sequenceSpaces, suits } from './game/setup.js'
import { createMultiplayerClient } from './network.js'
import { CARD_DRAG_TYPE, CARD_DROP_ACTION, ZONE, cardDropRule, zoneAcceptsCardDrop } from './zones.js'

installGlobalStyles()

let dragState = null
let dragGhost = null
let drawAnimationActive = false
let pointerListenersAttached = false
let networkAttached = false
let redrawQueued = false
let multiplayer = null
let networkStatus = 'offline'
let roomId = 'sequence'
let localClientId = 'local'
let localPlayerName = 'Player'
let loginRequired = false
let loginError = ''
let loginInputValue = ''
let localEntityCounter = 1
let lastPresenceSent = 0
let lastPointer = null
const remotePresence = new Map()
const SERVER_AUTHORITATIVE_COMMANDS = new Set([
  COMMAND.RESET,
  COMMAND.START,
  COMMAND.SEAT_JOIN,
  COMMAND.SEAT_LEAVE
])

const state = createInitialGameState()

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

function seatForClient(clientId) {
  return state.seats?.find(seat => seat.clientId === clientId) || null
}

function seatsForDisplay() {
  return state.seats || players.map(player => ({
    playerId: player.id,
    playerName: player.name,
    color: player.color,
    clientId: null,
    clientName: null
  }))
}

function occupiedSeatCount() {
  return seatsForDisplay().filter(seat => seat.clientId).length
}

function minPlayersToStart() {
  return state.minPlayersToStart || 1
}

function canStartGame() {
  return !state.started && occupiedSeatCount() >= minPlayersToStart()
}

function roomCommandUnavailable() {
  return Boolean(multiplayer && !multiplayer.connected)
}

function seatActionLabel(seat) {
  if (seat.clientId === localClientId) return '(you)'
  if (seat.clientId) return 'Taken'
  return 'Join'
}

function drawOne() {
  if (drawAnimationActive) return

  if (state.hand.length >= HAND_SIZE) {
    addLog('Hand is full.')
    return
  }

  if (!state.drawPile.length && !state.discardPile.length) {
    addLog('Draw deck is empty.')
    return
  }

  if (s.is.server) {
    submitCommand(createCommand(COMMAND.DRAW, { count: 1 }))
    return
  }

  drawAnimationActive = true
  animateDrawToHand().finally(() => {
    drawAnimationActive = false
    submitCommand(createCommand(COMMAND.DRAW, { count: 1 }))
    scheduleRedraw()
  })
}

function shuffleDrawPile() {
  submitCommand(createCommand(COMMAND.SHUFFLE_DRAW))
}

function selectHandCard(cardId) {
  state.selectedHandCardId = state.selectedHandCardId === cardId ? null : cardId
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
}

function switchPlayer(playerId) {
  submitCommand(createCommand(COMMAND.SWITCH_PLAYER, { playerId }))
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

function resetPrototype({ confirm = false } = {}) {
  if (confirm && !confirmAction('Reset the game for everyone in this room?')) return
  submitCommand(createCommand(COMMAND.RESET))
}

function startGame() {
  submitCommand(createCommand(COMMAND.START))
}

function joinSeat(playerId) {
  submitCommand(createCommand(COMMAND.SEAT_JOIN, { playerId }))
}

function leaveSeat(playerId) {
  submitCommand(createCommand(COMMAND.SEAT_LEAVE, { playerId }))
}

function handleSeatAction(seat) {
  if (seat.clientId === localClientId) {
    if (!confirmAction(`Leave the ${seat.playerName} seat?`)) return
    leaveSeat(seat.playerId)
    return
  }

  joinSeat(seat.playerId)
}

function confirmAction(message) {
  if (s.is.server) return false
  return window.confirm(message)
}

function addLog(message) {
  const maxEntries = Number(state.logConfig?.maxEntries) || 100
  state.log = [...(state.log || []), message].slice(-maxEntries)
}

function submitCommand(command) {
  if (SERVER_AUTHORITATIVE_COMMANDS.has(command.type) && multiplayer) {
    if (!multiplayer.connected) {
      const result = { ok: false, message: 'Waiting for room connection.' }
      addLog(result.message)
      scheduleRedraw()
      return result
    }

    if (!multiplayer.sendCommand(command)) {
      const result = { ok: false, message: 'Command could not be sent.' }
      addLog(result.message)
      scheduleRedraw()
      return result
    }

    return { ok: true, message: null }
  }

  const result = applyCommand(state, command, {
    actor: {
      clientId: localClientId,
      playerName: localPlayerName
    }
  })
  if (!result.ok) {
    addLog(result.message)
    scheduleRedraw()
    return result
  }

  multiplayer?.sendCommand(command)
  scheduleRedraw()
  return result
}

function applyAuthoritativeSnapshot(snapshot) {
  const selection = {
    selectedHandCardId: state.selectedHandCardId,
    selectedTableCardId: state.selectedTableCardId,
    selectedDiscardCardId: state.selectedDiscardCardId,
    selectedChipId: state.selectedChipId
  }

  applySnapshot(state, snapshot)
  Object.assign(state, selection)
  pruneMissingSelections()
  pruneUnseatedPresence()
  scheduleRedraw()
}

function pruneMissingSelections() {
  if (state.selectedHandCardId && !selectedHandCard()) state.selectedHandCardId = null
  if (state.selectedTableCardId && !selectedTableCard()) state.selectedTableCardId = null
  if (state.selectedDiscardCardId && !selectedDiscardCard()) state.selectedDiscardCardId = null
  if (state.selectedChipId && !chipById(state.selectedChipId)) state.selectedChipId = null
}

function pruneUnseatedPresence() {
  for (const clientId of remotePresence.keys()) {
    if (!seatForClient(clientId)) remotePresence.delete(clientId)
  }
}

function attachNetworking() {
  if (networkAttached || s.is.server) return
  networkAttached = true

  const params = new URLSearchParams(window.location.search)
  roomId = params.get('room') || roomId
  const credentials = loadRoomCredentials(roomId)

  if (!credentials) {
    loginRequired = true
    networkStatus = 'login required'
    scheduleRedraw()
    return
  }

  connectToRoom(credentials)
}

function connectToRoom(credentials) {
  multiplayer = createMultiplayerClient({
    roomId,
    username: credentials.username,
    secret: credentials.secret,
    onStatus: status => {
      networkStatus = status
      scheduleRedraw()
    },
    onWelcome: message => {
      localClientId = message.clientId
      localPlayerName = message.username || message.playerName || localPlayerName
      loginRequired = false
      loginError = ''
      saveRoomCredentials(roomId, {
        username: localPlayerName,
        secret: message.secret
      })
      addLog(`Connected to room ${message.roomId}.`)
    },
    onSnapshot: applyAuthoritativeSnapshot,
    onPresence: updateRemotePresence,
    onAuthError: handleAuthError,
    onRejected: message => {
      if (message.reason) addLog(message.reason)
      if (message.snapshot) applyAuthoritativeSnapshot(message.snapshot)
      else scheduleRedraw()
    }
  })
}

function handleAuthError(message) {
  const reason = message.reason || 'Username could not be used.'
  clearRoomCredentials(roomId)
  multiplayer = null
  loginRequired = true
  loginError = reason
  networkStatus = 'login required'
  addLog(reason)
  scheduleRedraw()
}

function submitUsername(event) {
  event.preventDefault()
  const username = loginInputValue.trim()

  if (!isValidUsername(username)) {
    loginError = 'Use lowercase letters with single spaces only between words.'
    scheduleRedraw()
    return
  }

  loginRequired = false
  loginError = ''
  networkStatus = 'connecting'
  connectToRoom({ username, secret: null })
  scheduleRedraw()
}

function updateLoginInput(event) {
  loginInputValue = sanitizeUsernameInput(event.target.value)
  event.target.value = loginInputValue
}

function sanitizeUsernameInput(value) {
  return String(value || '')
    .toLowerCase()
    .replace(/[^a-z ]/g, '')
    .replace(/\s+/g, ' ')
    .replace(/^\s+/, '')
    .slice(0, 32)
}

function isValidUsername(username) {
  return /^[a-z]+(?: [a-z]+)*$/.test(username)
}

function roomCredentialKey(room) {
  return `tabletime:room:${room}:identity`
}

function loadRoomCredentials(room) {
  try {
    const raw = window.localStorage.getItem(roomCredentialKey(room))
    if (!raw) return null
    const credentials = JSON.parse(raw)
    if (!isValidUsername(credentials?.username) || !credentials?.secret) return null
    return credentials
  } catch {
    return null
  }
}

function saveRoomCredentials(room, credentials) {
  if (!credentials.username || !credentials.secret) return
  try {
    window.localStorage.setItem(roomCredentialKey(room), JSON.stringify(credentials))
  } catch {}
}

function clearRoomCredentials(room) {
  try {
    window.localStorage.removeItem(roomCredentialKey(room))
  } catch {}
}

function updateRemotePresence(message) {
  if (message.presence?.status === 'left') {
    remotePresence.delete(message.clientId)
  } else {
    remotePresence.set(message.clientId, {
      clientId: message.clientId,
      playerId: message.playerId,
      playerName: message.playerName,
      color: message.color,
      updatedAt: Date.now(),
      ...message.presence
    })
  }

  scheduleRedraw()
}

function sendPresence(pointer = lastPointer, options = {}) {
  if (!multiplayer?.connected) return
  if (pointer) lastPointer = pointer

  const now = performance.now()
  if (!options.force && now - lastPresenceSent < 45) return
  lastPresenceSent = now

  multiplayer.sendPresence({
    pointer: pointer || lastPointer,
    selection: currentSelection(),
    drag: dragPresence()
  })
}

function currentSelection() {
  if (state.selectedTableCardId) return { kind: 'table-card', id: state.selectedTableCardId }
  if (state.selectedHandCardId) return { kind: 'hand-card', id: state.selectedHandCardId }
  if (state.selectedDiscardCardId) return { kind: 'discard-card', id: state.selectedDiscardCardId }
  if (state.selectedChipId) return { kind: 'chip', id: state.selectedChipId }
  return null
}

function dragPresence() {
  if (!dragState) return null
  if (dragState.returning) return null

  const base = {
    offsetX: dragState.offsetX,
    offsetY: dragState.offsetY,
    width: dragState.width,
    height: dragState.height
  }

  if (dragState.type === 'chip') {
    const chip = chipById(dragState.chipId)
    return { ...base, kind: 'chip', id: dragState.chipId, x: chip?.x, y: chip?.y }
  }

  if (dragState.type === CARD_DRAG_TYPE.TABLE) {
    const piece = tableCardById(dragState.pieceId)
    return { ...base, kind: 'table-card', id: dragState.pieceId, x: piece?.x, y: piece?.y }
  }

  if (dragState.type === CARD_DRAG_TYPE.HAND) return { kind: 'hand-card', id: dragState.cardId }
  if (dragState.type === CARD_DRAG_TYPE.DISCARD) return { kind: 'discard-card', id: dragState.cardId }

  if (dragState.type === 'object') {
    const object = objectById(dragState.objectId)
    return { ...base, kind: 'object', id: dragState.objectId, x: object?.x, y: object?.y }
  }

  return null
}

function localId(prefix) {
  return `${prefix}-${localClientId}-${localEntityCounter++}`
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
  attachNetworking()

  window.addEventListener('pointermove', event => {
    const pointer = tablePointFromEvent(event)

    if (!dragState) {
      sendPresence(pointer)
      return
    }

    if (dragState.returning) {
      sendPresence(pointer)
      return
    }

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
    sendPresence(pointer)
  })

  window.addEventListener('pointerup', event => {
    if (!dragState) return
    const currentDrag = dragState

    if (currentDrag.type === 'object') {
      const object = objectById(currentDrag.objectId)
      if (currentDrag.moved && object) {
        submitCommand(createCommand(COMMAND.OBJECT_MOVE, {
          objectId: object.id,
          x: object.x,
          y: object.y
        }))
      }
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
  sendPresence(tablePointFromEvent(event), { force: true })
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
  sendPresence(tablePointFromEvent(event), { force: true })
}

function createChipFromSupply(event, playerId) {
  event.preventDefault()
  event.stopPropagation()

  const point = tablePointFromEvent(event)
  if (!point) return

  const chip = {
    id: localId('chip'),
    playerId,
    x: clamp(point.x - CHIP_SIZE / 2, TABLE_PIECE_INSET, TABLE_WIDTH - CHIP_SIZE - TABLE_PIECE_INSET),
    y: clamp(point.y - CHIP_SIZE / 2, TABLE_PIECE_INSET, TABLE_HEIGHT - CHIP_SIZE - TABLE_PIECE_INSET)
  }

  submitCommand(createCommand(COMMAND.CHIP_CREATE, {
    chipId: chip.id,
    playerId: chip.playerId,
    playerName: playerById(chip.playerId).name,
    x: chip.x,
    y: chip.y
  }))
  selectChip(chip.id)
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

  sendPresence(point, { force: true })
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
  sendPresence(tablePointFromEvent(event), { force: true })
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
  sendPresence(tablePointFromEvent(event), { force: true })
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
  sendPresence(tablePointFromEvent(event), { force: true })
}

function finishHandCardDrop(event, drag) {
  const zoneId = dropZoneFromPoint(event)
  const card = state.hand.find(item => item.id === drag.cardId)
  if (!card) return

  const rule = cardDropRule(zoneId, drag.type)

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_DISCARD) {
    const result = submitCommand(createCommand(COMMAND.CARD_HAND_TO_DISCARD, {
      cardId: card.id,
      zoneId
    }))
    if (!result.ok) {
      returnDragGhostToOrigin(drag)
      return
    }
    state.selectedHandCardId = null
    state.selectedDiscardCardId = card.id
    removeDragGhost()
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
    const result = submitCommand(createCommand(COMMAND.CARD_DISCARD_TO_HAND, {
      cardId: card.id,
      zoneId
    }))
    if (!result.ok) {
      returnDragGhostToOrigin(drag)
      return
    }

    state.selectedDiscardCardId = null
    state.selectedHandCardId = card.id
    removeDragGhost()
    return
  }

  if (rule?.action === CARD_DROP_ACTION.PLACE_ON_TABLE) {
    const point = tablePointFromEvent(event)
    if (!point) return

    const pieceId = localId('piece')
    const position = clampPiecePosition(point.x - drag.offsetX, point.y - drag.offsetY, drag.width, drag.height)
    const result = submitCommand(createCommand(COMMAND.CARD_DISCARD_TO_TABLE, {
      cardId: card.id,
      pieceId,
      zoneId,
      x: position.x,
      y: position.y,
      width: drag.width,
      height: drag.height,
      source: 'discard-tray'
    }))
    if (!result.ok) {
      returnDragGhostToOrigin(drag)
      return
    }
    state.selectedDiscardCardId = null
    state.selectedTableCardId = pieceId
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
  submitCommand(createCommand(COMMAND.CHIP_MOVE, {
    chipId: chip.id,
    x: clamped.x,
    y: clamped.y,
    width: drag.width,
    height: drag.height
  }))

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
    const result = submitCommand(createCommand(COMMAND.CARD_TABLE_TO_HAND, {
      pieceId: piece.id,
      zoneId
    }))
    if (!result.ok) {
      finishDrag()
      return
    }

    state.selectedTableCardId = null
    state.selectedHandCardId = piece.card.id
    removeDragGhost()
    return
  }

  if (rule?.action === CARD_DROP_ACTION.MOVE_TO_DISCARD) {
    const result = submitCommand(createCommand(COMMAND.CARD_TABLE_TO_DISCARD, {
      pieceId: piece.id,
      zoneId
    }))
    if (!result.ok) {
      finishDrag()
      return
    }

    state.selectedTableCardId = null
    state.selectedDiscardCardId = piece.card.id
    removeDragGhost()
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
  submitCommand(createCommand(COMMAND.CARD_TABLE_MOVE, {
    pieceId: piece.id,
    x: clamped.x,
    y: clamped.y,
    width: drag.width,
    height: drag.height
  }))

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
  sendPresence(lastPointer, { force: true })
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
  sendPresence(lastPointer, { force: true })
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

function remoteDragPosition(kind, id) {
  const presence = remoteDragPresence(kind, id)
  if (!presence) return null

  const drag = presence.drag
  if (isFiniteNumber(drag.x) && isFiniteNumber(drag.y)) {
    return { x: drag.x, y: drag.y }
  }

  if (!presence.pointer) return null

  return {
    x: presence.pointer.x - (drag.offsetX || 0),
    y: presence.pointer.y - (drag.offsetY || 0)
  }
}

function remoteDragPresence(kind, id) {
  const now = Date.now()

  for (const presence of remotePresence.values()) {
    const drag = presence.drag
    if (!drag || !presence.pointer) continue
    if (drag.kind !== kind || drag.id !== id) continue
    if (now - presence.updatedAt > 1500) continue
    return presence
  }

  return null
}

function isRemoteDragging(kind, id) {
  return Boolean(remoteDragPresence(kind, id))
}

function isFiniteNumber(value) {
  return typeof value === 'number' && Number.isFinite(value)
}

function objectLeft(object) {
  const remote = remoteDragPosition('object', object.id)
  return remote?.x ?? object.x
}

function objectTop(object) {
  const remote = remoteDragPosition('object', object.id)
  return remote?.y ?? object.y
}

function chipLeft(chip) {
  if (dragState?.type === 'chip' && dragState.chipId === chip.id) {
    if (dragState.returning) return dragState.snapLeft
    return dragState.clientX - dragState.offsetX
  }

  const remote = remoteDragPosition('chip', chip.id)
  return remote?.x ?? chip.x
}

function chipTop(chip) {
  if (dragState?.type === 'chip' && dragState.chipId === chip.id) {
    if (dragState.returning) return dragState.snapTop
    return dragState.clientY - dragState.offsetY
  }

  const remote = remoteDragPosition('chip', chip.id)
  return remote?.y ?? chip.y
}

function tableCardLeft(piece) {
  if (dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id) {
    if (dragState.returning) return dragState.snapLeft
    return dragState.clientX - dragState.offsetX
  }

  const remote = remoteDragPosition('table-card', piece.id)
  return remote?.x ?? piece.x
}

function tableCardTop(piece) {
  if (dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id) {
    if (dragState.returning) return dragState.snapTop
    return dragState.clientY - dragState.offsetY
  }

  const remote = remoteDragPosition('table-card', piece.id)
  return remote?.y ?? piece.y
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
    submitCommand(createCommand(COMMAND.CARD_FLIP, { pieceId: piece.id }))
  }

  if (key === 'r' && piece) {
    submitCommand(createCommand(COMMAND.CARD_ROTATE, { pieceId: piece.id }))
  }

  if (key === 'l') {
    if (piece) {
      submitCommand(createCommand(COMMAND.CARD_LOCK, { pieceId: piece.id }))
    } else if (state.selectedChipId) {
      const chip = chipById(state.selectedChipId)
      if (!chip) return
      submitCommand(createCommand(COMMAND.CHIP_LOCK, { chipId: chip.id }))
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
    submitCommand(createCommand(COMMAND.CHIP_RETURN, { chipId: chip.id }))
    state.selectedChipId = null
  }
}

function takeTableCardToHand(piece) {
  const result = submitCommand(createCommand(COMMAND.CARD_TABLE_TO_HAND, {
    pieceId: piece.id,
    zoneId: ZONE.HAND
  }))
  if (!result.ok) return

  state.selectedTableCardId = null
  state.selectedHandCardId = piece.card.id
}

function takeDiscardCardToHand() {
  const card = selectedDiscardCard()
  const top = state.discardPile[state.discardPile.length - 1]
  if (!card || !top || card.id !== top.id) return

  const result = submitCommand(createCommand(COMMAND.CARD_DISCARD_TO_HAND, {
    cardId: card.id,
    zoneId: ZONE.HAND
  }))
  if (!result.ok) return

  state.selectedDiscardCardId = null
  state.selectedHandCardId = card.id
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
    MainContent(
      sidePanel(),
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
          presenceLayer()
        )
      )
    ),
    handBar(),
    loginRequired && loginOverlay()
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
      Pill(`${networkStatus} / ${roomId}`),
      Pill(`${occupiedSeatCount()} / ${seatsForDisplay().length} seated`),
      Pill(`${state.drawPile.length} deck`),
      Pill(`${state.chips.length} chips`),
      Pill(`${state.tableCards.length} loose cards`),
      SecondaryButton({ onclick: () => resetPrototype({ confirm: true }) }, 'Reset')
    )
  )
)

const loginOverlay = s(() =>
  AuthOverlay(
    AuthPanel({
      onsubmit: submitUsername
    },
      TitleBlock(
        Title('Join Room'),
        Subtitle(`Room ${roomId}`)
      ),
      AuthField({
        name: 'username',
        value: loginInputValue,
        placeholder: 'username',
        autocomplete: 'name',
        autofocus: true,
        pattern: '[a-z]+( [a-z]+)*',
        maxlength: 32,
        oninput: updateLoginInput
      }),
      loginError ? FormError(loginError) : FormNote('Lowercase letters only. Use single spaces between words.'),
      ToolbarButton({ type: 'submit' }, 'Join')
    )
  )
)

const tableObject = s(({ object }) => {
  const localDragging = dragState?.type === 'object' && dragState.objectId === object.id
  const remoteDragging = isRemoteDragging('object', object.id)

  return TableObjectShell`
    left ${objectLeft(object) + 'px'}
    top ${objectTop(object) + 'px'}
    z-index ${localDragging ? 60 : remoteDragging ? 55 : 2}
    transition ${remoteDragging ? 'left 45ms linear, top 45ms linear' : 'none'}
  `({
    'data-table-object': 'true'
  },
    ObjectHeader`
      cursor ${object.locked ? 'default' : localDragging ? 'grabbing' : 'grab'}
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
})

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

const tableChip = s(({ chip }) => {
  const localDragging = dragState?.type === 'chip' && dragState.chipId === chip.id
  const remoteDragging = isRemoteDragging('chip', chip.id)

  return TableChip`
    position ${localDragging ? 'fixed' : 'absolute'}
    left ${chipLeft(chip) + 'px'}
    top ${chipTop(chip) + 'px'}
    z-index ${localDragging ? 60 : remoteDragging ? 55 : 5}
    transition ${localDragging && dragState.returning ? SNAP_BACK_TRANSITION : remoteDragging ? 'left 45ms linear, top 45ms linear' : 'none'}
    --chip-color ${playerById(chip.playerId).color}
  `({
    'aria-label': `${playerById(chip.playerId).name} chip`,
    'data-dragging': localDragging || remoteDragging ? 'true' : 'false',
    'data-selected': state.selectedChipId === chip.id ? 'true' : 'false',
    'data-locked': chip.locked ? 'true' : 'false',
    onpointerdown: event => startChipDrag(event, chip)
  })
})

const tableCard = s(({ piece }) => {
  const size = cardPieceSize(piece)
  const localDragging = dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id
  const remoteDragging = isRemoteDragging('table-card', piece.id)

  return TableCardButton`
    position ${localDragging ? 'fixed' : 'absolute'}
    left ${tableCardLeft(piece) + 'px'}
    top ${tableCardTop(piece) + 'px'}
    z-index ${localDragging ? 60 : remoteDragging ? 55 : 6}
    transition ${localDragging && dragState.returning ? SNAP_BACK_TRANSITION : remoteDragging ? 'left 45ms linear, top 45ms linear' : 'none'}
    --piece-width ${size.width + 'px'}
    --piece-height ${size.height + 'px'}
    --piece-rotate ${(piece.rotation || 0) + 'deg'}
  `({
    'aria-label': `${piece.card.code} table card`,
    'data-piece-id': piece.id,
    'data-dragging': localDragging || remoteDragging ? 'true' : 'false',
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

const presenceLayer = s(() =>
  Array.from(remotePresence.values())
    .filter(presence => presence.pointer && seatForClient(presence.clientId))
    .map(presence => {
      const seat = seatForClient(presence.clientId)
      return (
      PresenceCursor`
        left ${presence.pointer.x + 'px'}
        top ${presence.pointer.y + 'px'}
        --presence-color ${seat.color}
      `({ key: presence.clientId },
        s`span`(seat.clientName || presence.playerName || 'Player')
      )
      )
    })
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
      PanelTitle('Game'),
      MetricGrid(
        Metric(MetricValue(state.started ? 'Started' : 'Waiting'), MetricLabel('status')),
        Metric(MetricValue(`${occupiedSeatCount()} / ${seatsForDisplay().length}`), MetricLabel('seated'))
      ),
      SeatList(
        seatsForDisplay().map(seat =>
          SeatRow({ key: seat.playerId },
            SeatSwatch({ style: `--seat-color: ${seat.color}` }),
            SeatText(
              SeatName(seat.playerName),
              SeatOccupant(seat.clientName || 'Open')
            ),
            MiniButton({
              disabled: roomCommandUnavailable() || state.started || (seat.clientId && seat.clientId !== localClientId) ? true : undefined,
              onclick: () => handleSeatAction(seat)
            }, seatActionLabel(seat))
          )
        )
      ),
      PanelActions(
        ToolbarButton({
          disabled: roomCommandUnavailable() || !canStartGame() ? true : undefined,
          onclick: startGame
        }, 'Start')
      )
    ),
    LogPanelSection(
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
