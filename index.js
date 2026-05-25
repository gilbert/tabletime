import s from 'cofound'
import {
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
  AppShell,
  AuthField,
  AuthOverlay,
  AuthPanel,
  BottomText,
  Brand,
  CardBack,
  CardSuit,
  CenterRank,
  CornerText,
  DeckBody,
  DeckStack,
  DiscardCardButton,
  DiscardBody,
  FormError,
  FormNote,
  HandBar,
  HandCardButton,
  HandCardInner,
  HandCards,
  HandIdentity,
  HandSeatSwatch,
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
  PolyominoCell,
  PolyominoPieceButton,
  PolyominoShape,
  RemoteHandDragCard,
  RemoteHandBack,
  RemoteHandCards,
  RemoteHandLabel,
  RemoteHandsLayer,
  RemoteHandZone,
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
  autoscrollLog,
  installGlobalStyles
} from './components.js'
import './games/register-components.js'
import { applyCommand, applySnapshot, COMMAND, createCommand } from './game/commands.js'
import { createInitialGameState, gameConfigs, getGameConfig, players, suits } from './game/setup.js'
import { getObjectComponent } from './games/components.js'
import { createMultiplayerClient } from './network.js'
import { CARD_DRAG_TYPE, CARD_DROP_ACTION, ZONE, cardDropRule, zoneAcceptsCardDrop } from './zones.js'

installGlobalStyles()

let dragState = null
let dragGhost = null
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
const drawAnimatingCardIds = new Set()
const DRAW_ANIMATION_MS = 340
const PIECE_DRAG_TYPE = 'piece'
const SERVER_AUTHORITATIVE_COMMANDS = new Set([
  COMMAND.RESET,
  COMMAND.START,
  COMMAND.GAME_CHANGE,
  COMMAND.SEAT_JOIN,
  COMMAND.SEAT_LEAVE,
  COMMAND.DRAW,
  COMMAND.SHUFFLE_DRAW,
  COMMAND.CARD_HAND_TO_DISCARD,
  COMMAND.CARD_DISCARD_TO_HAND
])

const state = createInitialGameState()

function activePlayer() {
  return playersForCurrentGame().find(player => player.id === state.activePlayerId) || playersForCurrentGame()[0]
}

function currentGameConfig() {
  return getGameConfig(state.gameId)
}

function playersForCurrentGame() {
  return currentGameConfig().players || players
}

function featureEnabled(feature) {
  return state.features?.[feature] !== false
}

function suitMeta(suitId) {
  return suits.find(suit => suit.id === suitId) || suits[0]
}

function suitGlyph(suitId) {
  const suit = suitMeta(suitId)
  return suit.symbol || suit.id
}

function cardLabel(card) {
  return `${card.rank}${suitGlyph(card.suit)}`
}

function objectById(id) {
  return state.objects.find(object => object.id === id)
}

function selectedHandCard() {
  return localHandCards().find(card => card.id === state.selectedHandCardId)
}

function chipById(id) {
  return state.chips.find(chip => chip.id === id)
}

function tableCardById(id) {
  return state.tableCards.find(piece => piece.id === id)
}

function pieceById(id) {
  return state.pieces?.find(piece => piece.id === id)
}

function selectedTableCard() {
  return tableCardById(state.selectedTableCardId)
}

function selectedDiscardCard() {
  return state.discardPile.find(card => card.id === state.selectedDiscardCardId)
}

function selectedPiece() {
  return pieceById(state.selectedPieceId)
}

function playerById(id) {
  const seat = seatsForDisplay().find(player => player.playerId === id)
  if (seat) return { id: seat.playerId, name: seat.playerName, color: seat.color }
  return playersForCurrentGame().find(player => player.id === id) || playersForCurrentGame()[0]
}

function seatForClient(clientId) {
  return state.seats?.find(seat => seat.clientId === clientId) || null
}

function localSeat() {
  return seatForClient(localClientId)
}

function seatsForDisplay() {
  return state.seats || playersForCurrentGame().map(player => ({
    playerId: player.id,
    playerName: player.name,
    color: player.color,
    clientId: null,
    clientName: null
  }))
}

function handValueForPlayer(playerId) {
  return state.handsByPlayerId?.[playerId] || []
}

function handCardsForPlayer(playerId) {
  const hand = handValueForPlayer(playerId)
  return Array.isArray(hand) ? hand : []
}

function localHandCards() {
  const seat = localSeat()
  return seat ? handCardsForPlayer(seat.playerId) : []
}

function handCountForPlayer(playerId) {
  const hand = handValueForPlayer(playerId)
  if (Array.isArray(hand)) return hand.length
  return Math.max(0, Number(hand?.count) || 0)
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

function canManipulateComponents() {
  return Boolean(localSeat())
}

function manipulationUnavailableReason() {
  if (!canManipulateComponents()) return 'Join a seat before manipulating components.'
  if (roomCommandUnavailable()) return 'Waiting for room connection.'
  return ''
}

function seatActionLabel(seat) {
  if (seat.clientId === localClientId) return '(you)'
  if (seat.clientId) return 'Taken'
  return 'Join'
}

function drawOne() {
  if (drawUnavailableReason()) return
  submitCommand(createCommand(COMMAND.DRAW, { count: 1 }))
}

function shuffleDrawPile() {
  if (manipulationUnavailableReason()) return
  submitCommand(createCommand(COMMAND.SHUFFLE_DRAW))
}

function drawUnavailableReason() {
  if (!featureEnabled('hands') || !featureEnabled('deck')) return 'Drawing is not available for this game.'
  if (!state.started) return 'Start the game before drawing.'
  const seat = localSeat()
  if (!seat) return 'Join a seat before drawing.'
  if (localHandCards().length >= HAND_SIZE) return 'Hand is full.'
  if (!state.drawPile.length && !state.discardPile.length) return 'Draw deck is empty.'
  if (roomCommandUnavailable()) return 'Waiting for room connection.'
  return ''
}

function selectHandCard(cardId) {
  state.selectedHandCardId = state.selectedHandCardId === cardId ? null : cardId
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
  state.selectedPieceId = null
}

function switchPlayer(playerId) {
  submitCommand(createCommand(COMMAND.SWITCH_PLAYER, { playerId }))
}

function selectTableCard(pieceId) {
  state.selectedTableCardId = state.selectedTableCardId === pieceId ? null : pieceId
  state.selectedHandCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
  state.selectedPieceId = null
}

function selectDiscardCard(cardId) {
  state.selectedDiscardCardId = state.selectedDiscardCardId === cardId ? null : cardId
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedChipId = null
  state.selectedPieceId = null
}

function selectChip(chipId) {
  state.selectedChipId = state.selectedChipId === chipId ? null : chipId
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedPieceId = null
}

function selectPiece(pieceId) {
  state.selectedPieceId = pieceId
  state.selectedHandCardId = null
  state.selectedTableCardId = null
  state.selectedDiscardCardId = null
  state.selectedChipId = null
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

function clearPieceSelectionFromTable(event) {
  if (!state.selectedPieceId) return
  if (event.target.closest?.('[data-piece-id]')) return
  state.selectedPieceId = null
  scheduleRedraw()
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

function applyAuthoritativeSnapshot(snapshot, message = {}) {
  const previousSeat = localSeat()
  const previousHandIds = new Set(previousSeat ? localHandCards().map(card => card.id) : [])
  const selection = {
    selectedHandCardId: state.selectedHandCardId,
    selectedTableCardId: state.selectedTableCardId,
    selectedDiscardCardId: state.selectedDiscardCardId,
    selectedChipId: state.selectedChipId,
    selectedPieceId: state.selectedPieceId
  }

  applySnapshot(state, snapshot)
  Object.assign(state, selection)
  pruneMissingSelections()
  pruneUnseatedPresence()
  const currentSeat = localSeat()
  const addedLocalCards = previousSeat && currentSeat?.playerId === previousSeat.playerId && isDrawSnapshotMessage(message)
    ? localHandCards().filter(card => !previousHandIds.has(card.id))
    : []
  const addedLocalCardIds = addedLocalCards.map(card => card.id)
  for (const cardId of addedLocalCardIds) drawAnimatingCardIds.add(cardId)
  scheduleRedraw()
  if (addedLocalCardIds.length) queueDrawAnimations(addedLocalCardIds)
}

function isDrawSnapshotMessage(message) {
  return /\bdrew \d+ cards?\./.test(message?.message || '')
}

function queueDrawAnimations(cardIds) {
  if (s.is.server || !cardIds.length) return

  window.requestAnimationFrame(() => {
    window.requestAnimationFrame(() => startDrawAnimations(cardIds))
  })
}

function startDrawAnimations(cardIds) {
  const source = document.querySelector('[data-draw-stack="true"]')
  if (!source) return

  const sourceRect = animationSourceRect(source)
  let startedAny = false

  for (const cardId of cardIds) {
    if (activeDrawAnimationForCard(cardId)) continue
    const target = findHandCardElement(cardId)
    if (!target) continue

    startedAny = true
    drawAnimatingCardIds.add(cardId)
    animateDrawCard({
      cardId,
      startRect: cardSizedRectAtCenter(sourceRect, target),
      endRect: animationTargetRect(target)
    })
  }

  if (!startedAny) {
    for (const cardId of cardIds) drawAnimatingCardIds.delete(cardId)
    scheduleRedraw()
  }
}

function findHandCardElement(cardId) {
  return Array.from(document.querySelectorAll('[data-hand-card-id]'))
    .find(element => element.dataset.handCardId === cardId)
}

function activeDrawAnimationForCard(cardId) {
  return Array.from(document.querySelectorAll('[data-draw-card-animation="true"]'))
    .some(element => element.dataset.cardId === cardId)
}

function animationSourceRect(source) {
  return rectFromDomRect(source.getBoundingClientRect())
}

function animationTargetRect(target) {
  const targetRect = rectFromDomRect(target.getBoundingClientRect())
  return centeredRect(targetRect, cardAnimationSize(target))
}

function cardSizedRectAtCenter(sourceRect, target) {
  return centeredRect(sourceRect, cardAnimationSize(target))
}

function cardAnimationSize(target) {
  return {
    width: target.offsetWidth || CARD_PORTRAIT_WIDTH,
    height: target.offsetHeight || CARD_PORTRAIT_HEIGHT
  }
}

function centeredRect(rect, size) {
  return {
    left: rect.left + rect.width / 2 - size.width / 2,
    top: rect.top + rect.height / 2 - size.height / 2,
    width: size.width,
    height: size.height
  }
}

function animateDrawCard({ cardId, startRect, endRect }) {
  const element = createDrawAnimationElement(cardId, startRect)
  document.body.append(element)

  const finish = () => {
    element.remove()
    drawAnimatingCardIds.delete(cardId)
    scheduleRedraw()
  }

  if (typeof element.animate === 'function') {
    const animation = element.animate([
      drawAnimationFrame(startRect, .92),
      drawAnimationFrame(endRect, .98)
    ], {
      duration: DRAW_ANIMATION_MS,
      easing: 'cubic-bezier(.2,.8,.2,1)',
      fill: 'forwards'
    })
    if (animation.finished) {
      animation.finished.then(finish, finish)
    } else {
      window.setTimeout(finish, DRAW_ANIMATION_MS + 40)
    }
    return
  }

  window.requestAnimationFrame(() => {
    element.style.transition = `left ${DRAW_ANIMATION_MS}ms cubic-bezier(.2,.8,.2,1), top ${DRAW_ANIMATION_MS}ms cubic-bezier(.2,.8,.2,1), width ${DRAW_ANIMATION_MS}ms cubic-bezier(.2,.8,.2,1), height ${DRAW_ANIMATION_MS}ms cubic-bezier(.2,.8,.2,1), opacity ${DRAW_ANIMATION_MS}ms`
    Object.assign(element.style, drawAnimationFrame(endRect, .98))
    window.setTimeout(finish, DRAW_ANIMATION_MS + 40)
  })
}

function createDrawAnimationElement(cardId, rect) {
  const element = document.createElement('div')
  element.dataset.drawCardAnimation = 'true'
  element.dataset.cardId = cardId
  Object.assign(element.style, {
    ...drawAnimationFrame(rect, .92),
    position: 'fixed',
    zIndex: '180',
    display: 'grid',
    pointerEvents: 'none',
    border: '1px solid rgba(255,255,255,.34)',
    borderRadius: '8px',
    overflow: 'hidden',
    boxShadow: '0 24px 44px rgba(0,0,0,.4)'
  })

  const back = document.createElement('div')
  Object.assign(back.style, {
    width: '100%',
    height: '100%',
    borderRadius: '7px',
    backgroundColor: '#223c4c',
    backgroundImage: "url('/tabletime-playing-card-back.png')",
    backgroundSize: 'cover',
    backgroundPosition: 'center'
  })
  element.append(back)
  return element
}

function drawAnimationFrame(rect, opacity) {
  return {
    left: `${rect.left}px`,
    top: `${rect.top}px`,
    width: `${rect.width}px`,
    height: `${rect.height}px`,
    opacity: `${opacity}`
  }
}

function pruneMissingSelections() {
  if (state.selectedHandCardId && !selectedHandCard()) state.selectedHandCardId = null
  if (state.selectedTableCardId && !selectedTableCard()) state.selectedTableCardId = null
  if (state.selectedDiscardCardId && !selectedDiscardCard()) state.selectedDiscardCardId = null
  if (state.selectedChipId && !chipById(state.selectedChipId)) state.selectedChipId = null
  if (state.selectedPieceId && !selectedPiece()) state.selectedPieceId = null
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
  removeGameParamFromUrl(params)
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

function changeGame(nextGameId) {
  const config = getGameConfig(nextGameId)
  if (state.gameId === config.id || roomCommandUnavailable()) return
  submitCommand(createCommand(COMMAND.GAME_CHANGE, { gameId: config.id }))
}

function removeGameParamFromUrl(params) {
  if (!params.has('game') || s.is.server) return
  params.delete('game')
  const query = params.toString()
  const nextUrl = `${window.location.pathname}${query ? `?${query}` : ''}${window.location.hash}`
  window.history.replaceState(window.history.state, '', nextUrl)
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
  if (state.selectedDiscardCardId) return { kind: 'discard-card', id: state.selectedDiscardCardId }
  if (state.selectedChipId) return { kind: 'chip', id: state.selectedChipId }
  if (state.selectedPieceId) return { kind: 'piece', id: state.selectedPieceId }
  return null
}

function dragPresence() {
  if (!dragState) return null
  if (dragState.returning && dragState.type !== CARD_DRAG_TYPE.HAND) return null

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

  if (dragState.type === PIECE_DRAG_TYPE) {
    const piece = pieceById(dragState.pieceId)
    return { ...base, kind: 'piece', id: dragState.pieceId, x: piece?.x, y: piece?.y }
  }

  if (dragState.type === CARD_DRAG_TYPE.TABLE) {
    const piece = tableCardById(dragState.pieceId)
    return { ...base, kind: 'table-card', id: dragState.pieceId, x: piece?.x, y: piece?.y }
  }

  if (dragState.type === CARD_DRAG_TYPE.HAND) {
    return { ...base, kind: 'hand-card', returning: dragState.returning ? true : undefined }
  }

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

    if (dragState.type === PIECE_DRAG_TYPE) {
      const piece = pieceById(dragState.pieceId)
      if (!piece || piece.locked) return

      dragState.clientX = event.clientX
      dragState.clientY = event.clientY
      if (dragState.fromSupply) {
        moveDragGhost(event.clientX, event.clientY)
      } else {
        piece.x = event.clientX - rect.left - dragState.offsetX
        piece.y = event.clientY - rect.top - dragState.offsetY
      }
    }

    scheduleRedraw()
    sendPresence(pointer)
  })

  window.addEventListener('pointerup', event => {
    if (!dragState) return
    const currentDrag = dragState
    const pointer = tablePointFromEvent(event)
    if (pointer) lastPointer = pointer

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

    if (currentDrag.type === PIECE_DRAG_TYPE) {
      if (!currentDrag.moved) {
        selectPiece(currentDrag.pieceId)
      } else {
        finishPieceDrop(event, currentDrag)
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
  if (!canManipulateComponents()) return
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
  if (!canManipulateComponents()) return
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
  if (!canManipulateComponents()) return
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

  const source = event.target.closest?.('[data-hand-card="true"]') || event.currentTarget
  const rect = source.getBoundingClientRect()
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
  createDragGhost(source, dragState)
  sendPresence(tablePointFromEvent(event), { force: true })
}

function startDiscardCardDrag(event, card) {
  if (!canManipulateComponents()) return
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
  if (!canManipulateComponents()) return
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

function startPieceDrag(event, piece, { fromSupply = false } = {}) {
  if (!canManipulateComponents() || !canManipulatePiece(piece)) return
  event.preventDefault()
  event.stopPropagation()
  selectPiece(piece.id)

  if (piece.locked) {
    scheduleRedraw()
    return
  }

  const source = event.currentTarget
  const rect = source.getBoundingClientRect()
  const originRect = rectFromDomRect(rect)
  const size = pieceSize(piece)
  dragState = {
    type: PIECE_DRAG_TYPE,
    pieceId: piece.id,
    fromSupply,
    startX: event.clientX,
    startY: event.clientY,
    clientX: event.clientX,
    clientY: event.clientY,
    moved: false,
    offsetX: Math.min(event.clientX - rect.left, size.width),
    offsetY: Math.min(event.clientY - rect.top, size.height),
    width: size.width,
    height: size.height,
    originRect,
    originX: piece.x,
    originY: piece.y,
    originInSupply: piece.inSupply
  }

  if (fromSupply) createDragGhost(source, dragState)
  sendPresence(tablePointFromEvent(event), { force: true })
}

function finishHandCardDrop(event, drag) {
  const zoneId = dropZoneFromPoint(event)
  const card = localHandCards().find(item => item.id === drag.cardId)
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

  addLog(`${cardLabel(card)} returned to hand.`)
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

  addLog(`${cardLabel(card)} returned to discard.`)
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

function finishPieceDrop(event, drag) {
  const piece = pieceById(drag.pieceId)
  if (!piece) return

  const point = tablePointFromEvent(event)
  if (!point) {
    returnPieceDragToOrigin(piece, drag)
    return
  }

  const raw = {
    x: point.x - drag.offsetX,
    y: point.y - drag.offsetY
  }
  const clamped = clampPiecePosition(raw.x, raw.y, drag.width, drag.height)
  piece.x = clamped.x
  piece.y = clamped.y

  const result = submitCommand(createCommand(COMMAND.PIECE_MOVE, {
    pieceId: piece.id,
    x: clamped.x,
    y: clamped.y,
    width: drag.width,
    height: drag.height
  }))

  if (!result.ok) {
    returnPieceDragToOrigin(piece, drag)
    return
  }

  state.selectedPieceId = piece.id
  removeDragGhost()

  if (positionWasClamped(raw, clamped)) {
    animateDraggedPieceToTablePosition(drag, clamped)
  }
}

function returnPieceDragToOrigin(piece, drag) {
  if (drag.fromSupply) {
    returnDragGhostToOrigin(drag)
    return
  }

  piece.x = drag.originX
  piece.y = drag.originY
  piece.inSupply = drag.originInSupply
  animateDraggedPieceToTablePosition(drag, {
    x: drag.originX ?? TABLE_PIECE_INSET,
    y: drag.originY ?? TABLE_PIECE_INSET
  })
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
  if (drag.type === CARD_DRAG_TYPE.HAND) sendPresence(lastPointer, { force: true })
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

function rectFromDomRect(rect) {
  return {
    left: rect.left,
    top: rect.top,
    width: rect.width,
    height: rect.height
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

function pieceLeft(piece) {
  if (dragState?.type === PIECE_DRAG_TYPE && dragState.pieceId === piece.id) {
    if (dragState.returning) return dragState.snapLeft
    if (!dragState.fromSupply) return dragState.clientX - dragState.offsetX
  }

  return piece.x ?? 0
}

function pieceTop(piece) {
  if (dragState?.type === PIECE_DRAG_TYPE && dragState.pieceId === piece.id) {
    if (dragState.returning) return dragState.snapTop
    if (!dragState.fromSupply) return dragState.clientY - dragState.offsetY
  }

  return piece.y ?? 0
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
  if (!canManipulateComponents() && ['f', 'r', 'l', 'backspace'].includes(key)) return

  const piece = selectedTableCard()
  const gamePiece = selectedPiece()

  if (key === 'f' && gamePiece) {
    submitCommand(createCommand(COMMAND.PIECE_FLIP, { pieceId: gamePiece.id }))
  } else if (key === 'f' && piece) {
    submitCommand(createCommand(COMMAND.CARD_FLIP, { pieceId: piece.id }))
  }

  if (key === 'r' && gamePiece) {
    submitCommand(createCommand(COMMAND.PIECE_ROTATE, { pieceId: gamePiece.id }))
  } else if (key === 'r' && piece) {
    submitCommand(createCommand(COMMAND.CARD_ROTATE, { pieceId: piece.id }))
  }

  if (key === 'l') {
    if (gamePiece) {
      submitCommand(createCommand(COMMAND.PIECE_LOCK, { pieceId: gamePiece.id }))
    } else if (piece) {
      submitCommand(createCommand(COMMAND.CARD_LOCK, { pieceId: piece.id }))
    } else if (state.selectedChipId) {
      const chip = chipById(state.selectedChipId)
      if (!chip) return
      submitCommand(createCommand(COMMAND.CHIP_LOCK, { chipId: chip.id }))
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

  return AppShell({
    style: `grid-template-rows: 58px minmax(0, 1fr) ${featureEnabled('hands') ? '190px' : '0px'}`
  },
    topBar(),
    MainContent(
      sidePanel(),
      Workspace(
        TableSurface({
          'data-table-surface': 'true',
          'data-drop-zone': ZONE.TABLE,
          style: `--table-width: ${TABLE_WIDTH}px; --table-height: ${TABLE_HEIGHT}px`,
          dom: attachPointerListeners,
          onpointerdown: clearPieceSelectionFromTable
        },
          state.objects.map(object => tableObject({ key: object.id, object })),
          featureEnabled('remoteHands') && remoteHands(),
          featureEnabled('cards') && state.tableCards.map(piece => tableCard({ key: piece.id, piece })),
          featureEnabled('chips') && state.chips.map(chip => tableChip({ key: chip.id, chip })),
          featureEnabled('polyominoes') && tablePieces(),
          featureEnabled('remoteHands') && remoteHandDrags(),
          presenceLayer()
        )
      )
    ),
    featureEnabled('hands') && handBar(),
    loginRequired && loginOverlay()
  )
})

const topBar = s(() =>
  TopBar(
    Brand(
      Mark(),
      TitleBlock(
        Title('Tabletime'),
        Subtitle(state.gameSubtitle || currentGameConfig().subtitle)
      )
    ),
    StatusStrip(
      Object.values(gameConfigs).map(config =>
        SecondaryButton({
          key: config.id,
          disabled: state.gameId === config.id ? true : undefined,
          title: `Open ${config.name}`,
          onclick: () => changeGame(config.id)
        }, config.name)
      ),
      Pill(currentGameConfig().name),
      Pill(`${networkStatus} / ${roomId}`),
      Pill(`${occupiedSeatCount()} / ${seatsForDisplay().length} seated`),
      featureEnabled('deck') && Pill(`${state.drawPile.length} deck`),
      featureEnabled('chips') && Pill(`${state.chips.length} chips`),
      featureEnabled('cards') && Pill(`${state.tableCards.length} loose cards`),
      featureEnabled('polyominoes') && Pill(`${state.pieces.length} pieces`),
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
  const canManipulate = canManipulateComponents()

  return TableObjectShell`
    left ${objectLeft(object) + 'px'}
    top ${objectTop(object) + 'px'}
    z-index ${localDragging ? 60 : remoteDragging ? 55 : 2}
    transition ${remoteDragging ? 'left 45ms linear, top 45ms linear' : 'none'}
  `({
    'data-table-object': 'true',
    'data-drop-zone': object.type === 'discard' ? ZONE.DISCARD : undefined
  },
    ObjectHeader`
      cursor ${object.locked || !canManipulate ? 'default' : localDragging ? 'grabbing' : 'grab'}
    `({
      onpointerdown: event => startObjectDrag(event, object)
    },
      object.title,
      LockBadge(object.locked ? 'locked' : 'movable')
    ),
    tableObjectContent({ object })
  )
})

const tableObjectContent = s(({ object }) => {
  if (object.type === 'deck') return deckObject()
  if (object.type === 'discard') return discardObject()
  if (object.type === 'supply') return supplyObject()

  const customObjectComponent = getObjectComponent(object.type)
  return customObjectComponent ? customObjectComponent({ state, object }) : null
})

const tableChip = s(({ chip }) => {
  const localDragging = dragState?.type === 'chip' && dragState.chipId === chip.id
  const remoteDragging = isRemoteDragging('chip', chip.id)
  const canManipulate = canManipulateComponents()

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
    'data-locked': chip.locked || !canManipulate ? 'true' : 'false',
    disabled: !canManipulate ? true : undefined,
    onpointerdown: event => startChipDrag(event, chip)
  })
})

const tablePieces = s(() =>
  state.pieces
    .filter(piece => piece.kind === 'polyomino')
    .map(piece => tablePiece({ key: piece.id, piece }))
)

const tablePiece = s(({ piece }) => {
  const size = pieceSize(piece)
  const localDragging = dragState?.type === PIECE_DRAG_TYPE && dragState.pieceId === piece.id && !dragState.fromSupply
  const canManipulate = canManipulatePiece(piece)
  const restingTransition = 'width 180ms cubic-bezier(.2,.8,.2,1), height 180ms cubic-bezier(.2,.8,.2,1)'

  return PolyominoPieceButton`
    position ${localDragging ? 'fixed' : 'absolute'}
    left ${pieceLeft(piece) + 'px'}
    top ${pieceTop(piece) + 'px'}
    z-index ${localDragging ? 60 : 7}
    transition ${localDragging && dragState.returning ? SNAP_BACK_TRANSITION : restingTransition}
    --piece-width ${size.width + 'px'}
    --piece-height ${size.height + 'px'}
  `({
    'aria-label': `${pieceLabel(piece)} piece`,
    'data-piece-id': piece.id,
    'data-selected': state.selectedPieceId === piece.id ? 'true' : 'false',
    'data-locked': piece.locked || !canManipulate ? 'true' : 'false',
    disabled: !canManipulate ? true : undefined,
    onpointerdown: event => startPieceDrag(event, piece)
  },
    polyominoVisual({ piece })
  )
})

const tableCard = s(({ piece }) => {
  const size = cardPieceSize(piece)
  const localDragging = dragState?.type === CARD_DRAG_TYPE.TABLE && dragState.pieceId === piece.id
  const remoteDragging = isRemoteDragging('table-card', piece.id)
  const canManipulate = canManipulateComponents()

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
    'data-locked': piece.locked || !canManipulate ? 'true' : 'false',
    disabled: !canManipulate ? true : undefined,
    onpointerdown: event => startTableCardDrag(event, piece)
  },
    cardVisual({ card: piece.card, faceUp: piece.faceUp })
  )
})

const deckObject = s(() => {
  const reason = drawUnavailableReason()
  const shuffleReason = manipulationUnavailableReason()

  return DeckBody(
    DeckStack({
      'data-draw-stack': 'true'
    },
      StackCount(state.drawPile.length)
    ),
    ObjectActions(
      MiniButton({
        'data-no-drag': 'true',
        disabled: reason ? true : undefined,
        title: reason || 'Draw a card',
        onclick: drawOne
      }, 'Draw'),
      MiniButton({
        'data-no-drag': 'true',
        disabled: shuffleReason ? true : undefined,
        title: shuffleReason || 'Shuffle draw deck',
        onclick: shuffleDrawPile
      }, 'Shuffle')
    )
  )
})

const discardObject = s(() => {
  const top = state.discardPile[state.discardPile.length - 1]

  return DiscardBody({
    'data-drop-zone': ZONE.DISCARD,
    'data-drop-ready': zoneAcceptsCardDrop(ZONE.DISCARD, dragState?.type) ? 'true' : 'false'
  },
    top ? compactCard({
      card: top,
      selected: state.selectedDiscardCardId === top.id,
      disabled: !canManipulateComponents(),
      onpointerdown: event => startDiscardCardDrag(event, top)
    }) : DeckStack`background rgba(255,255,255,.05); box-shadow none`('Empty'),
    Pill(`${state.discardPile.length} discarded`)
  )
})

const supplyObject = s(() =>
  SupplyBody(
    playersForCurrentGame().map(player =>
      SupplyRow({
        key: player.id,
        'data-no-drag': 'true'
      },
        SupplyChip({
          'data-disabled': !canManipulateComponents() ? 'true' : 'false',
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

const remoteHands = s(() =>
  RemoteHandsLayer(
    remoteHandSeats().map(({ seat, position }) => remoteHandZone({
      key: seat.playerId,
      seat,
      position
    }))
  )
)

function remoteHandSeats() {
  const seats = seatsForDisplay()
  const occupied = seats.filter(seat => seat.clientId)
  const mine = localSeat()
  const others = mine ? seatsAfter(mine.playerId, seats).filter(seat => seat.clientId) : occupied
  const positions = handPositionsForCount(others.length)

  return others.slice(0, positions.length).map((seat, index) => ({
    seat,
    position: positions[index]
  }))
}

function seatsAfter(playerId, seats) {
  const index = seats.findIndex(seat => seat.playerId === playerId)
  if (index < 0) return seats
  return [...seats.slice(index + 1), ...seats.slice(0, index)]
}

function handPositionsForCount(count) {
  if (count <= 0) return []
  if (count === 1) return ['top']
  if (count === 2) return ['left', 'right']
  return ['left', 'top', 'right']
}

const remoteHandZone = s(({ seat, position }) => {
  const count = handCountForPlayer(seat.playerId)
  const displayCount = remoteHandDragForPlayer(seat.playerId) ? Math.max(0, count - 1) : count
  const visibleBacks = Math.min(displayCount, 7)

  return RemoteHandZone({
    'data-position': position,
    'data-remote-hand-player-id': seat.playerId
  },
    RemoteHandLabel({
      style: `--seat-color: ${seat.color}`
    }, seat.clientName || seat.playerName),
    RemoteHandCards({
      'data-remote-hand-cards': 'true'
    },
      Array.from({ length: visibleBacks }, (_, index) => RemoteHandBack({
        key: `${seat.playerId}-${index}`,
        'data-remote-hand-back': 'true'
      })),
      displayCount > visibleBacks ? Pill(`+${displayCount - visibleBacks}`) : null
    )
  )
})

const remoteHandDrags = s(() =>
  remoteHandDragPresences().map(presence => {
    const drag = presence.drag
    const position = remoteHandDragPosition(presence)
    if (!position) return null

    return RemoteHandDragCard`
      left ${position.x + 'px'}
      top ${position.y + 'px'}
      width ${(drag.width || CARD_PORTRAIT_WIDTH) + 'px'}
      height ${(drag.height || CARD_PORTRAIT_HEIGHT) + 'px'}
      --presence-color ${presence.color || seatForClient(presence.clientId)?.color || '#f1d28a'}
    `({
      key: presence.clientId,
      'data-returning': drag.returning ? 'true' : 'false'
    },
      CardBack()
    )
  })
)

function remoteHandDragPresences() {
  const now = Date.now()

  return Array.from(remotePresence.values()).filter(presence =>
    presence.pointer &&
    presence.drag?.kind === 'hand-card' &&
    seatForClient(presence.clientId) &&
    now - presence.updatedAt <= 1500
  )
}

function remoteHandDragForPlayer(playerId) {
  return remoteHandDragPresences().find(presence => presence.playerId === playerId)
}

function remoteHandDragPosition(presence) {
  const drag = presence.drag
  const current = {
    x: presence.pointer.x - (drag.offsetX || 0),
    y: presence.pointer.y - (drag.offsetY || 0)
  }

  if (!drag.returning) return current

  return remoteHandReturnPosition(presence, drag) || current
}

function remoteHandReturnPosition(presence, drag) {
  const seat = seatForClient(presence.clientId)
  if (!seat) return null

  const surface = document.querySelector('[data-table-surface="true"]')
  const zone = Array.from(document.querySelectorAll('[data-remote-hand-player-id]'))
    .find(element => element.dataset.remoteHandPlayerId === seat.playerId)
  if (!surface || !zone) return null

  const target = zone.querySelector('[data-remote-hand-back]') ||
    zone.querySelector('[data-remote-hand-cards]') ||
    zone
  const surfaceRect = surface.getBoundingClientRect()
  const targetRect = target.getBoundingClientRect()
  const width = drag.width || CARD_PORTRAIT_WIDTH
  const height = drag.height || CARD_PORTRAIT_HEIGHT

  return {
    x: targetRect.left - surfaceRect.left + (targetRect.width - width) / 2,
    y: targetRect.top - surfaceRect.top + (targetRect.height - height) / 2
  }
}

const polyominoVisual = s(({ piece }) => {
  const cellSize = pieceCellSize()
  return PolyominoShape({
    style: `--piece-cell-size: ${cellSize}px; --piece-color: ${playerById(piece.playerId).color}`
  },
    transformedPieceCells(piece).map((cell, index) => PolyominoCell({
      key: `${piece.id}-${index}`,
      style: `--cell-x: ${cell.x}; --cell-y: ${cell.y}`
    }))
  )
})

function canManipulatePiece(piece) {
  const seat = localSeat()
  return Boolean(seat && piece?.playerId === seat.playerId)
}

function pieceLabel(piece) {
  return `${playerById(piece.playerId).name} ${piece.name || 'piece'}`
}

function pieceCellSize() {
  return Number(state.polyominoes?.cellSize) || 24
}

function pieceSize(piece) {
  const cells = transformedPieceCells(piece)
  const maxX = Math.max(0, ...cells.map(cell => cell.x))
  const maxY = Math.max(0, ...cells.map(cell => cell.y))
  const cellSize = pieceCellSize()
  return {
    width: (maxX + 1) * cellSize,
    height: (maxY + 1) * cellSize
  }
}

function transformedPieceCells(piece) {
  const rotation = normalizedQuarterTurn(piece.rotation)
  const raw = (piece.cells || []).map(cell => {
    let x = Number(cell.x) || 0
    let y = Number(cell.y) || 0
    if (piece.flipped) x = -x

    for (let index = 0; index < rotation; index++) {
      ;[x, y] = [-y, x]
    }

    return { x, y }
  })

  const minX = Math.min(0, ...raw.map(cell => cell.x))
  const minY = Math.min(0, ...raw.map(cell => cell.y))
  return raw.map(cell => ({ x: cell.x - minX, y: cell.y - minY }))
}

function normalizedQuarterTurn(value) {
  const turn = Number(value) || 0
  return ((turn % 4) + 4) % 4
}

const compactCard = s(({ card, selected = false, disabled = false, onpointerdown }) =>
  DiscardCardButton({
    selected,
    disabled: disabled ? true : undefined,
    onpointerdown
  },
    cardVisual({ card, faceUp: card.faceUp !== false })
  )
)

const cardVisual = s(({ card, faceUp = true }) =>
  faceUp
    ? HandCardInner(
      CornerText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(suitGlyph(card.suit))),
      CenterRank({ style: `color: ${suitMeta(card.suit).color}` }, card.rank),
      BottomText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(suitGlyph(card.suit)))
    )
    : CardBack()
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
          SeatRow({
            key: seat.playerId,
            'data-seat-row': seat.playerId
          },
            SeatSwatch({ style: `--seat-color: ${seat.color}` }),
            SeatText(
              SeatName(seat.playerName),
              SeatOccupant(seat.clientName || 'Open')
            ),
            MiniButton({
              'data-seat-action': seat.playerId,
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
      LogList({
        dom: autoscrollLog
      },
        state.log.map((item, index) => LogItem({ key: `${index}-${item}` }, item))
      )
    )
  )
)

const handBar = s(() => {
  const seat = localSeat()
  const cards = localHandCards()
  const selected = selectedHandCard()

  return HandBar({
    'data-drop-zone': ZONE.HAND,
    'data-drop-ready': zoneAcceptsCardDrop(ZONE.HAND, dragState?.type) ? 'true' : 'false'
  },
    HandStatus(
      HandIdentity(
        seat ? HandSeatSwatch({ style: `--seat-color: ${seat.color}` }) : null,
        TitleBlock(
          Title(seat ? `${seat.clientName || localPlayerName} Hand` : 'Spectator Hand'),
          Subtitle(selected
            ? `${cardLabel(selected)} selected`
            : seat
              ? state.started ? `${cards.length} cards` : `${seat.playerName} seat waiting`
              : 'Join a seat before start')
        )
      )
    ),
    HandCards({
      'data-hand-cards': 'true'
    },
      cards.map((card, index) => handCard({ key: card.id, card, index, total: cards.length }))
    )
  )
})

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
    'data-hand-card': 'true',
    'data-hand-card-id': card.id,
    selected: state.selectedHandCardId === card.id,
    'data-dragging': dragState?.type === CARD_DRAG_TYPE.HAND && dragState.cardId === card.id && dragState.moved ? 'true' : 'false',
    'data-entering-draw': drawAnimatingCardIds.has(card.id) ? 'true' : 'false',
    onpointerdown: event => startHandCardDrag(event, card)
  },
    HandCardInner(
      CornerText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(suitGlyph(card.suit))),
      CenterRank({ style: `color: ${suitMeta(card.suit).color}` }, card.rank),
      BottomText({ style: `color: ${suitMeta(card.suit).color}` }, card.rank, CardSuit(suitGlyph(card.suit)))
    )
  )
})

function handMetrics() {
  const width = s.is.server ? 1200 : window.innerWidth

  if (width < 560) return { width: 62, height: 86.8, spacing: 34, arcLift: 14, fanDegrees: 20 }
  if (width < 820) return { width: 88, height: 123.2, spacing: 55, arcLift: 20, fanDegrees: 24 }
  return { width: CARD_PORTRAIT_WIDTH, height: CARD_PORTRAIT_HEIGHT, spacing: 74, arcLift: 26, fanDegrees: 26 }
}

function cardPieceSize(piece) {
  return piece.orientation === 'landscape'
    ? { width: CARD_LANDSCAPE_WIDTH, height: CARD_LANDSCAPE_HEIGHT }
    : { width: CARD_PORTRAIT_WIDTH, height: CARD_PORTRAIT_HEIGHT }
}

s.mount(() => App())
