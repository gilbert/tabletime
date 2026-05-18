import crypto from 'node:crypto'

import { applyCommand } from '../game/commands.js'
import { cloneGameState, createInitialGameState, normalizeGameState, players } from '../game/setup.js'

const rooms = new Map()

export function getRoom(roomId) {
  if (!rooms.has(roomId)) {
    rooms.set(roomId, {
      id: roomId,
      revision: 0,
      state: createInitialGameState(),
      clients: new Map(),
      users: new Map(),
      pendingLeaves: new Map()
    })
  }

  return rooms.get(roomId)
}

export function connectClient(ws) {
  const room = getRoom(ws.roomId)
  if (ws.authError) {
    send(ws, {
      type: 'auth_error',
      reason: ws.authError
    })
    return
  }

  const client = {
    connectionId: ws.connectionId,
    id: ws.clientId,
    playerId: ws.playerId,
    playerName: ws.username,
    color: playerColor(ws.playerId),
    secret: ws.secret,
    ws
  }

  const wasConnected = roomHasClient(room, client.id)
  const pendingLeave = room.pendingLeaves.get(client.id)
  if (pendingLeave) {
    clearTimeout(pendingLeave)
    room.pendingLeaves.delete(client.id)
  }

  room.clients.set(client.connectionId, client)
  const user = room.users.get(client.playerName)
  if (!wasConnected && !user?.present && !pendingLeave) {
    appendLog(room, `${client.playerName} joined the room.`)
    room.revision++
  }
  if (user) user.present = true

  refreshClientSeatNames(room, client)
  sendWelcome(room, client)
  if (!wasConnected && !pendingLeave) {
    broadcastSnapshot(room, {
      actorId: client.id,
      message: `${client.playerName} joined the room.`
    })
  }
}

export function disconnectClient(ws) {
  const room = getRoom(ws.roomId)
  const client = room.clients.get(ws.connectionId)
  if (!client) return

  room.clients.delete(client.connectionId)
  if (roomHasClient(room, client.id)) return

  const timeout = setTimeout(() => {
    room.pendingLeaves.delete(client.id)
    if (roomHasClient(room, client.id)) return

    const user = room.users.get(client.playerName)
    if (user) user.present = false
    appendLog(room, `${client.playerName} left the room.`)
    room.revision++
    broadcastPresence(room, client, {
      status: 'left',
      pointer: null,
      selection: null
    })
    broadcastSnapshot(room, {
      actorId: client.id,
      message: `${client.playerName} left the room.`
    })
  }, 1000)
  room.pendingLeaves.set(client.id, timeout)
}

export function handleRoomMessage(ws, message) {
  const room = getRoom(ws.roomId)
  const client = room.clients.get(ws.connectionId)
  if (!client || !message?.type) return

  if (message.type === 'command') {
    const result = applyCommand(room.state, message.command, {
      actor: {
        clientId: client.id,
        playerName: client.playerName
      }
    })

    if (!result.ok) {
      send(client.ws, {
        type: 'rejected',
        commandId: message.commandId,
        reason: result.message,
        revision: room.revision,
        snapshot: snapshotForClient(room, client)
      })
      return
    }

    room.revision++
    broadcastSnapshot(room, {
      commandId: message.commandId,
      actorId: client.id,
      message: result.message
    })
  }

  if (message.type === 'presence') {
    broadcastPresence(room, client, message.presence || {})
  }
}

export function makeClientIdentity(r) {
  if (!r.url.startsWith('/ws/rooms/')) {
    r.statusEnd(404)
    return null
  }

  const roomId = decodeURIComponent(r.url.replace(/^\/ws\/rooms\/?/, '').split('/')[0]) || 'sequence'
  const room = getRoom(roomId)
  const username = r.query.get('username') || ''
  const secret = r.query.get('secret') || ''
  const authError = validateUsername(username)
  const user = authError ? null : getOrCreateUser(room, username, secret)

  return {
    roomId: room.id,
    connectionId: crypto.randomUUID(),
    clientId: user?.id || crypto.randomUUID(),
    playerId: user?.playerId || players[0].id,
    username,
    secret: user?.secret || null,
    authError: authError || user?.error || null
  }
}

function sendWelcome(room, client) {
  send(client.ws, {
    type: 'welcome',
    roomId: room.id,
    clientId: client.id,
    playerId: client.playerId,
    playerName: client.playerName,
    username: client.playerName,
    secret: client.secret,
    color: client.color,
    revision: room.revision,
    snapshot: snapshotForClient(room, client)
  })
}

function broadcastSnapshot(room, meta = {}) {
  for (const client of room.clients.values()) {
    send(client.ws, {
      type: 'snapshot',
      roomId: room.id,
      revision: room.revision,
      ...meta,
      snapshot: snapshotForClient(room, client)
    })
  }
}

function broadcastPresence(room, client, presence) {
  if (!clientSeat(room, client)) return

  for (const other of room.clients.values()) {
    if (other.connectionId === client.connectionId) continue
    send(other.ws, {
      type: 'presence',
      roomId: room.id,
      clientId: client.id,
      playerId: client.playerId,
      playerName: client.playerName,
      color: clientPresenceColor(room, client),
      presence
    })
  }
}

function snapshotForClient(room, client = null) {
  normalizeGameState(room.state)
  const snapshot = cloneGameState(room.state)
  const seat = client ? clientSeat(room, client) : null
  const visibleHands = {}

  for (const occupiedSeat of occupiedSeats(room.state)) {
    const hand = Array.isArray(room.state.handsByPlayerId?.[occupiedSeat.playerId])
      ? room.state.handsByPlayerId[occupiedSeat.playerId]
      : []

    visibleHands[occupiedSeat.playerId] = seat?.playerId === occupiedSeat.playerId
      ? cloneGameState(hand)
      : {
        playerId: occupiedSeat.playerId,
        playerName: occupiedSeat.playerName,
        clientName: occupiedSeat.clientName,
        color: occupiedSeat.color,
        count: hand.length,
        hidden: true
      }
  }

  snapshot.handsByPlayerId = visibleHands
  delete snapshot.hand
  return snapshot
}

function send(ws, message) {
  if (!ws.open) return
  ws.send(JSON.stringify(message))
}

function playerColor(playerId) {
  return players.find(player => player.id === playerId)?.color || players[0].color
}

function clientPresenceColor(room, client) {
  return clientSeat(room, client)?.color || client.color
}

function clientSeat(room, client) {
  return room.state.seats?.find(seat => seat.clientId === client.id) || null
}

function occupiedSeats(state) {
  return (state.seats || []).filter(seat => seat.clientId)
}

function roomHasClient(room, clientId) {
  for (const client of room.clients.values()) {
    if (client.id === clientId) return true
  }

  return false
}

function refreshClientSeatNames(room, client) {
  if (!Array.isArray(room.state.seats)) return
  for (const seat of room.state.seats) {
    if (seat.clientId === client.id) seat.clientName = client.playerName
  }
}

function getOrCreateUser(room, username, secret) {
  const existing = room.users.get(username)

  if (existing) {
    if (secret && secret === existing.secret) return existing
    return { error: 'Username is taken.' }
  }

  const user = {
    id: crypto.randomUUID(),
    username,
    playerId: players[room.users.size % players.length]?.id || players[0].id,
    secret: crypto.randomBytes(24).toString('base64url'),
    present: false
  }
  room.users.set(username, user)
  return user
}

function validateUsername(username) {
  if (!username) return 'Username is required.'
  if (username.length > 32) return 'Username must be 32 characters or fewer.'
  if (!/^[a-z]+(?: [a-z]+)*$/.test(username)) {
    return 'Use lowercase letters with single spaces only between words.'
  }

  return null
}

function appendLog(room, message) {
  const maxEntries = Number(room.state.logConfig?.maxEntries) || 100
  room.state.log = [...(room.state.log || []), message].slice(-maxEntries)
}
