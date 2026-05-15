import crypto from 'node:crypto'

import { applyCommand } from '../game/commands.js'
import { cloneGameState, createInitialGameState, players } from '../game/setup.js'

const rooms = new Map()

export function getRoom(roomId) {
  if (!rooms.has(roomId)) {
    rooms.set(roomId, {
      id: roomId,
      revision: 0,
      state: createInitialGameState(),
      clients: new Map()
    })
  }

  return rooms.get(roomId)
}

export function connectClient(ws) {
  const room = getRoom(ws.roomId)
  const client = {
    id: ws.clientId,
    playerId: ws.playerId,
    playerName: ws.playerName,
    color: playerColor(ws.playerId),
    ws
  }

  room.clients.set(client.id, client)
  sendWelcome(room, client)
  broadcastPresence(room, client, {
    status: 'joined',
    pointer: null,
    selection: null
  })
}

export function disconnectClient(ws) {
  const room = getRoom(ws.roomId)
  const client = room.clients.get(ws.clientId)
  if (!client) return

  room.clients.delete(client.id)
  broadcastPresence(room, client, {
    status: 'left',
    pointer: null,
    selection: null
  })
}

export function handleRoomMessage(ws, message) {
  const room = getRoom(ws.roomId)
  const client = room.clients.get(ws.clientId)
  if (!client || !message?.type) return

  if (message.type === 'command') {
    const result = applyCommand(room.state, message.command)

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

  const clientId = r.query.get('clientId') || crypto.randomUUID()
  const roomId = decodeURIComponent(r.url.replace(/^\/ws\/rooms\/?/, '').split('/')[0]) || 'sequence'
  const room = getRoom(roomId)
  const playerName = r.query.get('playerName') || `Player ${room.clients.size + 1}`
  const playerId = players[room.clients.size % players.length]?.id || players[0].id

  return {
    roomId: room.id,
    clientId,
    playerId,
    playerName
  }
}

function sendWelcome(room, client) {
  send(client.ws, {
    type: 'welcome',
    roomId: room.id,
    clientId: client.id,
    playerId: client.playerId,
    playerName: client.playerName,
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
  for (const other of room.clients.values()) {
    if (other.id === client.id) continue
    send(other.ws, {
      type: 'presence',
      roomId: room.id,
      clientId: client.id,
      playerId: client.playerId,
      playerName: client.playerName,
      color: client.color,
      presence
    })
  }
}

function snapshotForClient(room) {
  return cloneGameState(room.state)
}

function send(ws, message) {
  if (!ws.open) return
  ws.send(JSON.stringify(message))
}

function playerColor(playerId) {
  return players.find(player => player.id === playerId)?.color || players[0].color
}
