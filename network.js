export function createMultiplayerClient({
  roomId,
  username,
  secret,
  onStatus,
  onWelcome,
  onSnapshot,
  onPresence,
  onRejected,
  onAuthError
}) {
  let socket = null
  let connected = false
  let clientId = null
  let playerId = null
  let revision = 0
  let reconnectTimer = null
  let commandCounter = 1
  let closed = false
  const pendingCommands = new Set()

  connect()

  return {
    get connected() { return connected },
    get clientId() { return clientId },
    get playerId() { return playerId },
    get revision() { return revision },
    sendCommand,
    sendPresence,
    close
  }

  function connect() {
    if (typeof window === 'undefined') return

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
    const params = new URLSearchParams()
    if (username) params.set('username', username)
    if (secret) params.set('secret', secret)

    socket = new WebSocket(`${protocol}//${window.location.host}/ws/rooms/${encodeURIComponent(roomId)}?${params}`)
    onStatus?.('connecting')

    socket.addEventListener('open', () => {
      onStatus?.('authenticating')
    })

    socket.addEventListener('message', event => {
      const message = parseMessage(event.data)
      if (!message) return

      if (message.type === 'welcome') {
        clientId = message.clientId
        playerId = message.playerId
        username = message.username || username
        secret = message.secret || secret
        revision = message.revision || 0
        connected = true
        onStatus?.('connected')
        onWelcome?.(message)
        onSnapshot?.(message.snapshot, message)
      }

      if (message.type === 'snapshot') {
        revision = message.revision || revision
        if (message.commandId) pendingCommands.delete(message.commandId)
        onSnapshot?.(message.snapshot, message)
      }

      if (message.type === 'presence') {
        onPresence?.(message)
      }

      if (message.type === 'rejected') {
        if (message.commandId) pendingCommands.delete(message.commandId)
        onRejected?.(message)
      }

      if (message.type === 'auth_error') {
        closed = true
        connected = false
        onAuthError?.(message)
        socket.close()
      }
    })

    socket.addEventListener('close', () => {
      connected = false
      if (closed) return
      onStatus?.('disconnected')
      reconnectTimer = window.setTimeout(connect, 900)
    })

    socket.addEventListener('error', () => {
      onStatus?.('disconnected')
    })
  }

  function sendCommand(command) {
    const commandId = `${clientId || 'local'}-${commandCounter++}`
    pendingCommands.add(commandId)

    return send({
      type: 'command',
      commandId,
      baseRevision: revision,
      command
    })
  }

  function sendPresence(presence) {
    return send({
      type: 'presence',
      presence
    })
  }

  function send(message) {
    if (!socket || socket.readyState !== WebSocket.OPEN) return false
    socket.send(JSON.stringify(message))
    return true
  }

  function close() {
    closed = true
    if (reconnectTimer) window.clearTimeout(reconnectTimer)
    reconnectTimer = null
    socket?.close()
  }
}

function parseMessage(data) {
  try {
    return JSON.parse(data)
  } catch {
    return null
  }
}
