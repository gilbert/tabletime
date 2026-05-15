import { connectClient, disconnectClient, handleRoomMessage, makeClientIdentity } from './rooms.js'

export default async function(app) {
  app.get('/hello', r => r.end('Welcome to cofound'))

  app.ws({
    upgrade: makeClientIdentity,
    open: connectClient,
    close: disconnectClient,
    message: (ws, message) => handleRoomMessage(ws, message.json)
  })
}
