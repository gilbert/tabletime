import fs from 'node:fs/promises'
import { createRequire } from 'node:module'
import os from 'node:os'
import path from 'node:path'
import { pathToFileURL } from 'node:url'

import Server from 'cofound/server'
import t from 'cofound/test'
import { Window } from 'happy-dom'

import routes from '../server/index.js'

const root = process.cwd()
const require = createRequire(import.meta.url)
const esbuildPath = require.resolve('esbuild', {
  paths: [path.join(root, 'node_modules/cofound')]
})
const { build } = await import(pathToFileURL(esbuildPath).href)
const bundleDir = path.join(os.tmpdir(), 'tabletime-tests')
const bundlePath = path.join(bundleDir, 'app.mjs')
let bundleReady = null
let importCounter = 1

function assert(condition, message) {
  if (!condition) throw new Error(message)
}

function text(value) {
  return String(value || '').replace(/\s+/g, ' ').trim()
}

async function buildAppBundle() {
  if (bundleReady) return bundleReady

  bundleReady = (async () => {
    await fs.mkdir(bundleDir, { recursive: true })
    await build({
      entryPoints: [path.join(root, 'index.js')],
      outfile: bundlePath,
      bundle: true,
      format: 'esm',
      platform: 'browser',
      sourcemap: false,
      plugins: [{
        name: 'cofound-browser-entry',
        setup(build) {
          build.onResolve({ filter: /^cofound$/ }, () => ({
            path: path.join(root, 'node_modules/cofound/src/index.js')
          }))
        }
      }]
    })
  })()

  return bundleReady
}

async function makeTabletimeTestServer() {
  const app = Server()
  await routes(app)
  const { port, unlisten } = await app.listen(0)
  return {
    port,
    close: () => unlisten()
  }
}

function expose(name, value) {
  Object.defineProperty(globalThis, name, {
    value,
    configurable: true,
    writable: true
  })
}

function bindWindow(window) {
  for (const key of [
    'window',
    'document',
    'location',
    'history',
    'navigator',
    'localStorage',
    'sessionStorage',
    'WebSocket',
    'Event',
    'MouseEvent',
    'PointerEvent',
    'HTMLElement',
    'HTMLFormElement',
    'Element',
    'SVGElement',
    'Node',
    'NodeFilter',
    'DocumentFragment',
    'customElements',
    'MutationObserver'
  ]) {
    if (key in window) expose(key, window[key])
  }

  if (!window.matchMedia) {
    window.matchMedia = () => ({
      matches: false,
      media: '',
      addEventListener() {},
      removeEventListener() {}
    })
  }
  if (!window.scrollTo) window.scrollTo = () => {}
  if (window.HTMLFormElement) {
    window.HTMLFormElement.prototype.hasOwnProperty = function hasOwnProperty(property) {
      if (typeof property === 'symbol') return property in this
      return Object.prototype.hasOwnProperty.call(this, property)
    }
  }

  expose('requestAnimationFrame', window.requestAnimationFrame.bind(window))
  expose('cancelAnimationFrame', window.cancelAnimationFrame.bind(window))
  expose('getComputedStyle', window.getComputedStyle.bind(window))
}

async function createClient(api, room, username, { staleGameId = null } = {}) {
  await buildAppBundle()

  const sockets = new Set()
  const url = new URL(`http://localhost:${api.port}/`)
  url.searchParams.set('room', room)
  if (staleGameId) url.searchParams.set('game', staleGameId)
  const window = new Window({
    url: url.toString(),
    settings: {
      disableJavaScriptFileLoading: true,
      disableJavaScriptEvaluation: false
    }
  })
  const WindowWebSocket = window.WebSocket
  window.WebSocket = class TrackedWebSocket extends WindowWebSocket {
    constructor(...args) {
      super(...args)
      sockets.add(this)
      this.addEventListener('close', () => sockets.delete(this))
    }
  }
  const client = {
    username,
    window,
    get document() {
      return window.document
    },
    activate() {
      bindWindow(window)
    },
    async close() {
      for (const socket of sockets) {
        if (socket.readyState === WindowWebSocket.CONNECTING || socket.readyState === WindowWebSocket.OPEN) {
          socket.close()
        }
      }
      window.happyDOM.cancelAsync()
      window.happyDOM.close()
    }
  }

  window.localStorage.setItem(`tabletime:room:${room}:identity`, JSON.stringify({
    username,
    secret: `test-secret-${username}`
  }))

  client.activate()
  await import(pathToFileURL(bundlePath).href + `?client=${importCounter++}`)
  await waitFor(client, document => {
    const action = document.querySelector('[data-seat-action="red"]')
    return action && !action.disabled
  })
  return client
}

async function waitFor(client, predicate, { timeout = 3000 } = {}) {
  const started = Date.now()
  let lastError = null

  while (Date.now() - started < timeout) {
    client.activate()
    await client.window.happyDOM.waitUntilComplete()
    try {
      const result = predicate(client.document)
      if (result) return result
    } catch (error) {
      lastError = error
    }
    await new Promise(resolve => setTimeout(resolve, 25))
  }

  throw lastError || new Error(`Timed out waiting for ${client.username}`)
}

async function clickSeat(client, playerId) {
  client.activate()
  const button = client.document.querySelector(`[data-seat-action="${playerId}"]`)
  assert(button, `${client.username} could not find ${playerId} seat action`)
  assert(!button.disabled, `${client.username} ${playerId} seat action was disabled`)
  button.click()
  await client.window.happyDOM.waitUntilComplete()
}

async function clickButton(client, label) {
  client.activate()
  const button = Array.from(client.document.querySelectorAll('button'))
    .find(item => text(item.textContent) === label && !item.disabled)
  assert(button, `${client.username} could not find enabled ${label} button`)
  button.click()
  await client.window.happyDOM.waitUntilComplete()
}

function seatText(client, playerId) {
  return text(client.document.querySelector(`[data-seat-row="${playerId}"]`)?.textContent)
}

function enabledSeatActions(client) {
  return Array.from(client.document.querySelectorAll('[data-seat-action]'))
    .filter(button => !button.disabled)
    .map(button => button.dataset.seatAction)
}

function buttonByLabel(client, label) {
  return Array.from(client.document.querySelectorAll('button'))
    .find(item => text(item.textContent) === label)
}

function handCardCount(client) {
  return client.document.querySelectorAll('[data-hand-card="true"]').length
}

function remoteBackCount(client) {
  return client.document.querySelectorAll('[data-remote-hand-back="true"]').length
}

function blokusBoardCellCount(client) {
  return client.document.querySelectorAll('[data-board-kind="blokus"]').length
}

function tablePieceCount(client) {
  return client.document.querySelectorAll('[data-piece-id]').length
}

async function pointerDown(client, selector, { x = 10, y = 10 } = {}) {
  client.activate()
  const element = client.document.querySelector(selector)
  assert(element, `${client.username} could not find ${selector}`)
  element.dispatchEvent(new PointerEvent('pointerdown', {
    bubbles: true,
    cancelable: true,
    pointerId: 1,
    pointerType: 'mouse',
    button: 0,
    buttons: 1,
    clientX: x,
    clientY: y
  }))
  await client.window.happyDOM.waitUntilComplete()
  return element
}

async function pointerMove(client, { x, y }) {
  client.activate()
  client.window.dispatchEvent(new PointerEvent('pointermove', {
    bubbles: true,
    cancelable: true,
    pointerId: 1,
    pointerType: 'mouse',
    button: 0,
    buttons: 1,
    clientX: x,
    clientY: y
  }))
  await client.window.happyDOM.waitUntilComplete()
}

async function pointerUp(client, { x = 10, y = 10 } = {}) {
  client.activate()
  client.window.dispatchEvent(new PointerEvent('pointerup', {
    bubbles: true,
    cancelable: true,
    pointerId: 1,
    pointerType: 'mouse',
    button: 0,
    buttons: 0,
    clientX: x,
    clientY: y
  }))
  await client.window.happyDOM.waitUntilComplete()
}

async function pointerTap(client, selector) {
  await pointerDown(client, selector)
  await pointerUp(client)
}

function pieceIsSelected(client, pieceId) {
  return client.document.querySelector(`[data-piece-id="${pieceId}"]`)?.dataset.selected === 'true'
}

t`server-backed happy-dom UI`({
  timeout: 8000,
  async run(test) {
    const api = await makeTabletimeTestServer()
    try {
      return await test(api)
    } finally {
      await api.close()
    }
  }
},

  t`joins seats through the real websocket server`(async api => {
    const room = `ui-seat-${Date.now()}`
    const alice = await createClient(api, room, 'alice')
    const bob = await createClient(api, room, 'bob')

    try {
      await clickSeat(alice, 'red')
      await waitFor(alice, () => seatText(alice, 'red').includes('alice'))
      await waitFor(bob, () => seatText(bob, 'red').includes('alice'))

      await clickSeat(bob, 'blue')
      await waitFor(alice, () => seatText(alice, 'blue').includes('bob'))
      await waitFor(bob, () => seatText(bob, 'blue').includes('bob'))

      assert(seatText(alice, 'red').includes('(you)'), 'Alice should see her own Red seat as (you).')
      assert(seatText(bob, 'blue').includes('(you)'), 'Bob should see his own Blue seat as (you).')
    } finally {
      await alice.close()
      await bob.close()
    }
  }),

  t`switches games without changing rooms`(async api => {
    const room = `ui-switch-room-${Date.now()}`
    const alice = await createClient(api, room, 'alice')
    const bob = await createClient(api, room, 'bob')

    try {
      await clickButton(alice, 'Blokus')
      await waitFor(alice, () => text(alice.document.body.textContent).includes('Freeform Blokus table'))
      await waitFor(bob, () => text(bob.document.body.textContent).includes('Freeform Blokus table'))
      assert(alice.window.location.search.includes(`room=${room}`), 'Switching games should preserve the room query parameter.')
      assert(!alice.window.location.search.includes('game='), 'Game should not be stored in the URL.')
    } finally {
      await alice.close()
      await bob.close()
    }
  }),

  t`ignores stale game query params`(async api => {
    const room = `ui-stale-game-param-${Date.now()}`
    const alice = await createClient(api, room, 'alice', { staleGameId: 'blokus' })

    try {
      assert(alice.window.location.search.includes(`room=${room}`), 'Stale game URL cleanup should keep the room.')
      assert(!alice.window.location.search.includes('game='), 'Stale game query params should be removed.')
      assert(text(alice.document.body.textContent).includes('Sequence'), 'Rooms should open using room state, not game query params.')
    } finally {
      await alice.close()
    }
  }),

  t`starts with private hands and remote card backs`(async api => {
    const room = `ui-hands-${Date.now()}`
    const alice = await createClient(api, room, 'alice')
    const bob = await createClient(api, room, 'bob')

    try {
      await clickSeat(alice, 'red')
      await clickSeat(bob, 'blue')
      await waitFor(alice, () => seatText(alice, 'blue').includes('bob'))

      assert(handCardCount(alice) === 0, 'Alice should have no hand cards before Start.')
      assert(handCardCount(bob) === 0, 'Bob should have no hand cards before Start.')

      await clickButton(alice, 'Start')
      await waitFor(alice, () => handCardCount(alice) === 7 && remoteBackCount(alice) === 7)
      await waitFor(bob, () => handCardCount(bob) === 7 && remoteBackCount(bob) === 7)

      const handPanelDrawButtons = Array.from(alice.document.querySelectorAll('[data-drop-zone="hand"] button'))
        .filter(button => text(button.textContent) === 'Draw')
      assert(handPanelDrawButtons.length === 0, 'Draw should not be in the hand panel.')
      assert(
        Array.from(alice.document.querySelectorAll('button')).some(button => text(button.textContent) === 'Draw'),
        'Draw should still be available on the deck.'
      )
    } finally {
      await alice.close()
      await bob.close()
    }
  }),

  t`locks late seat joins after start`(async api => {
    const room = `ui-late-join-${Date.now()}`
    const alice = await createClient(api, room, 'alice')
    const bob = await createClient(api, room, 'bob')
    const charlie = await createClient(api, room, 'charlie')

    try {
      await clickSeat(alice, 'red')
      await clickSeat(bob, 'blue')
      await waitFor(alice, () => seatText(alice, 'blue').includes('bob'))
      await clickButton(alice, 'Start')

      await waitFor(charlie, () => text(charlie.document.body.textContent).includes('Started'))
      assert(enabledSeatActions(charlie).length === 0, 'Unseated late clients should not have enabled seat actions.')
      assert(buttonByLabel(charlie, 'Draw')?.disabled, 'Unseated late clients should not be able to draw.')
      assert(buttonByLabel(charlie, 'Shuffle')?.disabled, 'Unseated late clients should not be able to shuffle.')
    } finally {
      await alice.close()
      await bob.close()
      await charlie.close()
    }
  }),

  t`renders blokus without hands`(async api => {
    const room = `ui-blokus-${Date.now()}`
    const alice = await createClient(api, room, 'alice')
    const bob = await createClient(api, room, 'bob')

    try {
      await clickButton(alice, 'Blokus')
      await waitFor(alice, () => text(alice.document.body.textContent).includes('Blokus'))
      await waitFor(bob, () => text(bob.document.body.textContent).includes('Blokus'))
      assert(blokusBoardCellCount(alice) === 400, 'Blokus should render a 20x20 board.')
      assert(alice.document.querySelector('[data-hand-cards]') === null, 'Blokus should not render a hand zone.')
      assert(tablePieceCount(alice) === 84, 'Blokus should render all pieces on the table.')

      await clickSeat(alice, 'blue')
      await waitFor(alice, () => seatText(alice, 'blue').includes('alice'))
      await clickSeat(bob, 'gold')
      await waitFor(alice, () => seatText(alice, 'gold').includes('bob'))

      await clickButton(alice, 'Start')
      await waitFor(alice, () => text(alice.document.body.textContent).includes('Started'))
      assert(handCardCount(alice) === 0, 'Starting Blokus should not deal hand cards.')
      assert(remoteBackCount(alice) === 0, 'Blokus should not render remote hand backs.')
    } finally {
      await alice.close()
      await bob.close()
    }
  }),

  t`keeps blokus piece selection consistent`(async api => {
    const room = `ui-blokus-selection-${Date.now()}`
    const alice = await createClient(api, room, 'alice')

    try {
      await clickButton(alice, 'Blokus')
      await waitFor(alice, () => text(alice.document.body.textContent).includes('Blokus'))
      await clickSeat(alice, 'blue')
      await waitFor(alice, () => seatText(alice, 'blue').includes('alice'))

      await pointerTap(alice, '[data-piece-id="piece-blue-mono"]')
      await waitFor(alice, () => pieceIsSelected(alice, 'piece-blue-mono'))

      await pointerTap(alice, '[data-piece-id="piece-blue-mono"]')
      assert(pieceIsSelected(alice, 'piece-blue-mono'), 'Clicking a selected Blokus piece should keep it selected.')

      await pointerDown(alice, '[data-piece-id="piece-blue-mono"]', { x: 10, y: 10 })
      await pointerMove(alice, { x: 30, y: 30 })
      assert(pieceIsSelected(alice, 'piece-blue-mono'), 'Dragging a selected Blokus piece should keep it selected while dragging.')
      await pointerUp(alice, { x: 30, y: 30 })
      assert(pieceIsSelected(alice, 'piece-blue-mono'), 'Dropping a selected Blokus piece should keep it selected.')

      await pointerDown(alice, '[data-piece-id="piece-blue-domino"]', { x: 10, y: 10 })
      await pointerMove(alice, { x: 30, y: 30 })
      assert(pieceIsSelected(alice, 'piece-blue-domino'), 'Dragging an unselected Blokus piece should select it while dragging.')
      await pointerUp(alice, { x: 30, y: 30 })

      await pointerTap(alice, '[data-table-surface="true"]')
      assert(!pieceIsSelected(alice, 'piece-blue-domino'), 'Clicking the table should deselect the selected Blokus piece.')
    } finally {
      await alice.close()
    }
  })
)
