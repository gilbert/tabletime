// const Game = require('../games/avalon')
// const Game = require('../games/tinytown')
const Game = require('../games/kingmaker')

const ActionBar = require('./ui/action-bar')
const { zoneToSelector } = require('./util')

const PLAYER_HAND_ZONE = /^player\/([^/]+)\/hand/
const TRANSLATE_COORDS = /translate\(([0-9\.\-]+)px, *([0-9\.\-]+)px\)/

let actionBar;

let game, scale;
let app = module.exports
app.game;
app.tableScale = 1
app.currentPlayer = 'p1'

app.sync = sync
app.start = startGame
app.started = false
app.findInHand = findInHand

app.playerNames = {
  p1: 'Player 1',
  p2: 'Player 2',
  p3: 'Player 3',
  p4: 'Player 4',
  p5: 'Player 5',
  p6: 'Player 6',
  p7: 'Player 7',
  p8: 'Player 8',
  p9: 'Player 9',
  p10: 'Player 10',
}

let elementsToMove = []

async function startGame() {
  app.game = (game = Game.createGame())
  if (await Game.testSetup(game)) {
    app.started = true
  }
  else {
    await game.setup()
  }

  $table.innerHTML = await Game.views.table(app)
  $handbar.innerHTML = await Game.views.handBar(app)

  //
  // TODO, SOMEONE: Math here is off, could definitely be more accurate.
  //
  const padding = 100
  scale = app.tableScale = Math.max(
    Math.min(1, (window.innerWidth - padding) / ($tableCanvas.offsetWidth - padding / 2)),
    Math.min(1, (window.innerHeight - padding) / ($tableCanvas.offsetHeight - padding / 2)),
  )

  $table.style.transform = `scale(${scale})`
  $handbar.style.transform = `scale(${scale}) translateY(100%)`

  $tableCanvas.dataset.originalWidth = $tableCanvas.offsetWidth
  $tableCanvas.dataset.originalHeight = $tableCanvas.offsetHeight
  $tableCanvas.style.width = `${$tableCanvas.offsetWidth * scale + padding / 2}px`
  $tableCanvas.style.height = `${$tableCanvas.offsetHeight * scale + padding / 2}px`

  actionBar = ActionBar(app)
  // Put this after the `await` so we don't have a crazy zoom animation on page load
  $handbar.classList.add('initialized')
  sync()
}

async function sync() {
  const state = await game.getState()
  const draggables = await game.getDraggables(app.currentPlayer)
  const peekables = await game.getPeekables(app.currentPlayer)

  state.cards.forEach(card => {
    let domId = objectDomId(card.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
      elem.style.transform = `translate(0px, 0px)`
      elem.className = 'card npc'
      elem.dataset.id = card.id
      elem.dataset.type = card.type
      elem.dataset.name = card.name
      elem.dataset.zone = card.zone
      elem.innerHTML = `
        <div class="card-flipper">
          <div class="card-back"></div>
          <div class="card-front"></div>
        </div>
      `
      addToTable(app, elem, card.zone)
    }
    else {
      queueSyncZone(elem, card.zone)
    }

    elem.dataset.face = card.face
    syncDraggable(draggables, elem, card.id)
    syncPeekable(peekables, elem, card.id)
    syncWithHand(app, elem)
  })

  state.tokens.forEach(token => {
    let domId = objectDomId(token.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
      elem.style.transform = `translate(0px, 0px)`
      elem.className = 'token npc'
      elem.dataset.id = token.id
      elem.dataset.type = token.type
      elem.dataset.zone = token.zone
      addToTable(app, elem, token.zone)
    }
    else {
      queueSyncZone(elem, token.zone)
    }

    syncDraggable(draggables, elem, token.id)
    syncWithHand(app, elem)
  })

  syncZones()

  await actionBar.sync()

  const isHandAvailable = Object.keys(draggables).some(id => findInHand(id))
  $handbar.style.transform = `scale(${scale}) translateY(${isHandAvailable ? '0%' : '100%'})`
}

function addToTable(app, elem, zone) {
  const zoneElem = document.querySelector(zoneToSelector(zone))
  if (zoneElem) {
    zoneElem.appendChild(elem)
  }
  else {
    console.error('[TableTime] Zone not found', zone, 'for item', elem.dataset)
  }
}

function syncWithHand(app, elem) {
  const match = elem.dataset.zone.match(PLAYER_HAND_ZONE)
  // if (!match || match[1] !== app.currentPlayer) return

  const existing = findInHand(elem.dataset.id)

  const oldZone = existing && existing.dataset.zone
  const newZone = elem.dataset.zone

  if (existing && oldZone !== newZone) {
    // $handbar only handles one zone.
    // Therefore, if it's not in this zone, it's not in the handbar anymore.
    // HOWEVER, we still need this elem to be in the DOM for later calculation.
    // Other code will remove this element later.
  }
  else if (existing) {
    // Sync state
    if (elem.dataset.face) {
      existing.dataset.face = elem.dataset.face
    }
    if (elem.dataset.draggable) {
      existing.dataset.draggable = elem.dataset.draggable
    }
    else {
      delete existing.dataset.draggable
    }
  }
  else if (match && match[1] === app.currentPlayer) {
    // Add to hand bar
    const clone = elem.cloneNode(true)
    delete clone.id
    $handbar.querySelector(zoneToSelector(elem.dataset.zone)).appendChild(clone)
  }
}

function queueSyncZone(elem, newZone) {
  const oldZone = elem.dataset.zone
  if (newZone === oldZone) return;

  const handElem = findInHand(elem.dataset.id)
  if (handElem) {
    // Moved element is actually a drag & drop from player's hand!
    // Animate from that position instead.
    // elem.classList.remove('npc')

    // ASSUMPTION: Original element is at translate(0,0)
    // Therefore we only need to take the simple difference.
    const new_x = handElem.dataset.dropX / scale
    const new_y = handElem.dataset.dropY / scale
    // No need to delete dropX/Y as handElem will soon be removed by syncWithHand()

    elementsToMove.push({
      type: 'from-hand',
      elem,
      handElem,
      newParentElem: document.querySelector(zoneToSelector(newZone)),
      new_x,
      new_y,
      // oldTranslate: [null, `${new_x - old_x}px`, `${new_y - old_y}px`]
    })
    // void elem.offsetWidth; // reflow to avoid transition animation
    // elem.style.transform = `translate(${new_x - old_x}px, ${new_y - old_y}px)`
    return
  }
  else {
    // Record position before moving element

    const bbox = elem.getBoundingClientRect()
    console.log(elem.dataset, 'from', bbox)
    const old_x = bbox.x / scale
    const old_y = bbox.y / scale
    const oldTranslate = (elem.style.transform || '').match(TRANSLATE_COORDS) || [null, '0', '0']

    // const newParent = document.querySelector(zoneToSelector(newZone))

    elementsToMove.push({
      type: 'from-table',
      elem,
      newParentElem: document.querySelector(zoneToSelector(newZone)),
      old_x,
      old_y,
      oldTranslate,
    })

    // newParent.appendChild(elem)
    // elem.dataset.zone = newZone
  }
}

function syncZones() {
  elementsToMove.forEach(({ elem }) => {
    elem.classList.remove('npc')
  })

  void $table.offsetWidth; // reflow to avoid premature transition animations

  elementsToMove.forEach(({ elem, newParentElem }) => {
    newParentElem.appendChild(elem)
  })

  for (let row of elementsToMove) {
    let target_x, target_y
    if (row.type === 'from-table') {
      const {oldTranslate, old_x, old_y, new_x, new_y, elem} = row
      const [tx, ty] = oldTranslate.slice(1).map(Number)
      const bbox_2 = elem.getBoundingClientRect()
      console.log(elem.dataset, 'to', bbox_2)
      new_x = (bbox_2.x / scale) - tx
      new_y = (bbox_2.y / scale) - ty

      target_x = old_x - new_x
      target_y = old_y - new_y
    }
    else if (row.type === 'from-hand') {
      const {elem, handElem, new_x, new_y} = row
      const elem_bbox = elem.getBoundingClientRect()
      const old_x = elem_bbox.x / scale
      const old_y = elem_bbox.y / scale
      console.log('x', old_x, new_x, elem_bbox)
      console.log('y', old_y, new_y, elem_bbox)
      target_x = new_x - old_x - (window.scrollX / scale)
      target_y = new_y - old_y - (window.scrollY / scale)
      handElem.remove()
    }

    row.elem.style.transform = `translate(${target_x}px, ${target_y}px)`
  }

  void $table.offsetWidth; // reflow to avoid premature transition animations

  elementsToMove.forEach(({ elem }) => {
    elem.classList.add('npc')
  })

  void $table.offsetWidth; // reflow to ensure transition animations

  elementsToMove.forEach(({ elem }) => {
    elem.style.transform = `translate(0px, 0px)`
  })
}

function syncDraggable(draggables, elem, id) {
  if (draggables[id]) {
    elem.dataset.draggable = draggables[id]
  }
  else if (elem.dataset.draggable) {
    delete elem.dataset.draggable
  }
}

function syncPeekable(peekables, elem, id) {
  if (peekables[id]) {
    elem.dataset.peekable = peekables[id]
  }
  else if (elem.dataset.peekable) {
    delete elem.dataset.peekable
  }
}

function findInHand(id) {
  return $handbar.querySelector(`[data-id="${id}"]`)
}

function objectDomId(id) {
  return `obj-${id}`
}
