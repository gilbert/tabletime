const {eventstart, eventend, eventmove, eventcancel} = require('@composi/gestures')
const {zoneToSelector} = require('../util')
const app = require('../app')

let dragElem = null
let dropZone = null
let dragStart = []
let dragStartOffset = []
let dragMove = []
let zCounter = 500
let droppables = { zones: {} };
let droppableElems = []

document.addEventListener(eventstart, async e => {
  if (dragElem) {
    // This may occur (in the future, not yet implemented) when a user has
    // dropped into a drop zone, but has not selected which action to take.
    return
  }
  if (e.target.dataset.draggable !== 'true') return;

  dragElem = e.target
  dropZone = null

  const bbox = dragElem.getBoundingClientRect()
  dragStart[0] = (dragMove[0] = e.pageX)
  dragStart[1] = (dragMove[1] = e.pageY)
  dragStartOffset[0] = dragStart[0] - bbox.x
  dragStartOffset[1] = dragStart[1] - bbox.y

  dragElem.classList.remove('npc')
  dragElem.style.zIndex = zCounter++

  // If an item is draggable, then it is either a card or token
  droppables = await app.game.getDroppables(app.currentPlayer, +dragElem.dataset.id)
  droppableElems = []
  for (let zone in droppables.zones) {
    const droppable = document.querySelector(zoneToSelector(zone))
    droppable.classList.add('droppable')
    droppableElems.push(droppable)
  }
})

document.addEventListener(eventmove, e => {
  if (!dragElem) return;
  const offsetX = (e.pageX - dragStart[0]) / app.tableScale
  const offsetY = (e.pageY - dragStart[1]) / app.tableScale
  dragElem.style.transform = `translate(${offsetX}px, ${offsetY}px)`

  //
  // Search for dropzones under the cursor
  //
  dropZone = null
  const wx = window.pageXOffset
  const wy = window.pageYOffset
  for (let el of droppableElems) {
    const bbox = el.getBoundingClientRect()
    const x1 = bbox.left + wx
    const x2 = x1 + bbox.width
    const y1 = bbox.top + wy
    const y2 = y1 + bbox.height

    if (e.pageX > x1 && e.pageX < x2 && e.pageY > y1 && e.pageY < y2) {
      // Hit
      el.classList.add('hover')
      dropZone = el
    }
    else {
      el.classList.remove('hover')
    }
  }
})

document.addEventListener(eventend, async e => {
  if (!dragElem) return;
  const el = dragElem
  dragElem = null

  if (dropZone) {
    if (app.findInHand(el.dataset.id)) {
      el.dataset.dropX = e.pageX + window.pageXOffset - dragStartOffset[0]
      el.dataset.dropY = e.pageY + window.pageYOffset - dragStartOffset[1]
    }

    let actions = [];
    for(let zone in droppables.zones) {
      if (dropZone.matches(zoneToSelector(zone))) {
        actions.push(droppables.zones[zone])
      }
    }
    let [action, args] = actions.length >= 2
      ? await Promise.resolve('TODO: Ask user which action they want to perform')
      : actions[0]

    await app.game.act(app.currentPlayer, action, args)
    await app.sync()
  }
  else {
    el.classList.add('npc')
    requestAnimationFrame(() => {
      el.style.transform = `translate(0px, 0px)`
    })
  }
  for (let dropZone of document.querySelectorAll('.droppable')) {
    dropZone.classList.remove('droppable', 'hover')
  }
})
