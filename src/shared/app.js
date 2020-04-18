const Avalon = require('../games/avalon')
// const panzoom = require('panzoom')
// const Panzoom = require('@panzoom/panzoom')
const ActionBar = require('./action-bar')
const { zoneToSelector } = require('./util')

let actionBar;

let game, scale;
let app = module.exports
app.game;
app.tableScale = 1
app.currentPlayer = 'p1'

app.sync = sync
app.start = startAvalon

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

async function startAvalon() {
  app.game = (game = Avalon.createGame())
  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')
  await game.addPlayer('p4')
  await game.addPlayer('p5')

  await game.addConfig('roles', 'merlin')
  await game.addConfig('roles', 'servant_1')
  await game.addConfig('roles', 'servant_2')
  await game.addConfig('roles', 'assassin')
  await game.addConfig('roles', 'minion_1')

  const table = document.getElementById('table')
  table.innerHTML = await Avalon.views.table(app.currentPlayer, game)

  //
  // TODO, SOMEONE: Math here is off, could definitely be more accurate.
  //
  const padding = 100
  scale = app.tableScale = Math.max(
    Math.min(1, (window.innerWidth - padding) / (tableCanvas.offsetWidth - padding/2)),
    Math.min(1, (window.innerHeight - padding) / (tableCanvas.offsetHeight - padding/2)),
  )

  table.style.transform = `scale(${scale})`

  tableCanvas.dataset.originalWidth = tableCanvas.offsetWidth
  tableCanvas.dataset.originalHeight = tableCanvas.offsetHeight
  tableCanvas.style.width = `${tableCanvas.offsetWidth * scale + padding/2}px`
  tableCanvas.style.height = `${tableCanvas.offsetHeight * scale + padding/2}px`


  await game.start()
  actionBar = ActionBar(app)
  sync()
}

async function sync() {
  const state = await game.getState()
  const draggables = await game.getDraggables(app.currentPlayer)

  state.cards.forEach(card => {
    let domId = objectDomId(card.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
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
      document.querySelector(zoneToSelector(card.zone)).appendChild(elem)
    }
    elem.dataset.face = card.face
    if (draggables[card.id]) {
      elem.dataset.draggable = draggables[card.id]
    }
    else if (elem.dataset.draggable) {
      delete elem.dataset.draggable
    }
  })

  state.tokens.forEach(token => {
    let domId = objectDomId(token.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
      elem.className = 'token npc'
      elem.dataset.id = token.id
      elem.dataset.type = token.type
      elem.dataset.zone = token.zone
      document.querySelector(zoneToSelector(token.zone)).appendChild(elem)
    }
    else if (elem.dataset.zone !== token.zone) {
      const bbox = elem.getBoundingClientRect()
      const old_x = bbox.left / scale
      const old_y = bbox.top / scale
      const oldTranslate = (elem.style.transform || '').match(/translate\(([0-9\.\-]+)px, *([0-9\.\-]+)px\)/)

      const newParent = document.querySelector(zoneToSelector(token.zone))
      newParent.appendChild(elem)

      if (oldTranslate) {
        const [tx, ty] = oldTranslate.slice(1).map(Number)
        const bbox_2 = elem.getBoundingClientRect()
        const new_x = (bbox_2.left / scale) - tx
        const new_y = (bbox_2.top / scale)  - ty

        elem.style.transform = `translate(${old_x - new_x}px, ${old_y - new_y}px)`
        requestAnimationFrame(() => {
          elem.classList.add('npc')
          elem.style.transform = `translate(0px, 0px)`
        })
      }
    }
    if (draggables[token.id]) {
      elem.dataset.draggable = draggables[token.id]
    }
    else if (elem.dataset.draggable) {
      delete elem.dataset.draggable
    }
  })

  await actionBar.sync()
}

function objectDomId(id) {
  return `obj-${id}`
}

function moveAnimate(element, newParent){
  //Allow passing in either a jQuery object or selector
  element = $(element);
  newParent= $(newParent);

  var oldOffset = element.offset();
  element.appendTo(newParent);
  var newOffset = element.offset();

  var temp = element.clone().appendTo('body');
  temp.css({
      'position': 'absolute',
      'left': oldOffset.left,
      'top': oldOffset.top,
      'z-index': 1000
  });
  element.hide();
  temp.animate({'top': newOffset.top, 'left': newOffset.left}, 'slow', function(){
     element.show();
     temp.remove();
  });
}
