const Avalon = require('./games/avalon')
const panzoom = require('panzoom')
const ActionBar = require('./shared/action-bar')

const panzoomInstance = panzoom(document.getElementById('table'), {
  maxZoom: 1,
  minZoom: 0.25,
  // beforeWheel: function(e) {
  //   // allow wheel-zoom only if altKey is down. Otherwise - ignore
  //   var shouldIgnore = !e.altKey;
  //   return shouldIgnore;
  // },
  // beforeMouseDown: function(e) {
  //   // allow mouse-down panning only if altKey is down. Otherwise - ignore
  //   var shouldIgnore = !e.altKey;
  //   return shouldIgnore;
  // },
  // transformOrigin: {x: 0.5, y: 0.5}
}).zoomAbs(0, 0, 1)

let game;
let actionBar;
let currentPlayer = 'p1'

startAvalon()

async function startAvalon() {
  game = Avalon.createGame()
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

  document.getElementById('table').innerHTML =
    await Avalon.views.table(currentPlayer, game)

  await game.start()
  actionBar = ActionBar(currentPlayer, game)
  sync()
}

async function sync() {
  const state = await game.getState()
  const draggables = await game.getDraggables(currentPlayer)

  state.cards.forEach(card => {
    let domId = objectDomId(card.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
      elem.className = 'card'
      elem.dataset.type = card.type
      elem.dataset.name = card.name
      elem.innerHTML = `
        <div class="card-flipper">
          <div class="card-front"></div>
          <div class="card-back"></div>
        </div>
      `
      document.querySelector(zoneToSelector(card.zone)).appendChild(elem)
    }
    elem.dataset.face = card.face
    if (draggables[card.id]) {
      elem.dataset.draggable = draggables[card.id]
    }
  })

  state.tokens.forEach(token => {
    let domId = objectDomId(token.id)
    let elem = document.getElementById(domId)
    if (!elem) {
      elem = document.createElement('div')
      elem.id = domId
      elem.className = 'token'
      elem.dataset.type = token.type
      document.querySelector(zoneToSelector(token.zone)).appendChild(elem)
    }
    if (draggables[token.id]) {
      elem.dataset.draggable = draggables[token.id]
    }
  })

  await actionBar.sync()
}

function zoneToSelector(zone, id) {
  return zone.split('/').map(name => `.zone-${name}`).concat(id ? [`#${objectDomId(id)}`] : []).join(' ')
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
