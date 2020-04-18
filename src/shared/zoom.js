const kinetic = require('panzoom/lib/kinetic.js')

const el = document.body
const bbox = null

exports.sync = function syncZoom () {
  bbox = {
    left: 0,
    top: 0,
    width: el.clientWidth,
    height: el.clientHeight,
  }
}

function applyTransform(transform) {
  el.style.transformOrigin = '0 0 0';
  el.style.transform = 'matrix(' +
    transform.scale + ', 0, 0, ' +
    transform.scale + ', ' +
    transform.x + ', ' + transform.y + ')'
}
