const {eventmove} = require('@composi/gestures')
const app = require('../app')

let mouseX, mouseY, viewingItem

document.addEventListener(eventmove, e => {
  mouseX = e.pageX - window.pageXOffset
  mouseY = e.pageY - window.pageYOffset
})

document.addEventListener('keydown', e => {
  const item = document.elementsFromPoint(mouseX, mouseY).find(elem =>
    elem.classList.contains('card') || elem.classList.contains('token')
  )

  if (e.altKey && item) {
    const clone = item.cloneNode(true)
    delete clone.id
    clone.style = ''
    $itemViewer.innerHTML = ''
    $itemViewer.appendChild(clone)
    $itemViewer.classList.add('-viewing')
    viewingItem = item
  }

  if (e.altKey && e.shiftKey && viewingItem) {
    $itemViewer.classList.add(viewingItem.dataset.peekable ? '-peeking' : '-cannot-peek')
  }
})

document.addEventListener('keyup', e => {
  if (!e.shiftKey) {
    $itemViewer.classList.remove('-peeking', '-cannot-peek')
  }
  if (!e.altKey) {
    $itemViewer.classList.remove('-viewing')
  }
})
