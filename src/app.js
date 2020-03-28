import Gun from '/web_modules/gun.js'
console.log("yeh", pl)

const gun = Gun(['https://tabletime.herokuapp.com/gun'])

const items = gun.get('items')

const App = {
  view() {
    return m('div',
      m('.items', items.map(item => m('div', item))),
      m('button', {
        // onclick: e =>
      }, 'Add item')
    )
  }
}
