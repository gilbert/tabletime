const tippy = require('tippy.js')
const {escapeHtml} = require('./util')
const template = require('lodash/template')
const templateSettings = require('lodash/templateSettings')

templateSettings.interpolate = /{{([\s\S]+?)}}/g

module.exports = function ActionBar (app) {
  let todos = []
  const div = document.getElementById('action-bar')
  tippy.delegate(div, {
    target: '[data-tippy-content]',
    allowHTML: true,
  })

  document.getElementById('action-bar').addEventListener('click', async e => {
    const action = e.target.dataset.action
    if (!action) return;

    const args = JSON.parse(e.target.dataset.args)
    await app.game.act(app.currentPlayer, action, args)
    await app.sync()
    render()
  })

  async function render() {
    todos = await app.game.getCurrentPhaseTodos()

    const available = await app.game.getAvailableActions(app.currentPlayer, undefined, [])
    const actions = available.filter(a => a.type === 'button')
    let actionsHtml
    if (actions.length) {
      actionsHtml = actions.map(a => `
        <button
          data-action="${a.name}"
          data-args='${JSON.stringify(a.args || [])}'
          class="flex item-center rounded-sm px-2 py-1 bg-gray-200 text-gray-800"
        >${a.label}</button>
      `).join('\n')
    }
    else {
      actionsHtml = ''
    }

    div.innerHTML = `
      <div
        class="flex items-center"
        data-tippy-content="${
          escapeHtml(todos.map(t => template(t)(app.playerNames)).join('<br />'))
          || 'Ready to move on to next phase'
        }"
      >
        Game Status
        <div class="w-3 h-3 ml-2 ${
          todos.length > 0 ? 'bg-orange-400' : 'bg-green-500 rounded-full'
        }"></div>
      </div>
      <div class="flex-1 flex items-center justify-center">${actionsHtml}</div>
      <div>
        You are ${app.playerNames[app.currentPlayer]}
      </div>
    `
  }

  render()

  return {
    async sync() {
      action = undefined
      args = []
      await render()
    }
  }
}
