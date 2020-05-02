const tippy = require('tippy.js')
const {escapeHtml} = require('../util')
const template = require('lodash/template')
const templateSettings = require('lodash/templateSettings')

templateSettings.interpolate = /{{([\s\S]+?)}}/g

module.exports = function ActionBar (app) {
  let todos = []
  tippy.delegate($actionBar, {
    target: '[data-tippy-content]',
    allowHTML: true,
  })

  $actionBar.addEventListener('click', async e => {
    const action = e.target.dataset.action
    if (!action) return;

    const args = JSON.parse(e.target.dataset.args)
    await app.game.act(app.currentPlayer, action, args)
    await app.sync()
  })

  $actionBar.addEventListener('click', async e => {
    if (!e.target.dataset['game-start']) return;

    await app.game.start()
    app.started = true
    await app.sync()
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

    let statusHTML
    if (app.started) {
      statusHTML = `
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
      `
    }
    else {
      const errors = await app.game.checkReadyToStart()
      if (errors.length) {
        statusHTML = `
          <div
            class="flex items-center"
            data-tippy-content="${
              escapeHtml(errors.map(t => template(t)(app.playerNames)).join('<br />'))
              || 'Ready to move on to next phase'
            }"
          >
            Configuration Needed
            <div class="w-3 h-3 ml-2 bg-orange-400"></div>
          </div>

        `
      }
      else {
        statusHTML = `
          <div
            data-game-start="true"
            class="flex item-center rounded-sm px-2 py-1 bg-gray-200 text-gray-800 cursor-pointer"
          >
            Start Game
          </div>

        `
      }
    }

    $actionBar.innerHTML = `
      ${statusHTML}
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
