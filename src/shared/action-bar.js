
module.exports = function ActionBar (currentPlayer, game) {
  let action = undefined
  let args = []
  const div = document.getElementById('action-bar')

  document.getElementById('action-bar').addEventListener('click', e => {
    action = e.target.dataset.action
    args = JSON.parse(e.target.dataset.args || '[]')
    render()
  })

  async function render() {
    console.log("hm", action, args)
    const actions = await game.getAvailableActions(currentPlayer, action, args)
    if (actions.length) {
      div.innerHTML = actions.map(a => `
        <button
          data-action="${action || a}"
          ${action ? `data-args='${JSON.stringify(args.concat([a]))}'` : ''}
        >${a}</button>
      `).join('\n')
    }
    else {
      div.innerHTML = 'No actions available.'
    }
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
