const {escapeHtml} = require('../../shared/util')
const allPlayers = Array(4).fill().map((_, n) => `p${n+1}`)

exports.table = async (app) => `
  <div class="zone-shared">
    <div class="flex items-center justify-center mb-4">
      <div class="zone-vp_60 flex"></div>
      <div class="zone-objective_cards ml-10"></div>
      <div class="zone-first_player ml-8"></div>
    </div>
    <div class="zone-board">
      <div class="zone-map">
        ${Array(9 * 6).fill().map((_, n) =>
          `<div class="zone-${n}"></div>`
        ).join('\n')}
      </div>
      <div class="zone-round">
        <div class="zone-1"></div>
        <div class="zone-2"></div>
        <div class="zone-3"></div>
        <div class="zone-4"></div>
      </div>
      <div class="zone-build"></div>
      <div class="zone-wheat_fields"></div>
      <div class="zone-market"></div>
    </div>
    <div class="zone-resources mt-6">
      <div class="flex items-center">
        <div class="zone-coin_3 flex"></div>
        <div class="zone-coin_1 flex ml-4"></div>
      </div>
      <div class="pt-6"></div>
      <div class="flex">
        <div class="zone-wheat flex cube"></div>
        <div class="zone-fish flex cube ml-4"></div>
      </div>
      <div class="flex mt-4">
        <div class="zone-wood flex cube"></div>
        <div class="zone-stone flex cube ml-4"></div>
      </div>
    </div>
  </div>

  ${allPlayers.map(player => `
    <div class="zone-player">
      <div class="zone-${player}">
        <div class="player-name">${escapeHtml(app.playerNames[player])}</div>
        <div class="zone-objective_cards"></div>
        <div class="zone-first_player_token"></div>
        <div class="zone-resources">
          <div class="zone-coin_3"></div>
          <div class="zone-coin_1"></div>
          <div class="zone-wheat"></div>
          <div class="zone-fish"></div>
          <div class="zone-wood"></div>
          <div class="zone-stone"></div>
        </div>
        <div class="zone-workers"></div>
        <div class="zone-houses"></div>
      </div>
    </div>
  `).join('\n')}
`

exports.handBar = async () => ``
