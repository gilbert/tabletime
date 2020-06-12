const {escapeHtml} = require('../../shared/util')
const allPlayers = Array(5).fill().map((_, n) => `p${n+1}`)

exports.table = async (app) => `
  <div class="zone-shared">
    <div class="zone-discard"></div>
    ${allPlayers.map(player => `
      <div class="zone-${player}" data-player="${player}">
        <div class="zone-chosen"></div>
      </div>
    `).join('\n')}
  </div>

  ${allPlayers.map(player => `
    <div class="zone-player" data-player="${player}">
      <div class="zone-${player}">
        <div class="player-name">${escapeHtml(app.playerNames[player])}</div>
        <div class="flex items-center">
          <div class="zone-court"></div>
          <div class="zone-banner"></div>
        </div>
        <div class="zone-hand mt-4"></div>
        <div class="zone-chosen"></div>
      </div>
    </div>
  `).join('\n')}
`

exports.handBar = async (app) => `
  <!-- This must match card zones -->
  <div class="zone-player">
    <div class="zone-${app.currentPlayer}">
      <div class="zone-hand"></div>
    </div>
  </div>
`
