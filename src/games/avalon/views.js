const allPlayers = Array(10).fill().map((_, n) => `p${n+1}`)

exports.table = async (currentPlayer, game) => `
  <div class="zone-shared">
    <div class="zone-board">
      <div class="zone-reject_count">
        <div class="zone-0"></div>
        <div class="zone-1"></div>
        <div class="zone-2"></div>
        <div class="zone-3"></div>
        <div class="zone-4"></div>
      </div>
    </div>
    <div class="zone-standby">
      <div class="zone-quest_cards"></div>
      <div class="zone-mission_tokens mt-4"></div>
      <div class="zone-nomination_tokens mt-4"></div>
    </div>
  </div>

  ${allPlayers.map(player => `
    <div class="zone-player">
      <div class="zone-${player}">
        <div class="flex item-center">
          <div class="zone-assigned_role"></div>
          <div class="zone-status"></div>
          <div class="zone-vote"></div>
          <div class="zone-hand"></div>
        </div>
        <div class="zone-unused_nominations mt-2 pl-1 flex"></div>
      </div>
    </div>
  `).join('\n')}
`
