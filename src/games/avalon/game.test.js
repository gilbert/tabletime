const o = require("ospec")
const {create} = require('./game')

o.spec('Standard API', async function () {
  o('adds players', async function() {
    const game = create()
    await game.addPlayer('alice')
    await game.addPlayer('bob')

    const players = await game.getPlayers()
    o(players).deepEquals([ 'alice', 'bob' ])
  })

  o('removes players', async function() {
    const game = create()
    await game.addPlayer('alice')
    await game.addPlayer('bob')
    await game.removePlayer('bob')

    const players = await game.getPlayers()
    o(players).deepEquals([ 'alice' ])
  })

  o('needs at least one player', async function () {
    const game = create()
    const errors = await game.checkReadyToStart()
    o(errors.some(e => e.match(/players/))).deepEquals(true)
  })
})

o('supports 5 to 10 players', async function() {
  const game = create()
  await game.addPlayer('one')
  await game.addPlayer('two')
  await game.addPlayer('three')
  await game.addPlayer('four')
  await expectReadyError(/only supports/i, game)

  await game.addPlayer('five')
  await expectReadyError(/only supports/i, game, false)

  await game.addPlayer('six')
  await game.addPlayer('seven')
  await game.addPlayer('eight')
  await game.addPlayer('nine')
  await game.addPlayer('ten')
  await expectReadyError(/only supports/i, game, false)

  game.addPlayer('eleven')
  await expectReadyError(/only supports/i, game)
})

o('available roles', async function () {
  const game = create()
  const opts = await game.getConfigOptions('roles')
  o(opts.sort()).deepEquals([
    'assassin',
    'merlin',
    'minion_1',
    'minion_2',
    'minion_3',
    'mordred',
    'morgana',
    'oberon',
    'percival',
    'servant_1',
    'servant_2',
    'servant_3',
    'servant_4',
    'servant_5',
  ])
})

o('validates adding a role', async function () {
  const game = create()
  expectError(/invalid/i, await game.addConfig('roles', 'judge'))
})

async function createFivePlayerGameNoRoles() {
  const game = create()
  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')
  await game.addPlayer('p4')
  await game.addPlayer('p5')
  return game
}

o('cannot start unless role count matches player count', async function() {
  const game = await createFivePlayerGameNoRoles()
  await expectReadyError(/5 roles/i, game)

  expectNoErrors(await game.addConfig('roles', 'merlin'))
  expectNoErrors(await game.addConfig('roles', 'servant_1'))
  expectNoErrors(await game.addConfig('roles', 'servant_2'))
  expectNoErrors(await game.addConfig('roles', 'assassin'))
  expectNoErrors(await game.addConfig('roles', 'minion_1'))

  await expectReadyError(/5 roles/i, game, false)
})

o('validates evil role count', async function () {
  const game = await createFivePlayerGameNoRoles()
  expectNoErrors(await game.addConfig('roles', 'merlin'))
  expectNoErrors(await game.addConfig('roles', 'servant_1'))
  expectNoErrors(await game.addConfig('roles', 'servant_2'))
  expectNoErrors(await game.addConfig('roles', 'servant_3'))
  expectNoErrors(await game.addConfig('roles', 'minion_1'))

  await expectReadyError(/2 evil/i, game)
})

async function createFivePlayerGame() {
  const game = await createFivePlayerGameNoRoles()

  await game.addConfig('roles', 'merlin')
  await game.addConfig('roles', 'servant_1')
  await game.addConfig('roles', 'servant_2')
  await game.addConfig('roles', 'assassin')
  await game.addConfig('roles', 'minion_1')

  expectNoErrors(await game.setConfig('random_seed', 'ospec test seed'))

  o(await game.start()).equals(true)

  return game
}

o.only('runs through a game', async function() {
  const game = await createFivePlayerGame()
  const state = await game.getState()

  function playerHand(player, role) {
    return [
      { zone: `player/${player}/assigned_role`, type: 'role', name: role, face: 'down' },
      { zone: `player/${player}/hand`, type: 'vote', name: 'approve', face: 'down' },
      { zone: `player/${player}/hand`, type: 'vote', name: 'reject', face: 'down' },
    ]
  }

  o(state.cards.map(noId)).deepEquals([
    ...playerHand('p1', 'assassin'),
    ...playerHand('p2', 'minion_1'),
    ...playerHand('p3', 'servant_2'),
    ...playerHand('p4', 'servant_1'),
    ...playerHand('p5', 'merlin'),
    ...times(5, { zone: 'shared/standby/mission_tokens', type: 'mission', face: 'up' }),
    ...times(3, { zone: 'shared/standby/quest_cards', type: 'quest', name: 'fail', face: 'down' }),
    ...times(3, { zone: 'shared/standby/quest_cards', type: 'quest', name: 'success', face: 'down' }),
  ])

  o(state.tokens.map(noId)).deepEquals([
    { zone: 'player/p1/status', type: 'king' },
    ...times(2, { zone: 'player/p1/unused_nominations', type: 'nomination' }),
    { zone: 'shared/board/reject_count/0', type: 'reject' },
    ...times(1, { zone: 'shared/standby/nomination_tokens', type: 'nomination' }),
  ])

  o(await game.getCurrentPhaseTodos()).deepEquals([
    'King must nominate 2 more players'
  ])

  o(await game.getDraggables('p2')).deepEquals({})
  o(await game.getAvailableActions('p2')).deepEquals([])

  o(await game.getDraggables('p1')).deepEquals({ 114: true, 117: true })
  o(await game.getDroppables('p1', 114)).deepEquals({
    zones: {
      'player/p1/status': ['nominate', [114, 'p1']],
      'player/p2/status': ['nominate', [114, 'p2']],
      'player/p3/status': ['nominate', [114, 'p3']],
      'player/p4/status': ['nominate', [114, 'p4']],
      'player/p5/status': ['nominate', [114, 'p5']],
    }
  })
  o(await game.getAvailableActions('p1')).deepEquals([
    { name: 'nominate', type: 'drag', label: 'nominate' },
  ])

  o(await game.act('p1', 'nominate', [114, 'p2'])).equals(true)

  o(await game.getCurrentPhaseTodos()).deepEquals([
    'King must nominate 1 more player'
  ])

  o(await game.getDraggables('p1')).deepEquals({ 117: true })
  o(await game.getDroppables('p1', 117)).deepEquals({
    zones: {
      'player/p1/status': ['nominate', [117, 'p1']],
      'player/p3/status': ['nominate', [117, 'p3']],
      'player/p4/status': ['nominate', [117, 'p4']],
      'player/p5/status': ['nominate', [117, 'p5']],
    }
  })

  o(await game.act('p1', 'nominate', [117, 'p5'])).equals(true)

  o(await game.getCurrentPhaseTodos()).deepEquals([])
  o(await game.getAvailableActions('p1')).deepEquals([
    { name: 'next_phase', type: 'button', label: 'Proceed to Voting Phase' },
  ])
  o(await game.getAvailableActions('p2')).deepEquals([])

  //
  // Voting
  //
  o(await game.act('p1', 'next_phase', [])).equals(true)

  o(await game.getAvailableActions('p1')).deepEquals([
    { name: 'vote', type: 'drag', label: 'vote' }
  ])
  o(await game.getAvailableActions('p2')).deepEquals([
    { name: 'vote', type: 'drag', label: 'vote' }
  ])

  o(await game.getCurrentPhaseTodos()).deepEquals([
    '{{p1}} must cast a vote',
    '{{p2}} must cast a vote',
    '{{p3}} must cast a vote',
    '{{p4}} must cast a vote',
    '{{p5}} must cast a vote',
  ])
})

//
// Test helpers
//
async function expectError(regex, errors, expectToBePresent=true) {
  const has = errors.some(e => regex.test(e))
  if (!has && expectToBePresent) {
    console.log('Did not have error', regex, ':\n', errors)
  }
  o(has).equals(expectToBePresent)
}
async function expectReadyError(regex, game, expectToBePresent=true) {
  await expectError(regex, await game.checkReadyToStart(), expectToBePresent)
}
function expectNoErrors(errors) {
  o(errors).deepEquals([])
}
function noId({id, ...rest}) {
  return rest
}
function times(n, obj) {
  return Array(n).fill(obj)
}
