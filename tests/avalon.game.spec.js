const o = require("ospec")
const {create} = require('../src/games/avalon')

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
    const errors = await game.checkReady()
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
  await game.addPlayer('one')
  await game.addPlayer('two')
  await game.addPlayer('three')
  await game.addPlayer('four')
  await game.addPlayer('five')
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
  const game = createFivePlayerGameNoRoles()
  await game.addConfig('roles', 'merlin')
  await game.addConfig('roles', 'servant_1')
  await game.addConfig('roles', 'servant_2')
  await game.addConfig('roles', 'assassin')
  await game.addConfig('roles', 'minion_1')
  return game
}

o('begins the first round', async function() {
  const game = await createFivePlayerGame()
  // TODO
  // const state = await game.getState()
  // o(state.hands.one.length).deepEquals(1)
  // o(state.hands.two.length).deepEquals(1)
  // o(state.hands.three.length).deepEquals(1)
  // o(state.hands.four.length).deepEquals(1)
  // o(state.hands.five.length).deepEquals(1)
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
