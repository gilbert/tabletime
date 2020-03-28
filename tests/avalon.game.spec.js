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
  o(/only supports/.test((await game.checkReady())[0])).deepEquals(true) `Fewer than 5`

  await game.addPlayer('five')
  o(await game.checkReady()).deepEquals([]) `At least 5`

  await game.addPlayer('six')
  await game.addPlayer('seven')
  await game.addPlayer('eight')
  await game.addPlayer('nine')
  await game.addPlayer('ten')
  o(await game.checkReady()).deepEquals([]) `At most 10`

  game.addPlayer('eleven')
  o(/only supports/.test((await game.checkReady())[0])).deepEquals(true) `More than 10`
})
