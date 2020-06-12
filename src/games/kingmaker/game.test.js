const o = require("ospec")
const {create} = require('./game')
const {expectNoErrors, noId} = require('../../shared/test-helpers')

async function createThreePlayerGame() {
  const game = create()

  await game.addPlayer('p1')
  await game.addPlayer('p2')
  await game.addPlayer('p3')

  expectNoErrors(await game.setConfig('random_seed', 'ospec test seed'))

  o(await game.setup()).equals(true)
  o(await game.start()).equals(true)

  return game
}

o.spec('Kingmaker', function () {

  o.only('runs through a game', async function() {
    const game = await createThreePlayerGame()
    const state = await game.getState()

    function playerHand(p) {
      return [
        { zone: `player/${p}/court`, type: p, name: 'royalty/1', face: 'up' },
        { zone: `player/${p}/court`, type: p, name: 'royalty/2', face: 'up' },
        { zone: `player/${p}/court`, type: p, name: 'royalty/3', face: 'up' },
        ...['p1', 'p2', 'p3'].filter(x => x !== p).map(opponent => {
          return { zone: `player/${p}/hand`, type: p, name: `attack/${opponent}`, face: 'down' }
        }),
        { zone: `player/${p}/hand`, type: p, name: 'banner', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'fanatic', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'gold', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'jester', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'marshals', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'seer', face: 'down' },
        { zone: `player/${p}/hand`, type: p, name: 'thief', face: 'down' },
      ]
    }

    o(state.cards.map(noId)).deepEquals([
      ...playerHand('p1'),
      ...playerHand('p2'),
      ...playerHand('p3'),
    ])

    o(state.phase).deepEquals('choose_action')

    o(await game.getCurrentPhaseTodos()).deepEquals([
      '{{p1}} must play a card',
      '{{p2}} must play a card',
      '{{p3}} must play a card',
    ])

    o(await game.getDraggables('p1')).deepEquals({
      100: true, 101: true, 102: true, 103: true,
      104: true, 105: true, 106: true, 107: true, 108: true,
    })

    // All attacks
    o(await game.act('p1', 'choose_action', [108, 'commit'])).equals(true)
    o(await game.act('p2', 'choose_action', [120, 'commit'])).equals(true)
    o(await game.getCurrentPhaseTodos()).deepEquals(['{{p3}} must play a card'])
    o((await game.getState()).phase).equals('choose_action')

    o(await game.act('p3', 'choose_action', [131, 'commit'])).equals(true)
    o((await game.getState()).phase).equals(['execute', 'hit', ['p3']])
  })
})
