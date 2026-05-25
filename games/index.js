import { blokusGame } from './blokus/config.js'
import { players, ranks, sequenceGame, sequenceSpaces, suits } from './sequence/config.js'

export { players, ranks, sequenceSpaces, suits }

export const gameConfigs = Object.freeze({
  sequence: sequenceGame,
  blokus: blokusGame
})
