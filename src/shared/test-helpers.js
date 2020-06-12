const o = require("ospec")

module.exports = {
  expectError,
  expectNoErrors,
  expectReadyError,
  noId,
  times,
}

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
