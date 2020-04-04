const pl = require('tau-prolog')
require('tau-prolog/modules/lists')(pl)
require('tau-prolog/modules/statistics')(pl)
require('tau-prolog/modules/js')(pl)
require('tau-prolog/modules/system')(pl)
require('tau-prolog/modules/random')(pl)

exports.createPrologInstance = function createPrologInstance() {
  const session = pl.create()
  session.createRandom = createRandom
  return session
}

exports.escapeQueryTemplate = pl.utils.escapeQueryTemplate

//
// Seeded random logic
//
function createRandom(seed) {
  var seed = xmur3(seed);
  return sfc32(seed(), seed(), seed(), seed());
}

// A seeded random function maker
function sfc32(a, b, c, d) {
  return function() {
    a >>>= 0; b >>>= 0; c >>>= 0; d >>>= 0;
    var t = (a + b) | 0;
    a = b ^ b >>> 9;
    b = c + (c << 3) | 0;
    c = (c << 21 | c >>> 11);
    d = d + 1 | 0;
    t = t + d | 0;
    c = c + t | 0;
    return (t >>> 0) / 4294967296;
  }
}

// A hash function
function xmur3(str) {
  for(var i = 0, h = 1779033703 ^ str.length; i < str.length; i++)
      h = Math.imul(h ^ str.charCodeAt(i), 3432918353),
      h = h << 13 | h >>> 19;
  return function() {
      h = Math.imul(h ^ h >>> 16, 2246822507);
      h = Math.imul(h ^ h >>> 13, 3266489909);
      return (h ^= h >>> 16) >>> 0;
  }
}
