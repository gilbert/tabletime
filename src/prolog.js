const pl = require('tau-prolog')
require('tau-prolog/modules/lists')(pl)
require('tau-prolog/modules/random')(pl)
require('tau-prolog/modules/statistics')(pl)
require('tau-prolog/modules/js')(pl)
require('tau-prolog/modules/system')(pl)

exports.createPrologInstance = function createPrologInstance() {
  return pl.create()
}
