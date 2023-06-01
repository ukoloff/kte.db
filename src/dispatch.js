//
// Select appropriate KTE type to process
//
const bench = require('./bench')
const metal = require('./metal')
const prioritets = require('./prioritets')

module.exports = dispatch

function dispatch(m) {
  // Обязательные поля
  m.errors = []
  m.warnings = []

  const handler = `./kte/${+m.cur_kte}-${+!!m.cur_chist}`
  try {
    require.resolve(handler)
  } catch (e) {
    m.errors.push(`Не найден КТЭ с кодом ${m.cur_kte}/${m.cur_chist}`)
    return
  }
  m.$metal = metal(m)
  m.$pri = prioritets(m)
  m.$bench = bench
  require(handler)(m)
}
