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
  require(handler)(m)
}
