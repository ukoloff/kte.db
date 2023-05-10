//
// Поиск материала
//
const db = require('../db')

module.exports = mat

function mat(m) {
  // Обязательные поля
  m.errors = []
  m.warnings = []

  var metal = db.METAL.filter(x => x.ID_MAT == m.cur_id_mat)
  if (metal.length < 1) {
    m.errors.push(`Metal #${m.cur_id_mat} not found`)
    return
  }
  if (metal.length > 1) {
    m.warnings.push(`Metal #${m.cur_id_mat} found ${metal.length} times`)
  }
  return metal[0]
}
