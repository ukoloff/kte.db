// * MESSAGEBOX("Открытая зона наружная  черновая ",0," ")
//
const orderBy = require('lodash/orderBy')
const round = require('lodash/round')

const db = require('../db')

module.exports = query

function query(m) {
  for (const Kd_gr_rezc of m.$pri) {
    // SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
    var rezc_tmp = db.cutters.filter(x => x.TIP == Kd_gr_rezc && x.DIRECT == m.direction)
    if (rezc_tmp.length < 1) continue
    rezc_tmp = orderBy(rezc_tmp, 'PRIOR')[0]
    m.kod_instr = rezc_tmp.INSTR_ID
    m.name_instr = rezc_tmp.NAME
    m.obozn_instr = rezc_tmp.OBOZN

  }
}
