// * MESSAGEBOX("Торец черновая ",0," ")

const db = require('../db')
const metal = require('./metal')

module.exports = query

function query(m) {
  var cur_metal = metal(m)
  for (const grp of prioritets(m)) {
    m.Kd_gr_rezc = grp
  }
}

function prioritets(m) {
  var ps = db.prioritet.filter(x => x.KTE == m.cur_kte && x.CHIST == m.cur_chist)
  if (ps.length != 1) {
    m.errors.push("Error looking for priority ${m.cur_kte}:${m.cur_chist}")
    return
  }
  ps = ps[0]
  return Object.keys(ps)
    .filter(x => /^pri\D*\d+$/i.test(x))
    .map(x => ps[x])
    .filter(x => x)
}
