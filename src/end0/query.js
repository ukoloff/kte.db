// * MESSAGEBOX("Торец черновая ",0," ")

const db = require('../db')
const metal = require('./metal')

module.exports = query

function query(m) {
  var cur_metal = metal(m)
  for (const Kd_gr_rezc of prioritets(m)) {
    var rezc_tmp = db.cutters.filter(x => x.TIP == Kd_gr_rezc && x.DIRECT == m.direction)
    if (rezc_tmp.length < 1) continue
    rezc_tmp.sort((a, b) => a.PRIOR - b.PRIOR)
    rezc_tmp = rezc_tmp[0]
    m.kod_instr = rezc_tmp.INSTR_ID
    m.name_instr = rezc_tmp.NAME
    m.obozn_instr = rezc_tmp.OBOZN

    // * Проверка сплава
    m.cur_splav = rezc_tmp.MAT_NAME.trim().toUpperCase()
    m.cur_SMG = cur_metal.SMG[0].toUpperCase()
    var sss = db.SPLAV.filter(x => x.SPLAV.trim().toUpperCase() == m.cur_splav)
    m.splav_ok = sss.length > 0 && sss[0]['SMG_' + m.cur_SMG].trim().length > 0
    if (!m.splav_ok) {
      m.warnings.push("МАТЕРИАЛ РЕЖУЩЕЙ ПЛАСТИНЫ НЕ РЕКОМЕДУЕТСЯ ДЛЯ ОБРАБОТКИ УКАЗАННОГО МАТЕРИАЛА ДЕТАЛИ")
    }
    m.instr_OK = true

    // * Расчет режимов резания
    m.Ar_max = rezc_tmp.ARMAX   // &&&  по режушщей пластине
    m.ar_prip = 2.0             // && Для обработки торца припуск принят 2 мм
    m.ar_obr = m.X_max / 20    // &&& по диаметру

    m.ar_rasc = Math.min(m.Ar_max, m.ar_prip, m.ar_obr)

    m.SMG_met = cur_metal.SMG.trim().toUpperCase()

    break
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
