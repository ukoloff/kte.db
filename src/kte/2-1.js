// * MESSAGEBOX("Открытая зона наружная чистовая ",0," ")
const orderBy = require('lodash/orderBy')
const round = require('lodash/round')

const db = require('../db')

module.exports = query

function query(m) {
  for (const Kd_gr_rezc of m.$pri) {
    // * разбор инструмента
    // SELECT * from Cutters WHERE tip=m.Kd_gr_rezc  AND direct= m.direction  INTO TABLE rezc_tmp ORDER BY prior
    var rezc_tmp = db.cutters.filter(x => x.TIP == Kd_gr_rezc && x.DIRECT == m.direction)
    if (rezc_tmp.length < 1) continue
    rezc_tmp = orderBy(rezc_tmp, 'PRIOR')[0]
    m.kod_instr = rezc_tmp.INSTR_ID
    m.name_instr = rezc_tmp.NAME
    m.obozn_instr = rezc_tmp.OBOZN

    // * Проверка сплава
    m.cur_splav = rezc_tmp.MAT_NAME.trim().toUpperCase()
    m.cur_SMG = m.$metal.SMG[0].toUpperCase()
    // SELECT * from splav WHERE ALLTRIM(UPPER(splav)) = m.cur_splav  INTO CURSOR  sss_
    var sss = db.SPLAV.filter(x => x.SPLAV.trim().toUpperCase() == m.cur_splav)
    m.splav_ok = sss.length > 0 && sss[0]['SMG_' + m.cur_SMG].trim().length > 0
    if (!m.splav_ok) {
      m.warnings.push("МАТЕРИАЛ РЕЖУЩЕЙ ПЛАСТИНЫ НЕ РЕКОМЕНДУЕТСЯ ДЛЯ ОБРАБОТКИ УКАЗАННОГО МАТЕРИАЛА ДЕТАЛИ")
    }
    m.instr_OK = true

    // * Расчет режимов резания
    m.ar_rasc = 0.5

    m.SMG_met = m.$metal.SMG.trim().toUpperCase()
    // SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar asc, f asc
    var regim = db.turn_1.filter(x => x.SMG.toUpperCase() == m.SMG_met && x.AR <= m.ar_rasc)
    regim = orderBy(regim, ['AR', 'F'], ['asc', 'asc'])
    if (!regim.length) {
      m.errors.push("Режим резания не найден")
      regim = [{}]
    }
    regim = regim[0]
    m.F_tabl = regim.F
    m.V_tabl = regim.V

    // * выбор подачи от радиуса ппластинки
    m.rrr=m.roughness

    // SELECT * from R_shift WHERE ra<=m.rrr AND r = m.re  INTO CURSOR fff_  order BY ra desc
    var fff = db.R_shift.filter(x => x.RA <= m.rrr && x.R == rezc_tmp.RE)
    fff = orderBy(fff, 'RA', 'desc')
    if (fff.length < 1) {
      m.errors.push("No record in R_shift found!")
    }
    m.F_tabl = Math.min(m.F_tabl, fff[0].F)

    // &&& Корректировка от твердости
    if (m.hardness > 35) {
      m.warnings.push("Расчет для закаленных сталей пока в разработке")
    } else {
      // *корректировка  V  от прочности материала
      const koef = (m.$metal.MPA / m.$metal.ETAL_MPA) * 100
      var k_mpa = db.k_mpa.filter(x => x.MPA_PROC >= koef)
      k_mpa = orderBy(k_mpa, 'MPA_PROC')
      if (k_mpa.length > 0) {
        let kmpa = k_mpa[0].KMPA
        if (kmpa != 1) {
          m.V_tabl = Math.trunc(m.V_tabl * kmpa)
        }
      }
    }

    m.kc = m.$metal.KC

    m.V = m.V_tabl
    m.F = m.F_tabl
    m.Ar = m.ar_rasc

    // Not used???
    let P_rasc = (m.Ar * m.F * m.V * m.kc) / (60 * 1000 * 0.85)
    let m_rasc = m.Ar * m.F * m.kc * m.X_max / 1000

    return
  }
  m.errors.push('В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН')
}
