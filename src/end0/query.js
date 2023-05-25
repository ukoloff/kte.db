// * MESSAGEBOX("Торец черновая ",0," ")
const compact = require('lodash/compact')
const orderBy = require('lodash/orderBy')
const round = require('lodash/round')

const db = require('../db')
const bench = require('./bench')
const metal = require('./metal')

module.exports = query

function query(m) {
  var cur_metal = metal(m)
  for (const Kd_gr_rezc of prioritets(m)) {
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
    m.cur_SMG = cur_metal.SMG[0].toUpperCase()
    // SELECT * from splav WHERE ALLTRIM(UPPER(splav)) = m.cur_splav  INTO CURSOR  sss_
    var sss = db.SPLAV.filter(x => x.SPLAV.trim().toUpperCase() == m.cur_splav)
    m.splav_ok = sss.length > 0 && sss[0]['SMG_' + m.cur_SMG].trim().length > 0
    if (!m.splav_ok) {
      m.warnings.push("МАТЕРИАЛ РЕЖУЩЕЙ ПЛАСТИНЫ НЕ РЕКОМЕНДУЕТСЯ ДЛЯ ОБРАБОТКИ УКАЗАННОГО МАТЕРИАЛА ДЕТАЛИ")
    }
    m.instr_OK = true

    // * Расчет режимов резания
    m.Ar_max = rezc_tmp.ARMAX   // &&&  по режушщей пластине
    m.ar_prip = 2.0             // && Для обработки торца припуск принят 2 мм
    m.ar_obr = m.X_max / 20    // &&& по диаметру

    m.ar_rasc = Math.min(m.Ar_max, m.ar_prip, m.ar_obr)

    m.SMG_met = cur_metal.SMG.trim().toUpperCase()
    // SELECT * from TURN_1 WHERE UPPER(smg)= m.SMG_met  AND ar <= m.ar_rasc  INTO CURSOR regim_ order BY ar desc, f desc
    var regim = db.turn_1.filter(x => x.SMG.toUpperCase() == m.SMG_met && x.AR <= m.ar_rasc)
    regim = orderBy(regim, ['AR', 'F'], ['desc', 'desc'])
    if (!regim.length) {
      m.errors.push("Режим резания не найден")
      regim = [{}]
    }
    regim = regim[0]
    m.F_tabl = regim.F
    m.V_tabl = regim.V

    // &&& Корректировка от твердости
    if (m.hardness > 35) {
      m.warnings.push("Расчет для закаленных сталей пока в разработке")
    } else {
      // *корректировка  V  от прочности материала
      const koef = (cur_metal.MPA / cur_metal.ETAL_MPA) * 100
      var k_mpa = db.k_mpa.filter(x => x.MPA_PROC >= koef)
      k_mpa = orderBy(k_mpa, 'MPA_PROC')
      if (k_mpa.length > 0) {
        let kmpa = k_mpa[0].KMPA
        if (kmpa != 1) {
          m.V_tabl = Math.trunc(m.V_tabl * kmpa)
        }
      }
    }

    // * проверка по мощности и усилиям резания
    m.min_f = 0
    m.kc = cur_metal.KC

    let P_rasc = (m.ar_rasc * m.F_tabl * m.V_tabl * m.kc) / (60 * 1000 * 0.85)
    if (P_rasc > bench.P_tc) {
      m.ar_rasc = (60 * 1000 * 0.85 * bench.P_tc) / (m.F_tabl * m.V_tabl * m.kc)
      // *   нормирование  до 0.5, 1, 1.5, 2, 2.5, 3
      m.ar_rasc = clamp(0.5, 3, Math.floor(m.ar_rasc * 2) / 2)
    }

    // * Проверка по крутящему моменту

    let m_rasc = m.ar_rasc * m.F_tabl * m.kc * m.X_max / 1000
    m.f2 = m.F_tabl
    if (m_rasc > bench.M_tc) {
      m.f2 *= bench.M_tc / m_rasc
      m.f2 = round(m.f2, 3)
    }

    // *Проверка по усилию подач
    m.F_mx = 4000 // Duplicate in bench.js ???
    m.F_mz = 6000 // --//--

    m.F_rasc = m.ar_rasc * m.f2 * m.kc * 0.35

    if (m.F_rasc > bench.F_mx) {
      m.f2 *= bench.F_mx / m.F_rasc
    }

    m.V = m.V_tabl
    m.F = m.f2
    m.Ar = m.ar_rasc

    // Not used???
    P_rasc = (m.Ar * m.f2 * m.V_tabl * m.kc) / (60 * 1000 * 0.85)
    m_rasc = m.Ar * m.f2 * m.kc * m.X_max / 1000

    return
  }
  m.errors.push('В СПРАВОЧНИКЕ РЕЗЦОВ ИНСТРУМЕНТ НЕ НАЙДЕН')
}

function prioritets(m) {
  // * Скачаем строку Приоритетов выбора инструмента
  var ps = db.prioritet.filter(x => x.KTE == m.cur_kte && x.CHIST == m.cur_chist)
  if (ps.length != 1) {
    m.errors.push("Error looking for priority ${m.cur_kte}:${m.cur_chist}")
    return
  }
  ps = ps[0]
  return compact(Object.keys(ps)
    .filter(x => /^pri\D*\d+$/i.test(x))
    .map(x => ps[x]))
}

function clamp(min, max, x) {
  return Math.max(min, Math.min(max, x))
}
