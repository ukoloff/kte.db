const compact = require('lodash/compact');
const db = require('./db');

module.exports = prioritets

function prioritets(m) {
  // * Скачаем строку Приоритетов выбора инструмента
  var ps = db.prioritet.filter(x => x.KTE == m.cur_kte && x.CHIST == m.cur_chist);
  if (ps.length != 1) {
    m.errors.push("Error looking for priority ${m.cur_kte}:${m.cur_chist}");
    return;
  }
  ps = ps[0];
  return compact(Object.keys(ps)
    .filter(x => /^pri\D*\d+$/i.test(x))
    .map(x => ps[x]));
}
