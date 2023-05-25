//
// Output CSV
//
const extract = require('./extract')

module.exports = csv

function csv(m) {
  return Object.entries(extract(m))
    .map(([k, v]) => {
      if (v == null) v = ''
      v = String(v).replace(/"/g, '""')
      if (/^\s+|\s+$/.test(v)) {
        v = `"${v}"`
      }
      return v
    })
    .join(',')
}
