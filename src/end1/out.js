//
// Output CSV
//
module.exports = csv

function csv(m) {
  return 'kod_instr obozn_instr Ar F V'
    .split(/\s+/)
    .map(f => {
      let v = `${m[f] || 0}`
      v = v.replace(/"/g, '""')
      if (/^\s+|\s+$/.test(v)) {
        v = `"${v}"`
      }
      return v
    })
    .join(',')
}
