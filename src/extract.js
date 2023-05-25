//
// Extract output fields
//
module.exports = extract

function extract(m) {
  let result = {}
  'kod_instr obozn_instr Ar F V'
    .split(/\s+/)
    .forEach(f => result[f] = m[f])
  return result
}
