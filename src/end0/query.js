// * MESSAGEBOX("Торец черновая ",0," ")

const db = require('../db')
const metal = require('./metal')

module.exports = query

function query(m) {
  cur_metal = metal(m)
}
