// * MESSAGEBOX("Торец черновая ",0," ")

const query = require('./query')
const src = require('./in')
const out = require('../out')

query(src)

console.log(out(src))
