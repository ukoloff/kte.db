let path = require('path')
let xlsx = require('node-xlsx')

let db = xlsx.parse(path.join(path.dirname(__filename), "../db/kte.xlsx"))

for (var it of db) {
  exports[it.name] = ws2obj(it.data)
}

function ws2obj(ws) {
  let names = ws[0]
  return ws.slice(1).map(x => {
    result = {}
    for (const [i, name] of names.entries()) {
      result[name] = x[i]
    }
    return result
  })
}
