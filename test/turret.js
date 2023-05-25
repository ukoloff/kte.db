const fs = require('fs')
const path = require('path')

describe("Find tool",  context)

function context() {
  let folder = path.join(path.dirname(__filename), 'kte')
  let d = fs.opendirSync(folder)
  let f
  while(f = d.readSync()) {
    if(!f.isFile() || path.extname(f.name).toLowerCase() != '.yml') continue
    let z = path.join(folder, f.name)
    console.log(z)
  }
}
