const fs = require('fs')
const path = require('path')
const yaml = require('js-yaml')

const dispatch = require('../src/dispatch')

describe("Find tool", context)

function context() {
  let folder = path.join(path.dirname(__filename), 'kte')
  let d = fs.opendirSync(folder)
  let f
  let ktes = []
  while (f = d.readSync()) {
    if (!f.isFile() || path.extname(f.name).toLowerCase() != '.yml') continue
    let z = path.join(folder, f.name)
    let kte  = yaml.load(fs.readFileSync(z, 'utf-8'))
    kte.name = kte.name || path.parse(f.name).name
    ktes.push(kte)
  }
  const only = ktes.filter(x => 'only' in x).length > 0
  ktes.forEach(kte => {
    it(kte.name, function() {
      if ('skip' in kte) this.skip()
      if (only && !('only' in kte)) this.skip()

      dispatch(kte)
    })
  })
}
