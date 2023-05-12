from pathlib import Path
from dbfread import DBF
from xlsxwriter import Workbook

me = Path(__file__).parents[2]

src = me.parent / "kte.wiki/Bog"
dst = me / "tmp/kte.xlsx"
xlsx = Workbook(dst)

for db in src.glob('*.dbf'):
    if 'tmp' in db.stem.lower():
        continue
    if 'foxuser' == db.stem.lower():
        continue
    print(db.stem, end=' ')
    dbf = DBF(db)
    ws = xlsx.add_worksheet(db.stem)
    ws.write_row(0, 0, dbf.field_names)
    for i, record in enumerate(dbf):
        ws.write_row(i + 1, 0, record.values())
    ws.autofit()
xlsx.close()
