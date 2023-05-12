from pathlib import Path

me = Path(__file__).parent

src = me.parents[2] / "kte.wiki/Bog"
for db in src.glob('*.dbf'):
    if 'tmp' in db.stem.lower():
        continue
    if 'foxuser' == db.stem.lower():
        continue
    print(db.stem, end=' ')
