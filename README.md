# kte.db
Search for lathing tool using JavaScript

Этот репозиторий имеет копию
https://github.com/Shalov/kte.db

## Типы КТЭ

 Код | Чист. | КТЭ
----:|:-----:|------
1    | + | Торец
2    | + | Открытая зона наружная
3    | + | Полуоткрытая зона наружная
4    |   | Выточка наружная
5    | + | Канавка №1 наружная
6    | + | Канавка резьбовая наружная
7    |   | Выточка аксиальная наружная
8    | + | Канавка аксиальная наружная
9    | + | Отверстие резцом
10   | + | Полуоткрытая зона внутренняя
11   |   | Выточка внутренняя
12   | + | Канавка №1 внутренняя
13   | + | Канавка резьбовая внутренняя
14   |   | Резьба наружная
15   |   | Резьба внутренняя
16   |   | Сверление

## Параметры станка

```yml
P_tc: 3.7     # ???
M_tc: 30.0    # ???
F_mx: 4000    # ???
F_mz: 6000    # ???
```

## Входные параметры
```yml
  cur_kte: 1      # Код КТЭ
  cur_chist: 0    # Признак чистовой обработки
  cur_id_mat: 77  # Код обрабатываемого материала
  hardness: 0     # Твердость материала после закалки HRC (если есть ТО,HRC, иначе 0)
  X_max: 25       # Xmax обработки (в радиусах)
  X_min:          # Xmin обработки (--//--)
  H_kan:          # Глубина выточки или канавки
  B_kan:          # Ширина выточки или канавки
  d_nach:         # Диаметр начального отверстия под расточку
  L_obr:          # Глубина растачиваемого отверстия
  KOD_REZB:       # Код резьбы: 1 - Метрическая, 2 - Дюймовая
  P_pezb:         # Шаг резьбы
  ugol_rezb:      # Угол профиля резьбы
  roughness: 6.3  # Шероховатость Ra
  direction: R    # Направление обработки
```

## Выходные параметры
```yml
kod_instr: 5
obozn_instr: SCLCR1212M09
Ar: 1.25       # Глубина резания (мм) или количество проходов для нарезания резьбы
F: 0.25        # Подача (мм/об)
V: 148         # Скорость резания (м/мин)
```
