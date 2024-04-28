# FLP 23/24 - 2. projekt

## Téma

Rubikova kostka

## Autor

Vojtěch Dvořák (xdvora3o)

## Popis

Jedná se o program v jazyce Prolog, který řeší instance Rubikovy kostky.
Interně je Rubikova kostka reprezentována pomocí dvojrozměrného seznamu o rozměrech 6 x 9 (6 stěn, z nichž každá má 9 barevných ploch).
Položky, reprezantující barevné plochy jsou v seznamech rozloženy následujícím způsobem:

```
[
    [F1,F2,F3,F4,F5,F3,F7,F8,F9],
    [R1,R2,R3,R4,R5,R3,R7,R8,R9],
    [B1,B2,B3,B4,B5,B3,B7,B8,B9],
    [L1,L2,L3,L4,L5,L3,L7,L8,L9],
    [T1,T2,T3,T4,T5,T3,T7,T8,T9],
    [D1,D2,D3,D4,D5,D3,D7,D8,D9]
]
```

```
T1 T2 T3
T4 T5 T6
T7 T8 T9
F1 F2 F3   R1 R2 R3   B1 B2 B3   L1 L2 L3
F4 F5 F6   R4 R5 R6   B4 B5 B6   L4 L5 L6  
F7 F8 F9   R7 R8 R9   B7 B8 B9   L7 L8 L9  
D1 D2 D3
D4 D5 D6
D7 D8 D9
```

Program prohledává prostor řešení pomocí algoritmu IDS (iterativní prohledávání do hloubky), čímž je zajištěno, že program nalezne vždy optimální řešení.
Ačkoliv bylo experimentováno i různými způsoby, jak prostor prořezat (např. kontrolou již navštívených stavů, zakázáním, některých sekvencí tahů apod.), tyto pokusy nevedly ke zlepšení a často naopak vnesly do algoritmu režii, která prohledávání více zpomalila. Prostor je definován osmnácti tahy specifikovanými v zadání (U, U', B, B', E, E', F, F', R, R', L, L', M, M', S, S', D, D'), které jsou implementovány v souboru `cube.pl` a jeden po druhém otestovány pomocí testovacího rámce, který je součástí odevzdaného archivu. 

## Obsah odevzdaného archivu

`tests` - Adresář s testovacími případy

`examples` - Adresář s instancemi Rubikovy kostky pro manuální testování/experimentování

`cube.pl` - Klauzule realizující tahy

`ids_solver.pl` - Implementace IDS algoritmu

`main.pl` - Hlavní tělo programu

`Makefile` - Soubor pro program `make` s cíli pro překlad atd.

`test-moves.sh` - Testovací rámec pro testování samostatných tahů

## Překlad a spuštění

Program přeložíme pomocí přiloženého programu `Makefile` a programu `make` (prerekvizitou je mít nainstalovaný program `swipl`):

```
make
```



## Příklady

