# FLP 23/24 - 2. projekt

## Téma

Rubikova kostka

## Autor

Vojtěch Dvořák (xdvora3o)

## Popis

Jedná se o program v jazyce Prolog, který řeší instance Rubikovy kostky.
Program očekává na vstupu Rubikovu kostku v zadáním předepsaném formátu (pro příklady viz níže nebo adresář `examples`) a na stadardní výstup vypíše posloupnost tahů vedoucí k jejímu vyřešení (složení).

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
Prostor je definován osmnácti tahy specifikovanými v zadání (U, U', B, B', E, E', F, F', R, R', L, L', M, M', S, S', D, D'), které jsou implementovány v souboru `cube.pl` a otestovány pomocí testovacího skriptu, který je součástí odevzdaného archivu.

Ačkoliv bylo experimentováno i různými způsoby, jak prostor prořezat (např. kontrolou již navštívených stavů, zakázáním, některých sekvencí tahů apod.), tyto pokusy nevedly ke zlepšení a často naopak vnesly do algoritmu režii (např. velký počet porovnávání stavů kostky), která prohledávání více zpomalila.
Stejně tomu bylo i v případě většího množství dynamických predikátů (např. pokud byly použity pro kontrolu již navštívených tahů).

## Obsah odevzdaného archivu

`tests` - Adresář s testovacími případy (pro jednotlivé tahy)

`examples` - Adresář s instancemi Rubikovy kostky pro manuální testování/experimentování

`cube.pl` - Klauzule realizující jednotlivé tahy

`ids_solver.pl` - Implementace IDS algoritmu

`input2.pl` - Relevantní část zdrojového souboru `input2.pl` z informačního systému (kód v něm obsažený *není* mým dílem)

`main.pl` - Hlavní tělo programu

`Makefile` - Soubor pro program `make` s cíli pro překlad atd.

`test-moves.sh` - Testovací skript pro testování jednotlivých tahů

## Rozšíření

V rámci projektu byla implementována i dvě rozšíření, která nejsou uvedena v zadání:

*Podpora multithreadingu* - Program ve výchozím nastavení používá vícevláknové zpracování pro urychlení prohledávání stavového prostoru kostky (až 18 vláken). Pokud při spuštění programu použijeme přepínač `-c`, **deaktivujeme** tím prohledávání stavového prostoru pomocí více vláken. Vícevláknová verze IDS je implementována pomocí `first_solution` a je ji možné nalézt ve zdrojovém souboru `ids_solver.pl`.

    ```

    ```

*Výpis posloupnosti tahů* - Pokud použijeme při spuštění programu přepínač `-v`, program kromě stavů kostky během jeího řešení vypisuje také značky jednotlivých tahů (což je dobré pro ladění a případné složení fyzické Rubikovy kostky).


## Překlad a spuštění

Program přeložíme pomocí přiloženého programu `Makefile` a programu `make` (prerekvizitou je mít nainstalovaný `swipl`):

```
make
```


Testy pro ověření funkčnosti jednotlivých tahů můžeme poté spustit pomocí:

```
make test
```


Přeložený program spustíme následujícím způsobem:
```
./flp23-log <examples/from_assignment.txt
```

```
...

555
555
555
222 333 444 111
111 222 333 444
111 222 333 444
666
666
666

555
555
555
111 222 333 444
111 222 333 444
111 222 333 444
666
666
666
```


## Příklady




