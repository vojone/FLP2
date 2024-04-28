# FLP 23/24 - 2. projekt

## Téma

Rubikova kostka

## Autor

Vojtěch Dvořák (xdvora3o)

## Popis

Jedná se o program v jazyce Prolog, který řeší instance Rubikovy kostky.
Program očekává na vstupu Rubikovu kostku v zadáním předepsaném formátu (pro příklady viz níže nebo adresář `examples`) a na stadardní výstup vypíše posloupnost tahů vedoucí k jejímu vyřešení (složení).
Program rovněž kontroluje syntaktickou správnost formátu vstupu (zda každá strana obsahuje správný počet hodnot apod.) a pokud se mu nepodaří vstup zpracovat, vypíše chybové hlášení (viz příklady).

Interně je Rubikova kostka reprezentována pomocí dvojrozměrného seznamu o rozměrech 6 x 9 (6 stěn, z nichž každá má 9 barevných ploch).
Položky, reprezentující barevné plochy jsou v seznamech rozloženy následujícím způsobem:

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

`cube.pl` - Implementace tahů a čtení Rubikovy kostky ze vstupu

`ids_solver.pl` - Implementace IDS algoritmu

`input2.pl` - Relevantní část zdrojového souboru `input2.pl` z informačního systému (kód v něm obsažený *není* mým dílem)

`main.pl` - Hlavní tělo programu

`Makefile` - Soubor pro program `make` s cíli pro překlad, testování atd.

`test-moves.sh` - Testovací skript pro testování jednotlivých tahů

## Rozšíření

V rámci projektu byla implementována i dvě rozšíření, která nejsou uvedena v zadání:

*Podpora multithreadingu* - Možnost aktivovat vícevláknové zpracování pro urychlení prohledávání stavového prostoru kostky (až 18 vláken). Pokud při spuštění programu použijeme přepínač `-t`, **aktivujeme** prohledávání stavového prostoru pomocí více vláken. Vícevláknová verze IDS je implementována pomocí `first_solution` a je ji možné nalézt ve zdrojovém souboru `ids_solver.pl`. Např. při spuštění programu nad souborem `examples/6moves.txt` můžeme pozorovat výrazné zrychlení:

    ```
    time ./flp23-log -t <examples/6moves.txt
    ```
    
    ```
    ...
    real    0m8.212s
    ...
    ```

    ```
    time ./flp23-log <examples/6moves.txt
    ```

    ```
    ...
    real    0m45.175s
    ...
    ```

    Ve výchozím stavu je však multithreading deaktivován, neboť při testování na serveru Merlin docházelo k vyčerpání limitu procesorového času (řešení je sice nalezeno rychleji, nicméně efektivita je nižší a bylo tedy snazší limit vyčerpat). 

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

Příklady jsou umistěny v adresáři `examples`.

Pomocí `input_error.txt` můžeme otestovat reakci na nevalidní vstup:

```
./flp23-log <examples/input_error.txt
```
```
flp23-log: Error: Invalid input!
```

Další soubory obsahují instance rozmíchané Rubikovy kostky (`1moves.txt` jeden tak od složení, `2moves.txt` dva tahy od složení apod.).
Časy řešení vybraných (složitějších) úloh a příkladové plohy ze zadání jsou přibližně následující (měřeno na serveru Merlin pomocí nástroje `time` s deaktivovaným multithreadingem):

`examples/from_assignment.txt`
```
real    0m0.015s
user    0m0.010s
sys     0m0.003s
```

`examples/5moves.txt`
```
real    0m3.427s
user    0m3.262s
sys     0m0.163s
```

`examples/6moves.txt`
```
real    0m47.025s
user    0m44.643s
sys     0m2.397s
```

`examples/7moves.txt`
```
real    9m1.381s
user    8m47.925s
sys     0m13.389s
```


