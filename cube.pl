
% The mock cube for the testing
cube(C) :- C = [
        [0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 ],
        [9 , 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

% Cube side size
cube_size(3).


front(0).
right(1).
back(2).
left(3).
top(4).
bot(5).

side_num(6).

round_side_num(4).

cube_max_i(I) :-
    cube_size(SIDE),
    I is SIDE - 1.




replace(_, [], _, _) :- !.
replace(I, N, [_|T], NL) :- I = 0, NL = [N|T], !.
replace(I, N, [H|T], [H|NT]) :- I > 0, NI is I - 1, replace(NI, N, T, NT).


setv([], _, _, []) :- !.
setv([_|T], I, V, [V|T]) :- I = 0, !.
setv([H|T], I, V, [H|NT]) :- I > 0, NI is I - 1, setv(T, NI, V, NT).



msetv(L, [], _, L) :- !.
msetv(L, _, [], L) :- !.
msetv(L, [HI|TI], [HVAL|TVAL], NL) :-
    setv(L, HI, HVAL, NL_),
    msetv(NL_, TI, TVAL, NL).


set_column(S, CI, COL_VALUES, NS) :-
    findall(I, (column_indeces(CI, I)), COL_INDECES),
    msetv(S, COL_INDECES, COL_VALUES, NS).



between_rev(HIGH, LOW, X) :- between(LOW, HIGH, X_), X is HIGH - X_ + LOW. 


element(S, CI, RI, E) :- cube_size(SIDE), I is CI + RI * SIDE, nth0(I, S, E).


replace_by_list(_, _, [], []) :- !.
replace_by_list(_, [], L, L) :- !.
replace_by_list(I, [NH|NT], [_|T], NL) :- I = 0, replace_by_list(0, NT, T, NLT), NL = [NH|NLT], !.
replace_by_list(I, N, [H|T], [H|NT]) :- I > 0, NI is I - 1, replace_by_list(NI, N, T, NT).


slice(_, _, [], []) :- !.
slice(I, N, _, []) :- I = 0, N = 0, !.
slice(I, N, [H|LT], [H|ST]) :- I = 0, N > 0, NN is N - 1, slice(0, NN, LT, ST), !.
slice(I, N, [_|T], S) :- I > 0, NI is I - 1, slice(NI, N, T, S).


column_indeces(CI, I) :-
    cube_size(SIZE),
    cube_max_i(MAX_I),
    between(0, MAX_I, X),
    I is CI + X * SIZE.



column(S, CI, COL_VALUES) :-
    findall(V, (column_indeces(CI, I), nth0(I, S, V)), COL_VALUES).


front_col_bot(OFFSET, SIDE_INDECES) :-
    side_num(SIDE_NUM),
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0,
    END_I is ROUND_SIDE_NUM - 1,
    findall(I,
        (
            between(START_I, END_I, I_),
            X is (I_ + OFFSET) mod ROUND_SIDE_NUM,
            I is (X mod 2) * (SIDE_NUM - ((X + 1) div 2)) + ((X + 1) mod 2) * X
        ), SIDE_INDECES).


front_col_top(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    front_col_bot(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).


side_col_top(OFFSET, SIDE_INDECES) :-
    side_num(SIDE_NUM),
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0,
    END_I is ROUND_SIDE_NUM - 1,
    findall(I,
        (
            between(START_I, END_I, I_),
            X is (I_ + OFFSET) mod ROUND_SIDE_NUM,
            ODD_ALT is ((X mod 2) * (SIDE_NUM - (SIDE_NUM - X) div 2)),
            EVENT_ALT is ((X + 1) mod 2) * (X + 1),
            I is ODD_ALT + EVENT_ALT
        ), SIDE_INDECES).


side_col_bot(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    side_col_top(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).


hlevel_cv(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0,
    END_I is ROUND_SIDE_NUM - 1,
    findall(I,
        (
            between(START_I, END_I, I_),
            I is (I_ + OFFSET) mod ROUND_SIDE_NUM
        ), SIDE_INDECES).


hlevel_ccv(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    hlevel_cv(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).



% Rotation of side (plane/matrix) by 90° clockwise
% S - Side (plane)
% RS - Rotated plane
rotate_side_cw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between(0, MAX_I, CI), % For each column from 0 to MAX_I (included)
            between_rev(MAX_I, 0, RI), % For each row from MAX_I to 0 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


% Rotation of side (plane/matrix) by 90° COUNTER clockwise
% S - Side (plane)
% RS - Rotated plane
rotate_side_ccw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between_rev(MAX_I, 0, CI), % For each column from MAX_I to 0 (included)
            between(0, MAX_I, RI), % For each row from 0 to MAX_I 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


copy_row(RI, SRC_SIDE, DST_SIDE, NS) :-
    cube_size(SIZE),
    I is SIZE * RI,
    slice(I, SIZE, SRC_SIDE, SIDE_ROW),
    replace_by_list(I, SIDE_ROW, DST_SIDE, NS).


copy_col(CI, SRC_SIDE, DST_SIDE, NS) :-
    column(SRC_SIDE, CI, VALUES),
    set_column(DST_SIDE, CI, VALUES, NS).


rotate(C, DST_GEN, SRC_GEN, COPY, RC) :-
    round_side_num(ROUND_SIDE_NUM),
    call(DST_GEN, SRC_SIDE_INDECES),
    call(SRC_GEN, DST_SIDE_INDECES),
    findall(S,
        (
            between(0, ROUND_SIDE_NUM, I),
            nth0(I, SRC_SIDE_INDECES, SRC_SIDE_I),
            nth0(I, DST_SIDE_INDECES, DST_SIDE_I),
            nth0(SRC_SIDE_I, C, SRC_SIDE),
            nth0(DST_SIDE_I, C, DST_SIDE),
            call(COPY, SRC_SIDE, DST_SIDE, S)
        ), SIDES),
    msetv(C, DST_SIDE_INDECES, SIDES, RC).


rotate_hlevel_cv(C, RI, RC) :-
    rotate(C, hlevel_cv(-1), hlevel_cv(0), copy_row(RI), RC).

rotate_hlevel_ccv(C, RI, RC) :-
    rotate(C, hlevel_ccv(-1), hlevel_ccv(0), copy_row(RI), RC).


rotate_front_col_to_bot(C, CI, RC) :-
    rotate(C, front_col_bot(-1), front_col_bot(0), copy_col(CI), RC).

rotate_front_col_to_top(C, CI, RC) :-
    rotate(C, front_col_top(-1), front_col_top(0), copy_col(CI), RC).


rotate_side_col_to_bot(C, CI, RC) :-
    rotate(C, side_col_bot(-1), side_col_bot(0), copy_col(CI), RC).

rotate_side_col_to_top(C, CI, RC) :-
    rotate(C, side_col_top(-1), side_col_top(0), copy_col(CI), RC).


