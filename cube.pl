% cube(C) :- C = [
%         [
%             [2, 2, 5],
%             [1, 1, 5],
%             [1, 1, 5]
%         ],
%         [
%             [3, 2, 2],
%             [3, 2, 2],
%             [3, 2, 2]
%         ],
%         [
%             [6, 4, 4],
%             [6, 3, 3],
%             [6, 3, 3]
%         ],
%         [
%             [1, 1, 1],
%             [4, 4, 4],
%             [4, 4, 4]
%         ],
%         [
%             [5, 5, 3],
%             [5, 5, 3],
%             [5, 5, 4]
%         ],
%         [
%             [6, 6, 2],
%             [6, 6, 1],
%             [6, 6, 1]
%         ]
%     ].

% set_prolog_flag(answer_write_options,[max_depth(0)]).

cube(C) :- C = [
        [0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 ],
        [9 , 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

cube_size(3).


front(0).
right(1).
back(2).
left(3).
top(4).
bot(5).

side_num(4).

cube_max_i(I) :-
    cube_size(SIDE),
    I is SIDE - 1.

% cube_rotate_top_cw(C, NC) :- .

% rotate90([[H|T]|TT], [[H]|RMT]) :- .

% last([H], H) :- !.
% last([_|T], Last) :- last(T, Last). 

% shift_listr([H], [], H) :- !.
% shift_listr([H|T], [H|NT], Last) :- shift_listr(T, NT, Last).

% shift_listl([H], H, []) :- !.
% shift_listl([H|T], H, [NH|NT]) :- shift_listl(T, NH, NT).

% rotate_listr(L, [Last|NL]) :- shift_listr(L, NL, Last).

% rotate_listl(L, NL) :- shift_listl(L, NH, NT), append(NT, [NH], NL).

replace(_, [], _, _) :- !.
replace(I, N, [_|T], NL) :- I = 0, NL = [N|T], !.
replace(I, N, [H|T], [H|NT]) :- I > 0, NI is I - 1, replace(NI, N, T, NT).


column_indeces(CI, I) :-
    cube_size(SIZE),
    cube_max_i(MAX_I),
    between(0, MAX_I, X),
    I is CI + X * SIZE.



column(S, CI, COL_VALUES) :-
    findall(V, (column_indeces(CI, I), nth0(I, S, V)), COL_VALUES).





set_column(L, _, [], _, L) :- !.
set_column(L, _, _, [], L) :- !.
set_column([_|TS], I, [HCOL_V|TCOL_V], [HCOL_I|TCOL_I], [HCOL_V|NT]) :-
    I = HCOL_I, !,
    NI is I + 1,
    set_column(TS, NI, TCOL_V, TCOL_I, NT).
set_column([HS|TS], I, COL_VALUES, COL_INDECES, [HS|NT]) :-
    NI is I + 1,
    set_column(TS, NI, COL_VALUES, COL_INDECES, NT).

set_column(S, CI, COL_VALUES, NS) :-
    findall(I, (column_indeces(CI, I)), COL_INDECES),
    set_column(S, 0, COL_VALUES, COL_INDECES, NS).



between_rev(HIGH, LOW, X) :- between(LOW, HIGH, X_), X is HIGH - X_ + LOW. 


element(S, CI, RI, E) :- cube_size(SIDE), I is CI + RI * SIDE, nth0(I, S, E).



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


replace_by_list(_, _, [], []) :- !.
replace_by_list(_, [], L, L) :- !.
replace_by_list(I, [NH|NT], [_|T], NL) :- I = 0, replace_by_list(0, NT, T, NLT), NL = [NH|NLT], !.
replace_by_list(I, N, [H|T], [H|NT]) :- I > 0, NI is I - 1, replace_by_list(NI, N, T, NT).

slice(_, _, [], []) :- !.
slice(I, N, _, []) :- I = 0, N = 0, !.
slice(I, N, [H|LT], [H|ST]) :- I = 0, N > 0, NN is N - 1, slice(0, NN, LT, ST), !.
slice(I, N, [_|T], S) :- I > 0, NI is I - 1, slice(NI, N, T, S).


copy_row(RI, SRC_SIDE, DST_SIDE, NS) :-
    cube_size(SIZE),
    I is SIZE * RI,
    slice(I, SIZE, SRC_SIDE, SIDE_ROW),
    replace_by_list(I, SIDE_ROW, DST_SIDE, NS).


copy_col(CI, SRC_SIDE, DST_SIDE, NS) :-
    cube_size(SIZE),
    I is SIZE * RI,
    slice(I, SIZE, SRC_SIDE, SIDE_ROW),
    replace_by_list(I, SIDE_ROW, DST_SIDE, NS).


% Rotation of level in cube by 90° clockwise
rotate_hlevel_cv(C, LI, RC) :-
    side_num(SIDE_NUM),
    START_SIDE_I is 0,
    END_SIDE_I is SIDE_NUM - 1,
    findall(S,
        (
            between(START_SIDE_I, END_SIDE_I, SIDE_I_),
            SRC_SIDE_I is (SIDE_I_ - 1) mod SIDE_NUM,
            DST_SIDE_I is SIDE_I_ mod SIDE_NUM,
            nth0(SRC_SIDE_I, C, SRC_SIDE),
            nth0(DST_SIDE_I, C, DST_SIDE),
            copy_row(LI, SRC_SIDE, DST_SIDE, S)
        ), SIDES),
    replace_by_list(0, SIDES, C, RC).


rotate_hlevel_ccv(C, LI, RC) :-
    side_num(SIDE_NUM),
    START_SIDE_I is 0,
    END_SIDE_I is SIDE_NUM - 1,
    findall(S,
        (
            between(START_SIDE_I, END_SIDE_I, SIDE_I_),
            SRC_SIDE_I is (SIDE_I_ + 1) mod SIDE_NUM,
            DST_SIDE_I is SIDE_I_ mod SIDE_NUM,
            nth0(SRC_SIDE_I, C, SRC_SIDE),
            nth0(DST_SIDE_I, C, DST_SIDE),
            copy_row(LI, SRC_SIDE, DST_SIDE, S)
        ), SIDES),
    replace_by_list(0, SIDES, C, RC).


rotate_fcolumn_to_bot(C, CI, RC) :-
    side_num(SIDE_NUM),
    START_SIDE_I is 0,
    END_SIDE_I is SIDE_NUM - 1,
    findall(S,
        (
            between(START_SIDE_I, END_SIDE_I, SIDE_I_),
            SRC_SIDE_I is (SIDE_I_ + 1) mod SIDE_NUM,
            DST_SIDE_I is SIDE_I_ mod SIDE_NUM,
            nth0(SRC_SIDE_I, C, SRC_SIDE),
            nth0(DST_SIDE_I, C, DST_SIDE),
            copy_row(LI, SRC_SIDE, DST_SIDE, S)
        ), SIDES),
    replace_by_list(0, SIDES, C, RC).

% res(X) :- cube(C), nth0(8, C, X).

% cube_alt(C) :- C = [12321321221211111111111111111111111111111111111].


