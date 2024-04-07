
/*** The following code was taken from input2.pl, autor: Martin Hyrs **/

/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].



/** End of part taken from input2.pl **/

line2row(L, I, L, []) :- cube_size(SIZE), I = SIZE, !.
line2row([H|T], I, NLT, [H|RT]) :-
    cube_size(SIZE),
    I < SIZE, !,
    NI is I + 1,
    line2row(T, NI, NLT, RT).
line2row(L, NL, R) :- line2row(L, 0, NL, R).


lines2side(L, I, L, []) :- cube_size(SIZE), I = SIZE, !.
lines2side([H|T], I, [REM|NL], [NR|ST]) :-
    cube_size(SIZE),
    I < SIZE,
    line2row(H, REM, NR),
    NI is I + 1,
    lines2side(T, NI, NL, ST).

lines2side(L, NL, SIDE) :-
    lines2side(L, 0, NL, SIDE_),
    flatten(SIDE_, SIDE).


pop_empty_lines([], I, []) :- I = 0, !.
pop_empty_lines(L, I, L) :- cube_size(SIZE), I = SIZE, !.
pop_empty_lines([[]|T], I, NT) :-
    cube_size(SIZE), I < SIZE, NI is I + 1, !, pop_empty_lines(T, NI, NT).
pop_empty_lines(L, NL) :-
    pop_empty_lines(L, 0, NL).

skip_whitespace([], I, []) :- I = 0, !.
%pop_empty_lines([[]|T], I, [[]|T]) :- I = 0, !.
skip_whitespace(L, I, L) :- cube_size(SIZE), I = SIZE, !.
skip_whitespace([[H|T]|TT], I, [T|NT]) :-
    cube_size(SIZE), I < SIZE, NI is I + 1, H = ' ', !, skip_whitespace(TT, NI, NT).
skip_whitespace(L, NL) :-
    skip_whitespace(L, 0, NL).

lines2cube(L, I, NL, [SIDE]) :-
    round_side_num(ROUND_SIDE_NUM),
    I =:= ROUND_SIDE_NUM - 1, !,
    lines2side(L, NL, SIDE).
lines2cube(L, I, NNNL, [SIDE|ST]) :-
    round_side_num(ROUND_SIDE_NUM),
    I < ROUND_SIDE_NUM, !,
    NI is I + 1,
    lines2side(L, NL, SIDE),
    skip_whitespace(NL, NNL),
    lines2cube(NNL, NI, NNNL, ST).

lines2cube(L, REM, C) :-
    lines2side(L, L_, TOP_SIDE),
    pop_empty_lines(L_, NL),
    lines2cube(NL, 0, NNL, SIDES),
    pop_empty_lines(NNL, NNNL),
    lines2side(NNNL, NNNNL, BOT_SIDE),
    pop_empty_lines(NNNNL, REM),
    append(SIDES, [TOP_SIDE], C_),
    append(C_, [BOT_SIDE], C).

read_cube(C, NL) :-
    cube_size(SIZE),
    EXP_LINES is SIZE * SIZE,
    read_lines2(L, EXP_LINES),
    lines2cube(L, NL, C).


% The mock cube for the testing
cube(C) :- C = [
        [0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 ],
        [9 , 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

cube_s(C) :- C = [[2,2,5,1,1,5,1,1,5],[3,2,2,3,2,2,3,2,2],[6,4,4,6,3,3,6,3,3],[1,1,1,4,4,4,4,4,4],[5,5,3,5,5,3,5,5,4],[6,6,2,6,6,1,6,6,1]].

done_cube(C) :- C = [
        [1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1],
        [1,1,1,1,1,1,1,1,1]
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



onecolor([]) :- !.
onecolor([_]) :- !.
onecolor([H1,H2|T]) :- H1 = H2, onecolor([H2|T]).

cube_done([]) :- !. 
cube_done([H|T]) :- onecolor(H), cube_done(T).


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


hlevel_cw(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0,
    END_I is ROUND_SIDE_NUM - 1,
    findall(I,
        (
            between(START_I, END_I, I_),
            I is (I_ + OFFSET) mod ROUND_SIDE_NUM
        ), SIDE_INDECES).


hlevel_ccw(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    hlevel_cw(OFFSET_REV, SIDE_INDECES_),
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


copy_row(RI, SRC_SIDE, _, DST_SIDE, _, NS) :-
    cube_size(SIZE),
    I is SIZE * RI,
    slice(I, SIZE, SRC_SIDE, SIDE_ROW),
    replace_by_list(I, SIDE_ROW, DST_SIDE, NS).


copy_col(CI, SRC_SIDE, _, DST_SIDE, DST_SIDE_I, NS) :-
    back(BACK_I), left(LEFT_I), memberchk(DST_SIDE_I, [BACK_I, LEFT_I]), !,
    cube_max_i(MAX_I),
    INV_CI is MAX_I - CI,
    column(SRC_SIDE, CI, VALUES_REV),
    reverse(VALUES_REV, VALUES),
    set_column(DST_SIDE, INV_CI, VALUES, NS).

copy_col(CI, SRC_SIDE, SRC_SIDE_I, DST_SIDE, _, NS) :-
    back(BACK_I), left(LEFT_I), memberchk(SRC_SIDE_I, [BACK_I, LEFT_I]), !,
    cube_max_i(MAX_I),
    INV_CI is MAX_I - CI,
    column(SRC_SIDE, INV_CI, VALUES_REV),
    reverse(VALUES_REV, VALUES),
    set_column(DST_SIDE, CI, VALUES, NS).

copy_col(CI, SRC_SIDE, _, DST_SIDE, _, NS) :-
    column(SRC_SIDE, CI, VALUES),
    set_column(DST_SIDE, CI, VALUES, NS).


rotate(C, DST_GEN, SRC_GEN, COPY, RC) :-
    round_side_num(ROUND_SIDE_NUM),
    call(DST_GEN, SRC_SIDE_INDECES),
    call(SRC_GEN, DST_SIDE_INDECES),
    ROUND_SIDE_NUM_ is ROUND_SIDE_NUM - 1,
    findall(S,
        (
            between(0, ROUND_SIDE_NUM_, I),
            nth0(I, SRC_SIDE_INDECES, SRC_SIDE_I),
            nth0(I, DST_SIDE_INDECES, DST_SIDE_I),
            nth0(SRC_SIDE_I, C, SRC_SIDE),
            nth0(DST_SIDE_I, C, DST_SIDE),
            call(COPY, SRC_SIDE, SRC_SIDE_I, DST_SIDE, DST_SIDE_I, S)
        ), SIDES),
    msetv(C, DST_SIDE_INDECES, SIDES, RC).


rotate_hlevel_cw(C, RI, RC) :-
    rotate(C, hlevel_cw(-1), hlevel_cw(0), copy_row(RI), RC).

rotate_hlevel_ccw(C, RI, RC) :-
    rotate(C, hlevel_ccw(-1), hlevel_ccw(0), copy_row(RI), RC).


rotate_front_col_to_bot(C, CI, RC) :-
    rotate(C, front_col_bot(-1), front_col_bot(0), copy_col(CI), RC).

rotate_front_col_to_top(C, CI, RC) :-
    rotate(C, front_col_top(-1), front_col_top(0), copy_col(CI), RC).


rotate_side_col_to_bot(C, CI, RC) :-
    rotate(C, side_col_bot(-1), side_col_bot(0), copy_col(CI), RC).

rotate_side_col_to_top(C, CI, RC) :-
    rotate(C, side_col_top(-1), side_col_top(0), copy_col(CI), RC).


rotate_cube_side_cw(C, SIDE_I, RC) :-
    nth0(SIDE_I, C, SIDE),
    rotate_side_cw(SIDE, RSIDE),
    setv(C, SIDE_I, RSIDE, RC).


rotate_cube_side_ccw(C, SIDE_I, RC) :-
    nth0(SIDE_I, C, SIDE),
    rotate_side_ccw(SIDE, RSIDE),
    setv(C, SIDE_I, RSIDE, RC).


check_moves(_, []) :- !.
check_moves(M, [H|_]) :- ((M = H) -> false; true).

move_u_cw(C, M, I, [(u, I)|M], RC) :-
    top(TOP_I),
    rotate_cube_side_cw(C, TOP_I, RC_),
    rotate_hlevel_cw(RC_, 0, RC).
move_u_ccw(C, M, I, [(uc, I)|M], RC) :-
    top(TOP_I),
    rotate_cube_side_ccw(C, TOP_I, RC_),
    rotate_hlevel_ccw(RC_, 0, RC).


move_d_cw(C, M, I, [(d, I)|M], RC) :-
    cube_size(SIZE),
    RI is SIZE - 1,
    bot(BOT_I),
    rotate_cube_side_cw(C, BOT_I, RC_),
    rotate_hlevel_cw(RC_, RI, RC).
move_d_ccw(C, M, I, [(dc, I)|M], RC) :-
    cube_size(SIZE),
    RI is SIZE - 1,
    bot(BOT_I),
    rotate_cube_side_ccw(C, BOT_I, RC_),
    rotate_hlevel_ccw(RC_, RI, RC).

move_r_cw(C, M, I, [(r, I)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    right(RIGHT_I),
    rotate_cube_side_cw(C, RIGHT_I, RC_),
    rotate_front_col_to_top(RC_, CI, RC).
move_r_ccw(C, M, I, [(rc, I)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    right(RIGHT_I),
    rotate_cube_side_ccw(C, RIGHT_I, RC_),
    rotate_front_col_to_bot(RC_, CI, RC).


move_l_cw(C, M, I, [(l, I)|M], RC) :-
    left(LEFT_I),
    rotate_cube_side_cw(C, LEFT_I, RC_),
    rotate_front_col_to_bot(RC_, 0, RC).
move_l_ccw(C, M, I, [(l, I)|M], RC) :-
    left(LEFT_I),
    rotate_cube_side_ccw(C, LEFT_I, RC_),
    rotate_front_col_to_top(RC_, 0, RC).


move_f_cw(C, M, I, [(f, I)|M], RC) :-
    front(FRONT_I),
    rotate_cube_side_cw(C, FRONT_I, RC_),
    rotate_side_col_to_bot(RC_, 0, RC).
move_f_ccw(C, M, I, [(fc, I)|M], RC) :-
    front(FRONT_I),
    rotate_cube_side_ccw(C, FRONT_I, RC_),
    rotate_side_col_to_top(RC_, 0, RC).


move_b_cw(C, M, I, [(b, I)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    front(BACK_I),
    rotate_cube_side_cw(C, BACK_I, RC_),
    rotate_side_col_to_top(RC_, CI, RC).
move_b_ccw(C, M, I, [(bc, I)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    front(BACK_I),
    rotate_cube_side_ccw(C, BACK_I, RC_),
    rotate_side_col_to_bot(RC_, CI, RC).


add_state(C, 1) :- asserta(cube_state(C, 0)).
add_state(C, I, NI) :- asserta(cube_state(C, I)), NI is I + 1.

check_state(C) :- cube_state(C, _), !, false.
check_state(_) :- true.

clear_states :- retractall(cube_state(_, _)).

init_state(C) :- cube_state(C, 0).

nonopt(_, _, _, I) :- I = 0, !.
nonopt(M, NON_CONF, [(H, _)|T], I) :- memberchk(H, NON_CONF), !, nonopt(M, NON_CONF, T, I).
nonopt(M, NON_CONF, [(H, _)|T], I) :- M = H, I > 0, NI is I - 1, nonopt(M, NON_CONF, T, NI).
nonopt(M, NON_CONF, ML) :- nonopt(M, NON_CONF, ML, 3).

popn(_, N, L, L) :- N = 0, !.
popn(M, N, [(H, I)|T], [(H, I)|NT]) :- M \= H, !, popn(M, N, T, NT).
popn(M, N, [(H, _)|T], NT) :- M = H, N > 0, NN is N - 1, popn(M, NN, T, NT).


optimize(M, NON_CONF, NM, I, ML, OM) :- nonopt(M, NON_CONF, ML), !, popn(M, 3, ML, OM_), OM = [(NM, I)|OM_].
optimize(_, _, _, _, ML, ML) :- !.


move(C, M, I, M, C, I, _) :- cube_done(C), !.
move(C, M, I, RM, RC, RI, D) :- D > 0, move_u_cw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_u_ccw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_d_cw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_d_ccw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_r_cw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_r_ccw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_l_cw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).
move(C, M, I, RM, RC, RI, D) :- D > 0, move_l_ccw(C, M, I, M_, RC_), ND is D - 1, add_state(RC_, I, I_), move(RC_, M_, I_, RM, RC, RI, ND).

move_all(C, I, RM, RC, RI, D) :-
    move(C, [], I, RM, RC, RI, D);
    init_state(IC),
    clear_states,
    add_state(IC, NI),
    ND is D + 1,
    move_all(C, NI, RM, RC, RI, ND).

cube_fix(C, FC) :-
    back(BACK_I),
    left(LEFT_I),
    rotate_cube_side_cw(C, BACK_I, C1),
    rotate_cube_side_cw(C1, BACK_I, C2),
    rotate_cube_side_cw(C2, LEFT_I, C3),
    rotate_cube_side_cw(C3, LEFT_I, FC).

solve(RC, RM, RI) :-
    %cube_s(C),
    read_cube(C, NL),
    add_state(C, NI),
    move_all(C, NI, RM, RC, RI, 0).

% 555
% 555
% 555
% 444 111 222 333
% 111 222 333 444
% 111 222 333 444
% 666
% 666
% 666
