/**
* flp-23-log
* cube.pl
*
* Implementation of manipulations with the rubikscube. All implemented
* manipulations presumes that the following representation is used:
* cube(C) :- C = [
*     [1,1,1,1,1,1,1,1,1],
*     [2,2,2,2,2,2,2,2,2],
*     [3,3,3,3,3,3,3,3,3],
*     [4,4,4,4,4,4,4,4,4],
*     [5,5,5,5,5,5,5,5,5],
*     [6,6,6,6,6,6,6,6,6]
* ].
*
* (2D list, where the outer list represents the whole rubikscube and the inner
* one represents individual sides of the cube)
*
* One of the main goals of implementation was to be general - allow
* manipulation with 2x2, 4x4, 5x5 cubes
*
* Author: Vojtech Dvorak (xdvora3o)
*/

% Cube side size
cube_size(3).

% Idexes of cube sides
front(0).
right(1).
back(2).
left(3).
top(4).
bot(5).

% Number of cube sides (we use classic 3x3 cube by default)
side_num(6).

% How many sides are around the perimeter of the cube
round_side_num(4).

% Max index of row/column in one side
cube_max_i(I) :-
    cube_size(SIDE),
    I is SIDE - 1.


/***         Clauses for reading a 3x3 cube from the stdin                  **/
:- consult("input2").


% line2row(inline, line_remainder, row)
% Convert textual line to row of the side of rubikscube
line2row(L, I, L, []) :- cube_size(SIZE), I = SIZE, !.
line2row([H|T], I, NLT, [HN|RT]) :-
    cube_size(SIZE),
    I < SIZE, !,
    atom_number(H, HN),
    NI is I + 1,
    line2row(T, NI, NLT, RT).
line2row(L, NL, R) :- line2row(L, 0, NL, R).


% line2row(inline, line_remainder, side)
% Convert textual line to a side of rubikscube
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


% pop_empty_lines(lines_list, outlinelist)
% Remove speicfied amount of empty line lists from the start of the line list
pop_empty_lines([], I, []) :- I = 0, !.
pop_empty_lines(L, I, L) :- cube_size(SIZE), I = SIZE, !.
pop_empty_lines([[]|T], I, NT) :-
    cube_size(SIZE), I < SIZE, NI is I + 1, !, pop_empty_lines(T, NI, NT).
pop_empty_lines(L, NL) :-
    pop_empty_lines(L, 0, NL).


% skip_whitespace(line, outline)
% Skip whitespace (no newline) at start of the line
skip_whitespace([], I, []) :- I = 0, !.
%pop_empty_lines([[]|T], I, [[]|T]) :- I = 0, !.
skip_whitespace(L, I, L) :- cube_size(SIZE), I = SIZE, !.
skip_whitespace([[H|T]|TT], I, [T|NT]) :-
    cube_size(SIZE), I < SIZE, NI is I + 1, (H = ' '; H = '\t'), !,
    skip_whitespace(TT, NI, NT).
skip_whitespace(L, NL) :-
    skip_whitespace(L, 0, NL).


% lines2cube(line, line_remainder, cube)
% Convert lines to the rubikscube
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


% read_cube(result_cube, line_remainder)
% Reads rubikscube from stdin in the followint format:
% 555
% 555
% 555
% 222 333 444 111
% 222 333 444 111
% 444 111 222 333
% 666
% 666
% 666
read_cube(C, NL) :-
    cube_size(SIZE),
    EXP_LINES is SIZE * SIZE,
    read_lines2(L, EXP_LINES),
    lines2cube(L, NL, C).



/**                 Clauses for writing a cube to stdout                    **/


% write_row(side, row_index)
% Write one row of given side specififed by the index to the stdout
write_row(_, _, I) :- cube_size(SIZE), I >= SIZE, !. 
write_row(SIDE, RI, I) :-
    cube_size(SIZE),
    I_ is RI * SIZE + I,
    nth0(I_, SIDE, E),
    write(E),
    NI is I + 1,
    write_row(SIDE, RI, NI).
write_row(SIDE, RI) :- write_row(SIDE, RI, 0).


% write_side(side)
% Write given to the stdout
write_side(_, RI) :- cube_size(SIZE), RI >= SIZE, !. 
write_side(SIDE, RI) :- cube_size(SIZE), RI < SIZE, !,
    write_row(SIDE, RI), write("\n"), NRI is RI + 1, write_side(SIDE, NRI). 
write_side(SIDE) :- write_side(SIDE, 0).


% write_round_sides(cube)
% Write sides around the perimeter of the cube to stdout
write_round_sides(_, RI) :- cube_size(SIZE), RI >= SIZE, !. 
write_round_sides([FRONT, RIGHT, BACK, LEFT, _, _], RI) :-
    cube_size(SIZE), RI < SIZE, !, 
    write_row(FRONT, RI), write(" "),
    write_row(RIGHT, RI), write(" "),
    write_row(BACK, RI), write(" "),
    write_row(LEFT, RI), write("\n"),
    NRI is RI + 1, write_round_sides([FRONT, RIGHT, BACK, LEFT, _, _], NRI). 
write_round_sides(C) :- write_round_sides(C, 0).


% write_cube(cube)
% Write the whole cube to the stdout
write_cube([FRONT, RIGHT, BACK, LEFT, TOP, BOT]) :-
    write_side(TOP),
    write_round_sides([FRONT, RIGHT, BACK, LEFT, TOP, BOT]),
    write_side(BOT).




/**               Clauses for checking the state of a cube                  **/

side_color(_, []) :- !.
side_color(COLOR, [COLOR|T]) :- side_color(COLOR, T).


cube_done([]) :- !. 
cube_done([FRONT, RIGHT, BACK, LEFT, TOP, BOT]) :-
    side_color(1, FRONT),
    side_color(2, RIGHT),
    side_color(3, BACK),
    side_color(4, LEFT),
    side_color(5, TOP),
    side_color(6, BOT).



/**                              Auxiliary clauses                          **/


% setv(inlist, I, V, outlist)
% Set value in list at index I to value V
setv([], _, _, []) :- !.
setv([_|T], I, V, [V|T]) :- I = 0, !.
setv([H|T], I, V, [H|NT]) :- I > 0, NI is I - 1, setv(T, NI, V, NT).


% msetv(inlist, indeces, values, outlist)
% Set values in list at given indeces to given values
msetv(L, [], _, L) :- !.
msetv(L, _, [], L) :- !.
msetv(L, [HI|TI], [HVAL|TVAL], NL) :-
    setv(L, HI, HVAL, NL_),
    msetv(NL_, TI, TVAL, NL).


% between_rev(high, low, number)
% Reverse between - generates number from HIGH to LOW
between_rev(HIGH, LOW, X) :- between(LOW, HIGH, X_), X is HIGH - X_ + LOW. 


% replace_by_list(index, targetlist, sourcelist, outlist)
% Replace the part of list starting at index I by another list (or its part)
replace_by_list(_, _, [], []) :- !.
replace_by_list(_, [], L, L) :- !.
replace_by_list(I, [NH|NT], [_|T], NL) :- I = 0, replace_by_list(0, NT, T, NLT), NL = [NH|NLT], !.
replace_by_list(I, N, [H|T], [H|NT]) :- I > 0, NI is I - 1, replace_by_list(NI, N, T, NT).


% slice(index, len, sourcelist, slice)
% Get the part of the given list starting at index I with the specified length 
slice(_, _, [], []) :- !.
slice(I, N, _, []) :- I = 0, N = 0, !.
slice(I, N, [H|LT], [H|ST]) :- I = 0, N > 0, NN is N - 1, slice(0, NN, LT, ST), !.
slice(I, N, [_|T], S) :- I > 0, NI is I - 1, slice(NI, N, T, S).


/**                Clauses for manipulating with the cube                   **/


% Generators of number


% column_indeces(column_index, i)
% Generate indeces of elements of column with given index 
column_indeces(CI, I) :-
    cube_size(SIZE),
    cube_max_i(MAX_I),
    between(0, MAX_I, X),
    I is CI + X * SIZE.


% front_col_bot(offset, side_indeces)
% Create list of indeces of sides for rotation of front columns to bot
% (ccw when right side is on the front)
front_col_bot(OFFSET, SIDE_INDECES) :-
    side_num(SIDE_NUM),
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0, % Start index
    END_I is ROUND_SIDE_NUM - 1, % End index
    findall(I,
        (
            between(START_I, END_I, I_), % Generate indeces between START_I and END_I
            X is (I_ + OFFSET) mod ROUND_SIDE_NUM,
            I is (X mod 2) * (SIDE_NUM - ((X + 1) div 2)) + ((X + 1) mod 2) * X % Calculation of side index
        ), SIDE_INDECES).


% front_col_top(offset, side_indeces)
% Create list of indeces of sides for rotation of front columns to top
% (cw when right side is on the front)
front_col_top(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    front_col_bot(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).


% side_col_top(offset, side_indeces)
% Create list of indeces of sides for rotation of side columns to top
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


% side_col_bot(offset, side_indeces)
% Create list of indeces of sides for rotation of side columns to bot
side_col_bot(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    side_col_top(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).


% hlevel_cw(offset, side_indeces)
% Create list of side indeces for rotation of horizontal level of the cube cw
% (from the perspective of the top side)
hlevel_cw(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    START_I is 0,
    END_I is ROUND_SIDE_NUM - 1,
    findall(I,
        (
            between(START_I, END_I, I_),
            I is (I_ + OFFSET) mod ROUND_SIDE_NUM
        ), SIDE_INDECES).


% hlevel_ccw(offset, side_indeces)
% Create list of side indeces for rotation of horizontal level of the cube ccw
% (from the perspective of the top side)
hlevel_ccw(OFFSET, SIDE_INDECES) :-
    round_side_num(ROUND_SIDE_NUM),
    OFFSET_REV is ROUND_SIDE_NUM - OFFSET + 1,
    hlevel_cw(OFFSET_REV, SIDE_INDECES_),
    reverse(SIDE_INDECES_, SIDE_INDECES).





% element(side, column_index, row_index, element).
% Get the element in row with idex RI and in column with index CI of the
% given side of the cube
element(S, CI, RI, E) :- cube_size(SIDE), I is CI + RI * SIDE, nth0(I, S, E).


% set_column(inside, cloumn_index, values, outside)
% Set column with index CI in the given side to the new values
set_column(S, CI, COL_VALUES, NS) :-
    findall(I, (column_indeces(CI, I)), COL_INDECES),
    msetv(S, COL_INDECES, COL_VALUES, NS).



% column(side, column_index, values)
% Get values of column with specified index in given side
column(S, CI, COL_VALUES) :-
    findall(V, (column_indeces(CI, I), nth0(I, S, V)), COL_VALUES).



% rotate_side_cw(side, rotated_side)
% Perform rotation of side (plane/matrix) by 90° clockwise
rotate_side_cw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between(0, MAX_I, CI), % For each column from 0 to MAX_I (included)
            between_rev(MAX_I, 0, RI), % For each row from MAX_I to 0 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


% rotate_side_ccw(side, rotated_side)
% Perform rotation of side (plane/matrix) by 90° COUNTER clockwise
rotate_side_ccw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between_rev(MAX_I, 0, CI), % For each column from MAX_I to 0 (included)
            between(0, MAX_I, RI), % For each row from 0 to MAX_I 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


% copy_row(row_index, source_side, _, dst_side, _, outside)
% Copy row with given index from source side to destination side (there are
% two unused params to make this clause compatible with copy_col clause)
copy_row(RI, SRC_SIDE, _, DST_SIDE, _, NS) :-
    cube_size(SIZE),
    I is SIZE * RI,
    slice(I, SIZE, SRC_SIDE, SIDE_ROW),
    replace_by_list(I, SIDE_ROW, DST_SIDE, NS).


% copy_col(row_index, source_side, _, dst_side, _, outside)
% Copy columns with given index from source side to destination side
copy_col(CI, SRC_SIDE, _, DST_SIDE, DST_SIDE_I, NS) :-
    back(BACK_I), left(LEFT_I), memberchk(DST_SIDE_I, [BACK_I, LEFT_I]), !,
    cube_max_i(MAX_I),
    INV_CI is MAX_I - CI, % If dst side is left or back, inverse the index
    column(SRC_SIDE, CI, VALUES_REV),
    reverse(VALUES_REV, VALUES),
    set_column(DST_SIDE, INV_CI, VALUES, NS).

copy_col(CI, SRC_SIDE, SRC_SIDE_I, DST_SIDE, _, NS) :-
    back(BACK_I), left(LEFT_I), memberchk(SRC_SIDE_I, [BACK_I, LEFT_I]), !,
    cube_max_i(MAX_I),
    INV_CI is MAX_I - CI, % If source side is left or back, inverse the index
    column(SRC_SIDE, INV_CI, VALUES_REV),
    reverse(VALUES_REV, VALUES),
    set_column(DST_SIDE, CI, VALUES, NS).

copy_col(CI, SRC_SIDE, _, DST_SIDE, _, NS) :-
    column(SRC_SIDE, CI, VALUES),
    set_column(DST_SIDE, CI, VALUES, NS).


% rotate(cube, dst_generator, source_generator, copy_f, rotated_cube)
% Circular copy of the part of the cube (row/col) from one side to another,
% dst sides and sources sides are specififed by lists and copying itself
% is perormed by given copy function
rotate(C, DST_GEN, SRC_GEN, COPY, RC) :-
    round_side_num(ROUND_SIDE_NUM),
    call(DST_GEN, SRC_SIDE_INDECES), % Create list with indeces of source sides
    call(SRC_GEN, DST_SIDE_INDECES), % Create list with indeces of destination sides
    ROUND_SIDE_NUM_ is ROUND_SIDE_NUM - 1,
    findall(S,
        (
            between(0, ROUND_SIDE_NUM_, I),
            nth0(I, SRC_SIDE_INDECES, SRC_SIDE_I),
            nth0(I, DST_SIDE_INDECES, DST_SIDE_I),
            nth0(SRC_SIDE_I, C, SRC_SIDE), % Take side due to index
            nth0(DST_SIDE_I, C, DST_SIDE),
            call(COPY, SRC_SIDE, SRC_SIDE_I, DST_SIDE, DST_SIDE_I, S) % Copy part of the side to another
        ), SIDES),
    msetv(C, DST_SIDE_INDECES, SIDES, RC). % Replace updated sides in the cube


% rotate_hlevel_cw(cube, row_index, rotated_cube)
% Rotate horizontal level of the cube specified by row index clockwise
rotate_hlevel_cw(C, RI, RC) :-
    rotate(C, hlevel_cw(-1), hlevel_cw(0), copy_row(RI), RC).

% rotate_hlevel_ccw(cube, row_index, rotated_cube)
% Rotate horizontal level of the cube specified by row index counterclockwise
rotate_hlevel_ccw(C, RI, RC) :-
    rotate(C, hlevel_ccw(-1), hlevel_ccw(0), copy_row(RI), RC).


% rotate_front_col_to_bot(cube, column_index, rotated_cube)
% Rotate front column of the cube specified by column index to bot
rotate_front_col_to_bot(C, CI, RC) :-
    rotate(C, front_col_bot(-1), front_col_bot(0), copy_col(CI), RC).


% rotate_front_col_to_top(cube, column_index, rotated_cube)
% Rotate front column of the cube specified by column index to top
rotate_front_col_to_top(C, CI, RC) :-
    rotate(C, front_col_top(-1), front_col_top(0), copy_col(CI), RC).


% rotate_side_col_to_bot(cube, column_index, rotated_cube)
% Rotate side column of the cube specified by column index to bot
rotate_side_col_to_bot(C, CI, RC) :-
    rotate(C, side_col_bot(-1), side_col_bot(0), copy_col(CI), RC).


% rotate_side_col_to_top(cube, column_index, rotated_cube)
% Rotate side column of the cube specified by column index to top
rotate_side_col_to_top(C, CI, RC) :-
    rotate(C, side_col_top(-1), side_col_top(0), copy_col(CI), RC).


% rotate_cube_side_cw(cube, side_index, rotated_cube) 
% Rotate the whole side of the cube clockwise
rotate_cube_side_cw(C, SIDE_I, RC) :-
    nth0(SIDE_I, C, SIDE),
    rotate_side_cw(SIDE, RSIDE), % Create rotated side
    setv(C, SIDE_I, RSIDE, RC). % Replace the old side with rotated one

% rotate_cube_side_ccw(cube, side_index, rotated_cube) 
% Rotate the whole side of the cube counterclockwise
rotate_cube_side_ccw(C, SIDE_I, RC) :-
    nth0(SIDE_I, C, SIDE),
    rotate_side_ccw(SIDE, RSIDE),
    setv(C, SIDE_I, RSIDE, RC).



/**                            Particular moves                             **/


% move_u_cw(cube, moves, updated_moves, rotated_cube)
% Perform U move
move_u_cw(C, M, [("U", RC)|M], RC) :-
    top(TOP_I), % Get index of the top side
    rotate_cube_side_cw(C, TOP_I, RC_), % Rotate the side
    rotate_hlevel_cw(RC_, 0, RC). % Rotate the adjancent row (in the correct direction)

% move_u_ccw(cube, moves, updated_moves, rotated_cube)
% Perform U' move
move_u_ccw(C, M, [("UC", RC)|M], RC) :-
    top(TOP_I),
    rotate_cube_side_ccw(C, TOP_I, RC_),
    rotate_hlevel_ccw(RC_, 0, RC).


% move_d_cw(cube, moves, updated_moves, rotated_cube)
% Perform D move
move_d_cw(C, M, [("D", RC)|M], RC) :-
    cube_size(SIZE),
    RI is SIZE - 1,
    bot(BOT_I),
    rotate_cube_side_cw(C, BOT_I, RC_),
    rotate_hlevel_cw(RC_, RI, RC).

% move_d_ccw(cube, moves, updated_moves, rotated_cube)
% Perform D' move
move_d_ccw(C, M, [("DC", RC)|M], RC) :-
    cube_size(SIZE),
    RI is SIZE - 1,
    bot(BOT_I),
    rotate_cube_side_ccw(C, BOT_I, RC_),
    rotate_hlevel_ccw(RC_, RI, RC).


% move_r_cw(cube, moves, updated_moves, rotated_cube)
% Perform R move
move_r_cw(C, M, [("R", RC)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    right(RIGHT_I),
    rotate_cube_side_cw(C, RIGHT_I, RC_),
    rotate_front_col_to_top(RC_, CI, RC).

% move_r_ccw(cube, moves, updated_moves, rotated_cube)
% Perform R' move
move_r_ccw(C, M, [("RC", RC)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    right(RIGHT_I),
    rotate_cube_side_ccw(C, RIGHT_I, RC_),
    rotate_front_col_to_bot(RC_, CI, RC).


% move_l_cw(cube, moves, updated_moves, rotated_cube)
% Perform L move
move_l_cw(C, M, [("L", RC)|M], RC) :-
    left(LEFT_I),
    rotate_cube_side_cw(C, LEFT_I, RC_),
    rotate_front_col_to_bot(RC_, 0, RC).

% move_l_ccw(cube, moves, updated_moves, rotated_cube)
% Perform L' move
move_l_ccw(C, M, [("LC", RC)|M], RC) :-
    left(LEFT_I),
    rotate_cube_side_ccw(C, LEFT_I, RC_),
    rotate_front_col_to_top(RC_, 0, RC).


% move_f_cw(cube, moves, updated_moves, rotated_cube)
% Perform F move
move_f_cw(C, M, [("F", RC)|M], RC) :-
    front(FRONT_I),
    rotate_cube_side_cw(C, FRONT_I, RC_),
    rotate_side_col_to_bot(RC_, 0, RC).

% move_f_ccw(cube, moves, updated_moves, rotated_cube)
% Perform F' move
move_f_ccw(C, M, [("FC", RC)|M], RC) :-
    front(FRONT_I),
    rotate_cube_side_ccw(C, FRONT_I, RC_),
    rotate_side_col_to_top(RC_, 0, RC).


% move_b_cw(cube, moves, updated_moves, rotated_cube)
% Perform B move
move_b_cw(C, M, [("B", RC)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    back(BACK_I),
    rotate_cube_side_cw(C, BACK_I, RC_),
    rotate_side_col_to_top(RC_, CI, RC).

% move_b_ccw(cube, moves, updated_moves, rotated_cube)
% Perform B' move
move_b_ccw(C, M, [("BC", RC)|M], RC) :-
    cube_size(SIZE),
    CI is SIZE - 1,
    back(BACK_I),
    rotate_cube_side_ccw(C, BACK_I, RC_),
    rotate_side_col_to_bot(RC_, CI, RC).


% move_m_cw(cube, moves, updated_moves, rotated_cube)
% Perform M move
move_m_cw(C, M, [("M", RC)|M], RC) :-
    cube_max_i(MAX_I),
    CI is MAX_I div 2,
    rotate_front_col_to_bot(C, CI, RC).

% move_m_ccw(cube, moves, updated_moves, rotated_cube)
% Perform M' move
move_m_ccw(C, M, [("MC", RC)|M], RC) :-
    cube_max_i(MAX_I),
    CI is MAX_I div 2,
    rotate_front_col_to_top(C, CI, RC).


% move_e_cw(cube, moves, updated_moves, rotated_cube)
% Perform E move
move_e_cw(C, M, [("E", RC)|M], RC) :-
    cube_max_i(MAX_I),
    RI is MAX_I div 2,
    rotate_hlevel_ccw(C, RI, RC).

% move_e_ccw(cube, moves, updated_moves, rotated_cube)
% Perform E' move
move_e_ccw(C, M, [("EC", RC)|M], RC) :-
    cube_max_i(MAX_I),
    RI is MAX_I div 2,
    rotate_hlevel_cw(C, RI, RC).


% move_s_ccw(cube, moves, updated_moves, rotated_cube)
% Perform S move
move_s_cw(C, M, [("S", RC)|M], RC) :-
    cube_max_i(MAX_I),
    CI is MAX_I div 2,
    rotate_side_col_to_bot(C, CI, RC).

% move_s_ccw(cube, moves, updated_moves, rotated_cube)
% Perform S' move
move_s_ccw(C, M, [("SC", RC)|M], RC) :-
    cube_max_i(MAX_I),
    CI is MAX_I div 2,
    rotate_side_col_to_top(C, CI, RC).

