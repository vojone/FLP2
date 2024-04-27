/**
* flp-23-log
* cube.pl
*
* Implementation of moves with the rubikscube. All implemented
* manipulations presumes that the following representation is used:
* cube(C) :- C = [
*     [F1,F2,F3,F4,F5,F3,F7,F8,F9],
*     [R1,R2,R3,R4,R5,R3,R7,R8,R9],
*     [B1,B2,B3,B4,B5,B3,B7,B8,B9],
*     [L1,L2,L3,L4,L5,L3,L7,L8,L9],
*     [T1,T2,T3,T4,T5,T3,T7,T8,T9],
*     [D1,D2,D3,D4,D5,D3,D7,D8,D9]
* ].
*
* T1 T2 T3
* T4 T5 T6
* T7 T8 T9
* F1 F2 F3   R1 R2 R3   B1 B2 B3   L1 L2 L3
* F4 F5 F6   R4 R5 R6   B4 B5 B6   L4 L5 L6  
* F7 F8 F9   R7 R8 R9   B7 B8 B9   L7 L8 L9  
* D1 D2 D3
* D4 D5 D6
* D7 D8 D9
*
* (2D list, where the outer dimension represents the whole rubik's cube and the
* inner one represents individual sides of the cube)
*
*
* Author: Vojtech Dvorak (xdvora3o)
*/



/***         Some constants for printing and reading the cube              **/
% Cube side size
cube_size(3).

% Indexes of cube sides
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
% Convert lines to the rubik's cube
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
    lines2side(L, L_, TOP_SIDE), % Read top side first
    pop_empty_lines(L_, NL), % Remove empty lines from list of lines
    lines2cube(NL, 0, NNL, SIDES), % Read front, right, back and left side
    pop_empty_lines(NNL, NNNL),
    lines2side(NNNL, NNNNL, BOT_SIDE), % Read bot side
    pop_empty_lines(NNNNL, REM),
    append(SIDES, [TOP_SIDE], C_),
    append(C_, [BOT_SIDE], C).


% read_cube(result_cube, line_remainder)
% Reads rubikscube from stdin in the following format:
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





/**                         Clauses for performing moves                    **/

% sign2move(move_sign, move_clause)
% Translates signs of moves to clauses that actually perform the move
sign2move("U", move_u_cw) :- !.
sign2move("UC", move_u_ccw) :- !.
sign2move("D", move_d_cw) :- !.
sign2move("DC", move_d_ccw) :- !.
sign2move("R", move_r_cw) :- !.
sign2move("RC", move_r_ccw) :- !.
sign2move("L", move_l_cw) :- !.
sign2move("LC", move_l_ccw) :- !.
sign2move("F", move_f_cw) :- !.
sign2move("FC", move_f_ccw) :- !.
sign2move("B", move_b_cw) :- !.
sign2move("BC", move_b_ccw) :- !.
sign2move("M", move_m_cw) :- !.
sign2move("MC", move_m_ccw) :- !.
sign2move("E", move_e_cw) :- !.
sign2move("EC", move_e_ccw) :- !.
sign2move("S", move_s_cw) :- !.
sign2move("SC", move_s_ccw) :- !.



% All following clauses have this pattern:
% <move_type>(cube, moves, new_moves, new_cube)
%
% where:
% cube      rubik's cube list before the move
% moves     list with moves before the move
% new_moves list with moves after the move
% new_cube  rubik's cube list after the move


% Identity move (just template for creating the new moves)
move_id(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, [""|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].

% move_u_cw(cube, moves, updated_moves, rotated_cube)
% Perform U move
move_u_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["U"|M], RC) :-
        RC = [
            [
                R0, R1, R2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                B0, B1, B2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                L0, L1, L2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                F0, F1, F2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T6, T3, T0,
                T7, T4, T1,
                T8, T5, T2
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].


% move_u_ccw(cube, moves, updated_moves, rotated_cube)
% Perform U' move
move_u_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["UC"|M], RC) :-
        RC = [
            [
                L0, L1, L2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                F0, F1, F2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                R0, R1, R2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                B0, B1, B2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T2, T5, T8,
                T1, T4, T7,
                T0, T3, T6
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].


% move_d_cw(cube, moves, updated_moves, rotated_cube)
% Perform D move


move_d_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["D"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                L6, L7, L8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                F6, F7, F8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                R6, R7, R8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                B6, B7, B8
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D6, D3, D0,
                D7, D4, D1,
                D8, D5, D2
            ]
        ].



% move_d_ccw(cube, moves, updated_moves, rotated_cube)
% Perform D' move
move_d_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["DC"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                R6, R7, R8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                B6, B7, B8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                L6, L7, L8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                F6, F7, F8
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D2, D5, D8,
                D1, D4, D7,
                D0, D3, D6
            ]
        ].


% move_r_cw(cube, moves, updated_moves, rotated_cube)
% Perform R move
move_r_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["R"|M], RC) :-
        RC = [
            [
                F0, F1, D2,
                F3, F4, D5,
                F6, F7, D8
            ],
            [
                R6, R3, R0,
                R7, R4, R1,
                R8, R5, R2
            ],
            [
                T8, B1, B2,
                T5, B4, B5,
                T2, B7, B8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T0, T1, F2,
                T3, T4, F5,
                T6, T7, F8
            ],
            [
                D0, D1, B6,
                D3, D4, B3,
                D6, D7, B0
            ]
        ].


% move_r_ccw(cube, moves, updated_moves, rotated_cube)
% Perform R' move
move_r_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["RC"|M], RC) :-
        RC = [
            [
                F0, F1, T2,
                F3, F4, T5,
                F6, F7, T8
            ],
            [
                R2, R5, R8,
                R1, R4, R7,
                R0, R3, R6
            ],
            [
                D8, B1, B2,
                D5, B4, B5,
                D2, B7, B8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T0, T1, B6,
                T3, T4, B3,
                T6, T7, B0
            ],
            [
                D0, D1, F2,
                D3, D4, F5,
                D6, D7, F8
            ]
        ].


% move_l_cw(cube, moves, updated_moves, rotated_cube)
% Perform L move
move_l_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["L"|M], RC) :-
        RC = [
            [
                T0, F1, F2,
                T3, F4, F5,
                T6, F7, F8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                D0, B1, B2,
                D3, B4, B5,
                D6, B7, B8
            ],
            [
                L6, L3, L0,
                L7, L4, L1,
                L8, L5, L2
            ],
            [
                B0, T1, T2,
                B3, T4, T5,
                B6, T7, T8
            ],
            [
                F0, D1, D2,
                F3, D4, D5,
                F6, D7, D8
            ]
        ].

% move_l_ccw(cube, moves, updated_moves, rotated_cube)
% Perform L' move
move_l_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, [("", RC)|M], RC) :-
        RC = [
            [
                D0, F1, F2,
                D3, F4, F5,
                D6, F7, F8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                T0, B1, B2,
                T3, B4, B5,
                T6, B7, B8
            ],
            [
                L2, L5, L8,
                L1, L4, L7,
                L0, L3, L6
            ],
            [
                F0, T1, T2,
                F3, T4, T5,
                F6, T7, T8
            ],
            [
                B0, D1, D2,
                B3, D4, D5,
                B6, D7, D8
            ]
        ].


% move_f_cw(cube, moves, updated_moves, rotated_cube)
% Perform F move
move_f_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["F"|M], RC) :-
        RC = [
            [
                F6, F3, F0,
                F7, F4, F1,
                F8, F5, F2
            ],
            [
                T6, R1, R2,
                T7, R4, R5,
                T8, R7, R8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                L0, L1, D0,
                L3, L4, D1,
                L6, L7, D2
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                L8, L5, L2
            ],
            [
                R6, R3, R0,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].


% move_f_ccw(cube, moves, updated_moves, rotated_cube)
% Perform F' move
move_f_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["FC"|M], RC) :-
        RC = [
            [
                F2, F5, F8,
                F1, F4, F7,
                F0, F3, F6
            ],
            [
                D2, R1, R2,
                D1, R4, R5,
                D0, R7, R8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                L0, L1, T8,
                L3, L4, T7,
                L6, L7, T6
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                R0, R3, R6
            ],
            [
                L2, L5, L8,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].


% move_b_cw(cube, moves, updated_moves, rotated_cube)
% Perform B move
move_b_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["B"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                R0, R1, D8,
                R3, R4, D7,
                R6, R7, D6
            ],
            [
                B6, B3, B0,
                B7, B4, B1,
                B8, B5, B2
            ],
            [
                T2, L1, L2,
                T1, L4, L5,
                T0, L7, L8
            ],
            [
                R2, R5, R8,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                L0, L3, L6
            ]
        ].

% move_b_ccw(cube, moves, updated_moves, rotated_cube)
% Perform B' move
move_b_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["BC"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                R0, R1, T0,
                R3, R4, T1,
                R6, R7, T2
            ],
            [
                B2, B5, B8,
                B1, B4, B7,
                B0, B3, B6
            ],
            [
                D6, L1, L2,
                D7, L4, L5,
                D8, L7, L8
            ],
            [
                L6, L3, L0,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                R8, R5, R2
            ]
        ].


% move_m_cw(cube, moves, updated_moves, rotated_cube)
% Perform M move
move_m_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["M"|M], RC) :-
        RC = [
            [
                F0, T1, F2,
                F3, T4, F5,
                F6, T7, F8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                B0, D7, B2,
                B3, D4, B5,
                B6, D1, B8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T0, B7, T2,
                T3, B4, T5,
                T6, B1, T8
            ],
            [
                D0, F1, D2,
                D3, F4, D5,
                D6, F7, D8
            ]
        ].

% move_m_ccw(cube, moves, updated_moves, rotated_cube)
% Perform M' move
move_m_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["MC"|M], RC) :-
        RC = [
            [
                F0, D1, F2,
                F3, D4, F5,
                F6, D7, F8
            ],
            [
                R0, R1, R2,
                R3, R4, R5,
                R6, R7, R8
            ],
            [
                B0, T7, B2,
                B3, T4, B5,
                B6, T1, B8
            ],
            [
                L0, L1, L2,
                L3, L4, L5,
                L6, L7, L8
            ],
            [
                T0, F1, T2,
                T3, F4, T5,
                T6, F7, T8
            ],
            [
                D0, B7, D2,
                D3, B4, D5,
                D6, B1, D8
            ]
        ].


% move_e_cw(cube, moves, updated_moves, rotated_cube)
% Perform E move
move_e_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["E"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                L3, L4, L5,
                F6, F7, F8
            ],
            [
                R0, R1, R2,
                F3, F4, F5,
                R6, R7, R8
            ],
            [
                B0, B1, B2,
                R3, R4, R5,
                B6, B7, B8
            ],
            [
                L0, L1, L2,
                B3, B4, B5,
                L6, L7, L8
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].

% move_e_ccw(cube, moves, updated_moves, rotated_cube)
% Perform E' move
move_e_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["EC"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                R3, R4, R5,
                F6, F7, F8
            ],
            [
                R0, R1, R2,
                B3, B4, B5,
                R6, R7, R8
            ],
            [
                B0, B1, B2,
                L3, L4, L5,
                B6, B7, B8
            ],
            [
                L0, L1, L2,
                F3, F4, F5,
                L6, L7, L8
            ],
            [
                T0, T1, T2,
                T3, T4, T5,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                D3, D4, D5,
                D6, D7, D8
            ]
        ].


% move_s_ccw(cube, moves, updated_moves, rotated_cube)
% Perform S move
move_s_cw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["S"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                R0, T3, R2,
                R3, T4, R5,
                R6, T5, R8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                L0, D3, L2,
                L3, D4, L5,
                L6, D5, L8
            ],
            [
                T0, T1, T2,
                L7, L4, L1,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                R7, R4, R1,
                D6, D7, D8
            ]
        ].


% move_s_ccw(cube, moves, updated_moves, rotated_cube)
% Perform S' move
move_s_ccw(
    [
        [F0, F1, F2, F3, F4, F5, F6, F7, F8], % Front side
        [R0, R1, R2, R3, R4, R5, R6, R7, R8], % Right side
        [B0, B1, B2, B3, B4, B5, B6, B7, B8], % Back side
        [L0, L1, L2, L3, L4, L5, L6, L7, L8], % Left side
        [T0, T1, T2, T3, T4, T5, T6, T7, T8], % Top side
        [D0, D1, D2, D3, D4, D5, D6, D7, D8] % Bottom (down) side
    ],
    M, ["SC"|M], RC) :-
        RC = [
            [
                F0, F1, F2,
                F3, F4, F5,
                F6, F7, F8
            ],
            [
                R0, D5, R2,
                R3, D4, R5,
                R6, D3, R8
            ],
            [
                B0, B1, B2,
                B3, B4, B5,
                B6, B7, B8
            ],
            [
                L0, T5, L2,
                L3, T4, L5,
                L6, T3, L8
            ],
            [
                T0, T1, T2,
                R1, R4, R7,
                T6, T7, T8
            ],
            [
                D0, D1, D2,
                L1, L4, L7,
                D6, D7, D8
            ]
        ].


% move_sq(move_fs, cube, moves, updated_moves, rotated_cube)
% Perform sequence of moves
move_seq([], C, M, M, C).
move_seq([HMOVE|TMOVES], C, M, RM, RC) :-
    call(HMOVE, C, M, RM_, RC_),
    move_seq(TMOVES, RC_, RM_, RM, RC).



% Mapping of move sequences for right, back and left sides: 
front_right([], C, M, M, C).
front_right([move_u_cw|TMOVES], C, M, RM, RC) :- move_u_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_u_ccw|TMOVES], C, M, RM, RC) :- move_u_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_d_cw|TMOVES], C, M, RM, RC) :- move_d_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_d_ccw|TMOVES], C, M, RM, RC) :- move_d_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_r_cw|TMOVES], C, M, RM, RC) :- move_b_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_r_ccw|TMOVES], C, M, RM, RC) :- move_b_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_l_cw|TMOVES], C, M, RM, RC) :- move_f_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_l_ccw|TMOVES], C, M, RM, RC) :- move_f_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_f_cw|TMOVES], C, M, RM, RC) :- move_r_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_f_ccw|TMOVES], C, M, RM, RC) :- move_r_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_b_cw|TMOVES], C, M, RM, RC) :- move_l_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_b_ccw|TMOVES], C, M, RM, RC) :- move_l_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_m_cw|TMOVES], C, M, RM, RC) :- move_s_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_m_ccw|TMOVES], C, M, RM, RC) :- move_s_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_e_cw|TMOVES], C, M, RM, RC) :- move_e_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_e_ccw|TMOVES], C, M, RM, RC) :- move_e_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_s_cw|TMOVES], C, M, RM, RC) :- move_m_cw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).
front_right([move_s_csw|TMOVES], C, M, RM, RC) :- move_m_ccw(C, M, RM_, RC_), front_right(TMOVES, RC_, RM_, RM, RC).


front_back([], C, M, M, C).
front_back([move_u_cw|TMOVES], C, M, RM, RC) :- move_u_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_u_ccw|TMOVES], C, M, RM, RC) :- move_u_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_d_cw|TMOVES], C, M, RM, RC) :- move_d_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_d_ccw|TMOVES], C, M, RM, RC) :- move_d_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_r_cw|TMOVES], C, M, RM, RC) :- move_l_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_r_ccw|TMOVES], C, M, RM, RC) :- move_l_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_l_cw|TMOVES], C, M, RM, RC) :- move_r_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_l_ccw|TMOVES], C, M, RM, RC) :- move_r_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_f_cw|TMOVES], C, M, RM, RC) :- move_b_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_f_ccw|TMOVES], C, M, RM, RC) :- move_b_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_b_cw|TMOVES], C, M, RM, RC) :- move_f_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_b_ccw|TMOVES], C, M, RM, RC) :- move_f_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_m_cw|TMOVES], C, M, RM, RC) :- move_m_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_m_ccw|TMOVES], C, M, RM, RC) :- move_m_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_e_cw|TMOVES], C, M, RM, RC) :- move_e_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_e_ccw|TMOVES], C, M, RM, RC) :- move_e_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_s_cw|TMOVES], C, M, RM, RC) :- move_s_ccw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).
front_back([move_s_csw|TMOVES], C, M, RM, RC) :- move_s_cw(C, M, RM_, RC_), front_back(TMOVES, RC_, RM_, RM, RC).


front_left([], C, M, M, C).
front_left([move_u_cw|TMOVES], C, M, RM, RC) :- move_u_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_u_ccw|TMOVES], C, M, RM, RC) :- move_u_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_d_cw|TMOVES], C, M, RM, RC) :- move_d_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_d_ccw|TMOVES], C, M, RM, RC) :- move_d_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_r_cw|TMOVES], C, M, RM, RC) :- move_f_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_r_ccw|TMOVES], C, M, RM, RC) :- move_f_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_l_cw|TMOVES], C, M, RM, RC) :- move_b_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_l_ccw|TMOVES], C, M, RM, RC) :- move_b_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_f_cw|TMOVES], C, M, RM, RC) :- move_l_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_f_ccw|TMOVES], C, M, RM, RC) :- move_l_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_b_cw|TMOVES], C, M, RM, RC) :- move_r_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_b_ccw|TMOVES], C, M, RM, RC) :- move_r_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_m_cw|TMOVES], C, M, RM, RC) :- move_s_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_m_ccw|TMOVES], C, M, RM, RC) :- move_s_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_e_cw|TMOVES], C, M, RM, RC) :- move_e_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_e_ccw|TMOVES], C, M, RM, RC) :- move_e_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_s_cw|TMOVES], C, M, RM, RC) :- move_m_ccw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
front_left([move_s_csw|TMOVES], C, M, RM, RC) :- move_m_cw(C, M, RM_, RC_), front_left(TMOVES, RC_, RM_, RM, RC).
