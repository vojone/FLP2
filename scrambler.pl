/**
 * Clauses for simple scrambling the cube
 * 
 * (not submitted)
 */

:- consult('main').

% move_sq(move_fs, cube, moves, updated_moves, rotated_cube)
%
% Perform sequence of moves
move_seq([], C, M, M, C).
move_seq([HMOVE|TMOVES], C, M, RM, RC) :-
    call(HMOVE, C, M, RM_, RC_),
    move_seq(TMOVES, RC_, RM_, RM, RC).


scramble(MOVES) :- done_cube(C), move_seq(MOVES, C, _, _, RC), write_cube(RC).
