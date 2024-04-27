/**
* flp-23-log
* ids_solver.pl
*
* Clauses for solving rubikscube by IDS method.
*
* Author: Vojtech Dvorak (xdvora3o)
*/

:- consult("cube").

:- dynamic(ids_state_/1).


check_ids_state(C) :- ids_state_(C), !, false.
check_ids_state(_).

save_ids_state(C) :- asserta(ids_state_(C)).
remove_ids_state(C) :- retract(ids_state_(C)).
clear_ids_states :- retractall(ids_state_(_)).

% ids_move(cube, move, updated_move, updated_cube)
%
% Generates cubes and corresponding move sequences after moves specified by
% assignment
ids_move(C, M, RM, RC) :-
    move_u_cw(C, M, RM, RC);
    move_u_ccw(C, M, RM, RC);
    move_d_cw(C, M, RM, RC);
    move_d_ccw(C, M, RM, RC);
    move_r_cw(C, M, RM, RC);
    move_r_ccw(C, M, RM, RC);
    move_l_cw(C, M, RM, RC);
    move_l_ccw(C, M, RM, RC);
    move_f_cw(C, M, RM, RC);
    move_f_ccw(C, M, RM, RC);
    move_b_cw(C, M, RM, RC);
    move_b_ccw(C, M, RM, RC);
    move_m_cw(C, M, RM, RC);
    move_m_ccw(C, M, RM, RC);
    move_e_cw(C, M, RM, RC);
    move_e_ccw(C, M, RM, RC);
    move_s_cw(C, M, RM, RC);
    move_s_ccw(C, M, RM, RC).


/** Complete solution by IDS **/

% solve_ids_step(cube, updated_cube, moves, updated_moves, done_clause, cur_depth, max_depth)
%
% Performs one iteration of IDS algorithm
solve_ids_step(C, C, M, M, DONE_C, _, _) :- call(DONE_C, C), !. % If cube satisfies "done condition", stop and return the cube as the result 
solve_ids_step(_, _, _, _, _, D, MD) :-  D >= MD, !, false. % If current max depth is lower than current depth, stop with false
solve_ids_step(C, RC, M, RM, DC, D, MD) :- ids_move(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).


% solve_ids(cube, result_moves, done_clause, cur_depth, max_depth)
%
% Tries to solve cube with IDS algorithm until max_depth depth
solve_ids(C, RM, DONE_C, D, _) :- solve_ids_step(C, _, [], RM, DONE_C, 0, D), !.
solve_ids(C, RM, DONE_C, D, MD) :- D < MD, ND is D + 1, solve_ids(C, RM, DONE_C, ND, MD).


% solve_ids(cube, done_clause, max_depth, result_move)
%
% Tries to solve cube with IDS algorithm until max_depth depth and returns
% reversed sequence of moves, that leads to the solution specified by the 
% done_clause
solve_ids(C, DONE_CLAUSE, MD, RM) :- solve_ids(C, RM, DONE_CLAUSE, 0, MD).
