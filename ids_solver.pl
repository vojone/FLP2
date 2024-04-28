/**
* flp-23-log
* ids_solver.pl
*
* Clauses for solving rubikscube by IDS method. Also contains multithreaded
* version of IDS, that uses up to 18 threads to speed up the searching.
*
* Author: Vojtech Dvorak (xdvora3o)
*/

:- consult("cube").


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


/**                              Multithreaded                              **/


% solve_ids_c_(initial_cube, done_clause, max_depth, first_move, result)
%
% Auxiliary clause for multithreaded IDS - it performs the first move and then
solve_ids_c_(C, DONE_C, D, MOVE, RM) :-
    call(MOVE, C, [], M_, RC),
    ND is D - 1,
    solve_ids_step(RC, _, M_, RM, DONE_C, 0, ND).


% solve_ids_concurrent_(initial_cube, move_seq, done_clause, current_depth, max_depth)
%
% One step of IDS executed with multiple threads
solve_ids_concurrent_(C, RM, DONE_CLAUSE, D, _) :- D =< 1, solve_ids(C, RM, DONE_CLAUSE, 0, 1), !. % Try the first step with only one thread
solve_ids_concurrent_(C, RM, DONE_CLAUSE, D, _) :- D > 1,
    first_solution(
        RM,
        [
            solve_ids_c_(C, DONE_CLAUSE, D, move_u_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_u_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_d_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_d_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_f_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_f_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_r_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_r_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_l_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_l_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_b_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_b_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_e_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_e_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_m_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_m_ccw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_s_cw, RM),
            solve_ids_c_(C, DONE_CLAUSE, D, move_s_ccw, RM)
        ],
    [on_fail(continue)]). % Wait for all threads (they may find a solution)


% solve_ids_concurrent(initial_cube, move_seq, done_clause, current_depth, max_depth)
%
% IDS algorithm that uses multithreading
solve_ids_concurrent(C, RM, DONE_C, D, MD) :- solve_ids_concurrent_(C, RM, DONE_C, D, MD), !.
% Increase current depth and repeat
solve_ids_concurrent(C, RM, DONE_C, D, MD) :- D < MD, ND is D + 1, solve_ids_concurrent(C, RM, DONE_C, ND, MD).


% solve_ids_concurrent(initial_cube, done_clause, max_depth, move_seq)
%
% Same as solve_ids_concurrent/4, but more convenient
solve_ids_concurrent(C, DONE_C, MD, RM) :- solve_ids_concurrent(C, RM, DONE_C, 0, MD).
