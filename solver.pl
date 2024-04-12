:- consult("cube").

% The mock cube for the testing
cube(C) :- C = [
        [0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 ],
        [9 , 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

cube_s(C) :- C = [[3,3,3,3,5,5,5,5,5],[4,4,4,6,6,6,6,6,6],[5,5,5,3,3,5,3,3,3],[4,4,4,4,4,4,6,6,6],[1,1,1,2,2,2,2,2,2],[1,1,1,1,1,1,2,2,2]].

done_cube(C) :- C = [
        [1,1,1,1,1,1,1,1,1],
        [2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,3],
        [4,4,4,4,4,4,4,4,4],
        [5,5,5,5,5,5,5,5,5],
        [6,6,6,6,6,6,6,6,6]
    ].


write_solutionm_([]).
write_solutionm_([(HMOVE, HCUBE)|TMOVES]) :- 
    write(HMOVE), write("\n"),
    write_cube(HCUBE), write("\n"),
    write_solutionm_(TMOVES).

write_solution_([]).
write_solution_([(_, HCUBE)|TMOVES]) :- write_cube(HCUBE), write("\n"), write_solution_(TMOVES).
write_solution(MOVES, ARGV) :- reverse(MOVES, RMOVES),
    (
        memberchk('-v', ARGV) -> 
            write_solutionm_(RMOVES);
            write_solution_(RMOVES)
    ).


write_moves_([]).
write_moves_([(HMOVE, _)|T]) :- write(HMOVE), write(" "), write_moves(T), write("\n").
write_moves(MOVES) :- reverse(MOVES, RMOVES), write_moves_(RMOVES).


solve_ids_step(_, _, _, _, D) :-  D =< 0, !, false.
solve_ids_step(C, M, M, C, _) :- cube_done(C), !.
solve_ids_step(C, M, RM, RC, D) :- move_u_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_u_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_d_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_d_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_r_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_r_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_l_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_l_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_f_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_f_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_b_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_b_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_m_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_m_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_e_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_e_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_s_cw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_s_ccw(C, M, M_, RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).


solve_ids(C, RM, RC, D, _) :- solve_ids_step(C, [], RM, RC, D), !.
solve_ids(C, RM, RC, D, MD) :- D < MD, ND is D + 1, solve_ids(C, RM, RC, ND, MD).

solve(CUBE, MOVES) :-
    solve_ids(CUBE, MOVES, _, 0, 5).


scramble(R______) :-
    done_cube(C),
    move_e_ccw(C, _, _, R___),
    move_b_ccw(R___, _, _, R____),
    move_m_cw(R____, _, _, R______).


main(ARGV) :-
    read_cube(CUBE, NL),
    solve(CUBE, MOVES),
    write_solution(MOVES, ARGV),
    (
        memberchk('-v', ARGV) -> 
            write_moves(MOVES);
            true
    ).
