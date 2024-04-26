:- consult("cube").
:- consult("ids_solver").

% The mock cube for the testing
cube(C) :- C = [
        [0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 ],
        [9 , 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

cube_s(C) :- C = [
    [3,3,3,3,5,5,5,5,5],
    [4,4,4,6,6,6,6,6,6],
    [5,5,5,3,3,5,3,3,3],
    [4,4,4,4,4,4,6,6,6],
    [1,1,1,2,2,2,2,2,2],
    [1,1,1,1,1,1,2,2,2]
].

done_cube(C) :- C = [
        [1,1,3,1,1,3,1,1,3],
        [4,4,4,2,2,2,2,2,2],
        [1,1,1,3,3,3,3,3,3],
        [2,2,2,4,4,4,4,4,4],
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


% scramble(FC) :-
%     done_cube(C),
%     move_u_ccw(C, _, _, RC),
%     move_e_cw(R___, _, _, R____),
%     move_m_ccw(R____, _, _, R_____).

% solve(INIT_CUBE, WC_MOVES2) :-
%     solve_correctly_sided(INIT_CUBE, [], CS_MOVES, CS_CUBE, 0),
%     solve_daisy(CS_CUBE, CS_MOVES, WC_MOVES1, WC_CUBE1, 100),
%     solve_white_cross(WC_CUBE1, WC_MOVES1, WC_MOVES2, WC_CUBE2, 100).

solve(INIT_CUBE, MOVES) :-
    solve_ids(INIT_CUBE, MOVES, _, 0, 20).


main(ARGV) :-
    read_cube(CUBE, NL),
    solve(CUBE, MOVES),
    write_solution(MOVES, ARGV),
    (
        memberchk('-v', ARGV) -> 
            write_moves(MOVES);
            true
    ).
