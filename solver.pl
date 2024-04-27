/**
* flp-23-log
* solver.pl
*
* The main body and arguments parsing of rubik's cube solver. 
*
* Author: Vojtech Dvorak (xdvora3o)
*/

:- consult("cube").
:- consult("ids_solver").


/**               Clauses for checking the state of a cube                  **/


% side_color(color, cube)
%
% Checks if one side of the cube has specified color (all squares must have
% the color)
side_color(_, []) :- !.
side_color(COLOR, [COLOR|T]) :- side_color(COLOR, T).


% cube_done(cube)
%
% Check if cube is solved
% Note: only this solution is accepted:
% 555
% 555
% 555
% 111 222 333 444
% 111 222 333 444
% 111 222 333 444
% 666
% 666
% 666
cube_done([]) :- !. 
cube_done([FRONT, RIGHT, BACK, LEFT, TOP, BOT]) :-
    side_color(1, FRONT),
    side_color(2, RIGHT),
    side_color(3, BACK),
    side_color(4, LEFT),
    side_color(5, TOP),
    side_color(6, BOT).



% Auxiliary clauses for testing

cube_s(C) :- C = [
        [2,2,2,1,1,1,4,4,4],
        [3,3,3,2,2,2,1,1,1],
        [4,4,4,3,3,3,2,2,2],
        [1,1,1,4,4,4,3,3,3],
        [5,5,5,5,5,5,5,5,5],
        [6,6,6,6,6,6,6,6,6]
    ].

done_cube(C) :- C = [
        [1,1,1,1,1,1,1,1,1],
        [2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,3],
        [4,4,4,4,4,4,4,4,4],
        [5,5,5,5,5,5,5,5,5],
        [6,6,6,6,6,6,6,6,6]
    ].

scramble(C, SC) :-
    move_seq([move_r_cw, move_s_cw, move_e_ccw, move_d_ccw, move_b_cw, move_b_cw], C, _, _, SC).

% move_seq([move_r_cw, move_s_cw, move_e_ccw, move_d_ccw, move_d_ccw, move_b_cw, move_b_cw], C, _, _, SC). 15 min


/**               Clauses for writign a solution to stdout                  **/


% write_moves_(moves)
%
% Writes list with moves to stdout
write_moves_([]).
write_moves_([HMOVE|T]) :- write(HMOVE), write(" "), write_moves_(T).

% write_moves(moves)
%
% Writes list with moves to stdout in reversed order (individual moves are
% appended when moves are performed, therefore this is correct order)
write_moves(MOVES) :- reverse(MOVES, RMOVES), write_moves_(RMOVES), write("\n").


% smove(cube, move_sign, result_cube)
%
% Performs move base on sign
smove(C, S, RC) :- sign2move(S, MOVE_CLAUSE), call(MOVE_CLAUSE, C, _, _, RC).


% write_solutionm_(initial_cube, moves)
%
% Writes solution (with moves!) to stdout
write_solutionm_(_, []).
write_solutionm_(CUBE, [HMOVE|TMOVES]) :- 
    write(HMOVE), write("\n"),
    smove(CUBE, HMOVE, RCUBE),
    write_cube(CUBE), write("\n"),
    write_solutionm_(RCUBE, TMOVES).


% write_solution_(initial_cube, moves)
%
% Writes solution (WITHOUT moves!) to stdout
write_solution_(_, []).
write_solution_(CUBE, [HMOVE|TMOVES]) :-
    smove(CUBE, HMOVE, RCUBE),
    write_cube(RCUBE),
    write("\n"),
    write_solution_(RCUBE, TMOVES).


% write_solution(initial_cube, moves, argv)
%
% Writes solution to stdout, by argv can be defined if solution will be in
% standard format (defined by the assignment) or in verbose format (with moves
% more readable, better for solving rubikscube in reality)
write_solution(INITIAL_CUBE, MOVES, ARGV) :- 
    reverse(MOVES, RMOVES),
    write_cube(INITIAL_CUBE), % Write initial cube first
    write("\n"),
    (
        memberchk('-v', ARGV) -> % Write solution (with or without moves)
            write_solutionm_(INITIAL_CUBE, RMOVES);
            write_solution_(INITIAL_CUBE, RMOVES)
    ).


% solve(intial_cube, moves)
%
% Treíes to solve the cube and return (reversed) sequence of moves
solve(INIT_CUBE, MOVES) :-
    solve_ids(INIT_CUBE, cube_done, 20, MOVES). % All cubes are solvable until 20 moves


% Main body
main(ARGV) :-
    read_cube(CUBE, NL), % Read the cube from stdin
    solve(CUBE, MOVES), % Solve cube
    write_solution(CUBE, MOVES, ARGV), % Print solution (with or without moves)
    (
        memberchk('-v', ARGV) -> 
            write_moves(MOVES);
            true
    ).
