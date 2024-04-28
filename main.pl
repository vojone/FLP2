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


% cube_done(cube)
%
% Checks if cube is solved
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
cube_done(C) :- done_cube(DONE_C), C = DONE_C.


% done_cube(cube)
%
% Clause that returns solved cube
done_cube(C) :- C = [
        [1,1,1,1,1,1,1,1,1],
        [2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,3],
        [4,4,4,4,4,4,4,4,4],
        [5,5,5,5,5,5,5,5,5],
        [6,6,6,6,6,6,6,6,6]
    ].


/**               Clauses for writing a solution to stdout                  **/


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
% Tries to solve the cube and returns (reversed) sequence of moves
solve(INIT_CUBE, MOVES, ARGV) :-
    % All cubes are solvable within 20 moves
    (
        memberchk('-c', ARGV) -> % Choose solution with multithreading or without
            solve_ids_concurrent(INIT_CUBE, cube_done, 20, MOVES);
            solve_ids(INIT_CUBE, cube_done, 20, MOVES)
    ).


% Main body
main(ARGV) :-
    read_cube(CUBE, _), % Read the cube from stdin and ignore remainding lines
    solve(CUBE, MOVES, ARGV), % Solve cube
    write_solution(CUBE, MOVES, ARGV), % Print solution (with or without moves)
    (
        memberchk('-v', ARGV) -> 
            write_moves(MOVES);
            true
    ).
