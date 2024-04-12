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


add_state(C, 1) :- asserta(cube_state(C, 0)).
add_state(C, I, NI) :- asserta(cube_state(C, I)), NI is I + 1.

check_state(C) :- cube_state(C, _), !, false.
check_state(_) :- true.

clear_states :- retractall(cube_state(_, _)).

init_state(C) :- cube_state(C, 0).



solve_ids_step(C, M, I, M, C, I, _) :- cube_done(C), !.
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_u_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_u_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_d_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_d_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_r_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_r_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_l_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_l_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_f_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_f_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_b_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_b_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_m_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_m_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_e_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_e_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_s_cw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).
solve_ids_step(C, M, I, RM, RC, RI, D) :- D > 0, move_s_ccw(C, M, I, M_, RC_), check_state(RC_), ND is D - 1, add_state(RC_, I, I_), solve_ids_step(RC_, M_, I_, RM, RC, RI, ND).


white_cross(CUBE) :- CUBE = [
        [_,B,_,
         _,B,_,
         _,_,_],
        [_,C,_,
         _,C,_,
         _,_,_],
        [_,D,_,
         _,D,_,
         _,_,_],
        [_,E,_,
         _,E,_,
         _,_,_],
        [_,A,_,
         A,A,A,
         _,A,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].


white_cross_sit1(C) :- C = [
        [_,_,_,
         _,A,A,
         _,_,_],
        [_,_,_,
         B,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,B,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].

white_cross_sit2(C) :- C = [
        [_,_,_,
         _,A,_,
         _,A,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,B,_,
         _,_,_],
        [_,B,_,
         _,_,_,
         _,_,_]
    ].

white_cross_sit3(C) :- C = [
        [_,_,_,
         _,A,B,
         _,_,_],
        [_,_,_,
         A,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,B,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].

white_cross_sit4(C) :- C = [
        [_,_,_,
         _,A,_,
         _,B,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,B,_,
         _,_,_],
        [_,A,_,
         _,_,_,
         _,_,_]
    ].

white_cross_sit5(C) :- C = [
        [_,A,_,
         _,B,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,A,_,
         _,B,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].

white_cross_sit6(C) :- C = [
        [_,_,_,
         A,A,_,
         _,_,_],
        [_,_,_,
         _,_,B,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,B,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].



white_cross_sit7(C) :- C = [
        [_,_,_,
         B,A,_,
         _,_,_],
        [_,_,_,
         _,_,A,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_],
        [_,_,_,
         _,B,_,
         _,_,_],
        [_,_,_,
         _,_,_,
         _,_,_]
    ].


front_right(C, [R, B, L, F, T, BO]) :- C = [F, R, B, L, T, BO].
front_back(C, [B, L, F, R, T, BO]) :- C = [F, R, B, L, T, BO].
front_left(C, [L, F, R, B, T, BO]) :- C = [F, R, B, L, T, BO].

move_seq(C, M, I, C, M, I, []).
move_seq(C, M, I, RM, RC, RI, [HMOVE|TMOVES]) :-
    RI_ is I + 1,
    call(HMOVE, C, M, RI_, RM_, RC_),
    move_seq(RC_, RM_, RI_, RM, RC, RI, TMOVES).

% white_cross_sit4(C) :- CUBE = [
%         [_,_,_,
%          _,_,_,
%          _,_,_],
%         [_,_,_,
%          _,_,_,
%          _,_,_],
%         [_,_,_,
%          _,_,_,
%          _,_,_],
%         [_,_,_,
%          _,_,_,
%          _,_,_],
%         [_,_,_,
%          _,_,_,
%          _,_,_],
%         [_,_,_,
%          _,_,_,
%          _,_,_]
%     ].

solve_ids(C, I, RM, RC, RI, D) :-
    solve_ids_step(C, [], I, RM, RC, RI, D);
    init_state(IC),
    clear_states,
    add_state(IC, NI),
    ND is D + 1,
    solve_ids(C, NI, RM, RC, RI, ND).

cube_fix(C, FC) :-
    back(BACK_I),
    left(LEFT_I),
    rotate_cube_side_cw(C, BACK_I, C1),
    rotate_cube_side_cw(C1, BACK_I, C2),
    rotate_cube_side_cw(C2, LEFT_I, C3),
    rotate_cube_side_cw(C3, LEFT_I, FC).

solve(RC, RM, RI) :-
    done_cube(C),
    %scramble(FC),
    %read_cube(FC, NL),
    add_state(C, NI),
    solve_ids(C, NI, RM, RC, RI, 0).


scramble(R______) :-
    done_cube(C),
    % move_e_cw(C, _, _, _, RC_),
    % move_f_ccw(RC_, _, _, _, RRC_),
    % move_e_ccw(RRC_, _, _, _, R_),
    % move_e_ccw(R_, _, _, _, R__),
    move_m_ccw(C, _, _, _, R___),
    move_b_ccw(R___, _, _, _, R____),
    move_m_cw(R____, _, _, _, R______).

main(Argv) :-
    solve(_, _, _).
