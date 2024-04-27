:- consult("cube").

:- dynamic(ids_state_/1).


check_ids_state(C) :- ids_state_(C), !, false.
check_ids_state(_).

save_ids_state(C) :- asserta(ids_state_(C)).
remove_ids_state(C) :- retract(ids_state_(C)).
clear_ids_states :- retractall(ids_state_(_)).


ids_move(C, M, M_, RC_) :-
    move_u_cw(C, M, M_, RC_);
    move_u_ccw(C, M, M_, RC_);
    move_d_cw(C, M, M_, RC_);
    move_d_ccw(C, M, M_, RC_);
    move_r_cw(C, M, M_, RC_);
    move_r_ccw(C, M, M_, RC_);
    move_l_cw(C, M, M_, RC_);
    move_l_ccw(C, M, M_, RC_);
    move_f_cw(C, M, M_, RC_);
    move_f_ccw(C, M, M_, RC_);
    move_b_cw(C, M, M_, RC_);
    move_b_ccw(C, M, M_, RC_);
    move_m_cw(C, M, M_, RC_);
    move_m_ccw(C, M, M_, RC_);
    move_e_cw(C, M, M_, RC_);
    move_e_ccw(C, M, M_, RC_);
    move_s_cw(C, M, M_, RC_);
    move_s_ccw(C, M, M_, RC_).


/** Complete solution by IDS **/
solve_ids_step(C, C, M, M, DONE_C, _, _) :- call(DONE_C, C), !. % If cube satisfies "done condition", stop and return the cube as the result 
solve_ids_step(_, _, _, _, _, D, MD) :-  D >= MD, !, false. % If current max depth is lower than current depth, stop with false
solve_ids_step(C, RC, M, RM, DC, D, MD) :- ids_move(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_u_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_u_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_d_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_d_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_r_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_r_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_l_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_l_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_f_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_f_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_b_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_b_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_m_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_m_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_e_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_e_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_s_cw(C, M, M_, RC_),  ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).
% solve_ids_step(C, RC, M, RM, DC, D, MD) :- move_s_ccw(C, M, M_, RC_), ND is D + 1, solve_ids_step(RC_, RC, M_, RM, DC, ND, MD).


solve_ids(C, RM, DONE_C, D, _) :- solve_ids_step(C, _, [], RM, DONE_C, 0, D), !.
solve_ids(C, RM, DONE_C, D, MD) :- D < MD, ND is D + 1, solve_ids(C, RM, DONE_C, ND, MD).


solve_ids(C, DONE_CLAUSE, MD, RM) :- solve_ids(C, RM, DONE_CLAUSE, 0, MD).
