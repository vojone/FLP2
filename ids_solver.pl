:- consult("cube").

:- dynamic(ids_state_/1).

write_moves_([]).
write_moves_([(HMOVE, _)|T]) :- write(HMOVE), write(" "), write_moves_(T).
write_moves(MOVES) :- reverse(MOVES, RMOVES), write_moves_(RMOVES), write("\n").

check_ids_state(C) :- ids_state_(C), !, false.
check_ids_state(_).

save_ids_state(C) :- asserta(ids_state_(C)).
remove_ids_state(C) :- retract(ids_state_(C)).
clear_ids_states :- retractall(ids_state_(_)).

/** Complete solution by IDS **/
solve_ids_step(_, _, _, _, D) :-  D =< 0, !, false.
solve_ids_step(C, M, M, C, _) :- cube_done(C), !.
solve_ids_step(C, M, RM, RC, D) :- move_u_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_u_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_d_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_d_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_r_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_r_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_l_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_l_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_f_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_f_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_b_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_b_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_m_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_m_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_e_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_e_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_s_cw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).
solve_ids_step(C, M, RM, RC, D) :- move_s_ccw(C, M, M_, RC_), check_ids_state(RC_), save_ids_state(RC_), ND is D - 1, solve_ids_step(RC_, M_, RM, RC, ND).


solve_ids(C, RM, RC, D, _) :- save_ids_state(C), solve_ids_step(C, [], RM, RC, D), !.
solve_ids(C, RM, RC, D, MD) :- D =< MD, ND is D + 1, clear_ids_states, solve_ids(C, RM, RC, ND, MD).

