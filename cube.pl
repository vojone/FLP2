% cube(C) :- C = [
%         [
%             [2, 2, 5],
%             [1, 1, 5],
%             [1, 1, 5]
%         ],
%         [
%             [3, 2, 2],
%             [3, 2, 2],
%             [3, 2, 2]
%         ],
%         [
%             [6, 4, 4],
%             [6, 3, 3],
%             [6, 3, 3]
%         ],
%         [
%             [1, 1, 1],
%             [4, 4, 4],
%             [4, 4, 4]
%         ],
%         [
%             [5, 5, 3],
%             [5, 5, 3],
%             [5, 5, 4]
%         ],
%         [
%             [6, 6, 2],
%             [6, 6, 1],
%             [6, 6, 1]
%         ]
%     ].

% set_prolog_flag(answer_write_options,[max_depth(0)]).

cube(C) :- C = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8],
        [9, 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53]
    ].

cube_size(3).


front(0).
right(1).
back(2).
left(3).
top(4).
bot(5).

side_num(4).

cube_max_i(I) :-
    cube_size(SIDE),
    I is SIDE - 1.

% cube_rotate_top_cw(C, NC) :- .

% rotate90([[H|T]|TT], [[H]|RMT]) :- .

% last([H], H) :- !.
% last([_|T], Last) :- last(T, Last). 

% shift_listr([H], [], H) :- !.
% shift_listr([H|T], [H|NT], Last) :- shift_listr(T, NT, Last).

% shift_listl([H], H, []) :- !.
% shift_listl([H|T], H, [NH|NT]) :- shift_listl(T, NH, NT).

% rotate_listr(L, [Last|NL]) :- shift_listr(L, NL, Last).

% rotate_listl(L, NL) :- shift_listl(L, NH, NT), append(NT, [NH], NL).


% column_indeces(CI, C) :- cube_size(SIDE), cube_i_range(X), C is CI + X * SIDE.


% column(S, CI, C) :- column_indeces(CI, I), nth0(I, S, C).

% between_rev(HIGH, LOW, X) :- between(LOW, HIGH, X_), X is HIGH - X_ + LOW. 


element(S, CI, RI, E) :- cube_size(SIDE), I is CI + RI * SIDE, nth0(I, S, E).



% Rotation of side (plane/matrix) by 90° clockwise
% S - Side (plane)
% RS - Rotated plane
rotate_side_cw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between(0, MAX_I, CI), % For each column from 0 to MAX_I (included)
            between_rev(MAX_I, 0, RI), % For each row from MAX_I to 0 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


% Rotation of side (plane/matrix) by 90° COUNTER clockwise
% S - Side (plane)
% RS - Rotated plane
rotate_side_ccw(S, RS) :-
    cube_max_i(MAX_I),
    findall(E,
        (
            between_rev(MAX_I, 0, CI), % For each column from MAX_I to 0 (included)
            between(0, MAX_I, RI), % For each row from 0 to MAX_I 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).


replace(_, [], _, _) :- !.
replace(I, [_|T], N, NL) :- I = 0, NL = [N|T], !.
replace(I, [H|T], N, [H|NT]) :- I > 0, NI is I - 1, replace(NI, T, N, NT).

replace_by_list(_, [], _, []) :- !.
replace_by_list(_, L, [], L) :- !.
replace_by_list(I, [_|T], [NH|NT], NL) :- I = 0, replace_by_list(0, T, NT, NLT), NL = [NH|NLT], !.
replace_by_list(I, [H|T], N, [H|NT]) :- I > 0, NI is I - 1, replace_by_list(NI, T, N, NT).

slice(_, _, [], []) :- !.
slice(I, N, _, []) :- I = 0, N = 0, !.
slice(I, N, [H|LT], [H|ST]) :- I = 0, N > 0, NN is N - 1, slice(0, NN, LT, ST), !.
slice(I, N, [_|T], S) :- I > 0, NI is I - 1, slice(NI, N, T, S).


% Rotation of level in cube by 90° clockwise
rotate_hlevel_cv(C, LI, RC) :-
    cube_size(SIZE),
    cube_max_i(MAX_I),
    side_num(SIDE_NUM),
    I is MAX_I * LI,
    START_SIDE_I is SIDE_NUM - 1,
    END_SIDE_I is START_SIDE_I + SIDE_NUM - 1,
    findall(S,
        (
            between(START_SIDE_I, END_SIDE_I, SIDE_I_),
            SIDE_I is (SIDE_I_ mod SIDE_NUM),
            SUCC_SIDE_I is (SIDE_I + 1) mod SIDE_NUM,
            nth0(SIDE_I, C, SIDE),
            nth0(SUCC_SIDE_I, C, SUCC_SIDE),
            slice(I, SIZE, SIDE, SIDE_ROW),
            replace_by_list(I, SUCC_SIDE, SIDE_ROW, S)
        ), SIDES),
    replace_by_list(0, C, SIDES, RC).

% rotate_side_cw(S, RS) :- .

% res(X) :- cube(C), nth0(8, C, X).

% cube_alt(C) :- C = [12321321221211111111111111111111111111111111111].


