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

cube(C) :- C = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8],
        [2, 2, 5, 5, 5, 1, 1, 1, 2],
        [2, 2, 5, 5, 5, 1, 1, 1, 2],
        [2, 2, 5, 5, 5, 1, 1, 1, 1],
        [2, 2, 5, 5, 5, 1, 1, 1, 1],
        [2, 2, 5, 5, 5, 1, 1, 1, 1]
    ].

cube_size(3).

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


rotate_side_ccw(S, RS) :-
    cube_irange(MAX_I),
    findall(E,
        (
            between_rev(MAX_I, 0, CI), % For each column from MAX_I to 0 (included)
            between(0, MAX_I, RI), % For each row from 0 to MAX_I 
            element(S, CI, RI, E) % Pick element on the current coordinates
        ), RS).



% rotate_side_cw(S, RS) :- .

% replace(_, [], _, _) :- !.
% replace(I, [_|T], N, NL) :- I = 0, NL = [N|T], !.
% replace(I, [H|T], N, [H|NT]) :- I > 0, NI is I - 1, replace(NI, T, N, NT).

% res(X) :- cube(C), nth0(8, C, X).

% cube_alt(C) :- C = [12321321221211111111111111111111111111111111111].


