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
        [2, 2, 5, 5, 5, 1, 1, 1, 1],
        [2, 2, 5, 5, 5, 1, 1, 1, 2],
        [2, 2, 5, 5, 5, 1, 1, 1, 2],
        [2, 2, 5, 5, 5, 1, 1, 1, 1],
        [2, 2, 5, 5, 5, 1, 1, 1, 1],
        [2, 2, 5, 5, 5, 1, 1, 1, 1]
    ].

% cube_rotate_top_cw(C, NC) :- .

% rotate90([[H|T]|TT], [[H]|RMT]) :- .

last([H], H) :- !.
last([_|T], Last) :- last(T, Last). 

shift_listr([H], [], H) :- !.
shift_listr([H|T], [H|NT], Last) :- shift_listr(T, NT, Last).

shift_listl([H], H, []) :- !.
shift_listl([H|T], H, [NH|NT]) :- shift_listl(T, NH, NT).

rotate_listr(L, [Last|NL]) :- shift_listr(L, NL, Last).

rotate_listl(L, NL) :- shift_listl(L, NH, NT), append(NT, [NH], NL).

% rotate_side([H|T], [H|RT]) :- RT.

% replace(_, [], _, _) :- !.
% replace(I, [_|T], N, NL) :- I = 0, NL = [N|T], !.
% replace(I, [H|T], N, [H|NT]) :- I > 0, NI is I - 1, replace(NI, T, N, NT).

% res(X) :- cube(C), nth0(8, C, X).

% cube_alt(C) :- C = [12321321221211111111111111111111111111111111111].


