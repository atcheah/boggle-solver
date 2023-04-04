% dynamically define the list of characters
characters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).

% generate N by N boggle board
generate_boggle_board(N, M) :-
    length(M, N),
    maplist(generate_boggle_row(N), M).

% generate a boggle row of length N
generate_boggle_row(N, R) :-
    length(R, N),
    maplist(generate_random_character, R).

% generate a random character
generate_random_character(C) :-
    characters(Cs), 
    length(Cs, L),
    random(0, L, I),
    nth0(I, Cs, C).

% words must be at least three letters in length
min_word_length(3).

% check neighbour in board, a neighbour is a letter that is horizontal, vertical, or diagonal neighbor of the previous letter
check_neighbour(A, B, Board) :-
    nth0(Y_A, Board, Row_A),
    nth0(X_A, Row_A, A),
    nth0(Y_B, Board, Row_B),
    nth0(X_B, Row_B, B),
    X_Diff is X_A - X_B,
    Y_Diff is Y_A - Y_B,
    abs(X_Diff) =< 1,
    abs(Y_Diff) =< 1,
    different_cells(X_A, Y_A, X_B, Y_B).

% check if two cells are different
different_cells(X_A, Y_A, X_B, Y_B) :-
    X_A \= X_B;
    Y_A \= Y_B.

% no individual cell can be used more than once in a word
% UsedPositions is a list of lists, each list is a pair of X and Y coordinates
% e.g. [[0, 0], [1, 0], [2, 0]]. Needed as we have randomly generated boards,
% so we can't trust the a character's position in the board is unique, as it may be repeated in the board
check_letter_used(A, Board, UsedPositions) :-
    nth0(Y_A, Board, Row_A),
    nth0(X_A, Row_A, A),
    \+ member([X_A, Y_A], UsedPositions).