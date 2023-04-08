
:- dynamic used/2.

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
% e.g. [[0, 0], [1, 0], [2, 0]]. Needed as we have randomly generated boards, so we can't not retrieve a letters position from the board
% as it could be duplicated
check_letter_used(X_A, Y_A, UsedPositions) :-
    \+ member([X_A, Y_A], UsedPositions).


solve_boggle_board(Board, Words) :-
    findall(Word, (nth0(Index, Board, X), solve_boggle_board_helper(Board, Index, [Index], [X], Word)), Words).

solve_boggle_board_helper(_, _, _, Word, Word) :-
    atom_chars(Atom, Word),
    atom_length(Atom, Length),
    atom_string(Atom,X),
    Length > 2,
    dictionary(X).

solve_boggle_board_helper(Board, Index, UsedIndexes, Word, Result) :-
    adjacent_positions(Index, Positions), 
    member(NextIndex, Positions),
    \+ member(NextIndex, UsedIndexes),
    nth0(NextIndex, Board, Letter),
    append(Word, [Letter], NewWord),
    solve_boggle_board_helper(Board, NextIndex, [NextIndex|UsedIndexes], NewWord, Result).

adjacent_positions(Index, ValidIndexes) :-
    adjacent_indexes(Index, AdjacentIndexes),
    include(is_valid_index, AdjacentIndexes, ValidIndexes).

adjacent_indexes(0, [1,5,4]).
adjacent_indexes(3, [2,6,7]).
adjacent_indexes(4, [0,1,5,9,8]).
adjacent_indexes(8, [4,5,9,13,12]).
adjacent_indexes(12, [8,9,13]).
adjacent_indexes(7, [3,2,6,10,11]).
adjacent_indexes(11, [7,6,10,14,15]).

adjacent_indexes(Index, AdjacentIndexes) :-
   \+ member(Index, [0,3,4,8,12,7,11]),
    Top is Index - 4,
    Bottom is Index + 4,
    Left is Index - 1,
    Right is Index + 1,
    TopLeft is Top - 1,
    TopRight is Top + 1,
    BottomLeft is Bottom - 1,
    BottomRight is Bottom + 1,
    AdjacentIndexes = [Top, Bottom, Left, Right, TopLeft, TopRight, BottomLeft, BottomRight].

is_valid_index(Index) :-
    Index >= 0,
    Index =< 15.


dictionary("zebra").

% board to use to test for zebra ['z','e','b','r','a','a','a','a','a','a','a','a','a','a','a','a']
