:- [wn_s].
:- dynamic used/2.
:- use_module(library(pce)).

% dynamically define the list of characters
characters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).

% Facts about adjacent indexes
adjacent_indexes(0, [1,3,4]).
adjacent_indexes(1, [0,3, 4,5,2]).
adjacent_indexes(2, [1,4,5]).
adjacent_indexes(3, [0,1,4,7,6]).
adjacent_indexes(4, [0,1,2,3,5,7,6,8]).
adjacent_indexes(5, [2,1,4,7,8]).
adjacent_indexes(6, [3,4,7]).
adjacent_indexes(7, [6,3,4,5,8]).
adjacent_indexes(8, [5,4,7]).

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

adjacent_positions(Index, AdjacentIndexes) :-
    adjacent_indexes(Index, AdjacentIndexes).



dictionary(A) :-
    string_to_atom(A, X),
    s(_,_,X,_,_,_).

% run('z','e','b','r','a','l','t','s','e').
run(A, B, C, D, E, F, G, H, I) :-
    new(@p, picture('Boggle Board')),
    send(@p, open),

    % First row
    send(@p, display, new(@box1, box(50,50))),
    send(@p, display, new(@text1, text(A)), point(10,10)),
    send(@p, display, new(@box2, box(100,50))),
    send(@p, display, new(@text2, text(B)), point(60,10)),
    send(@p, display, new(@box3, box(150,50))),
    send(@p, display, new(@text3, text(C)), point(110,10)),

    % Second row
    send(@p, display, new(@box5, box(50,100))),
    send(@p, display, new(@text5, text(D)), point(10,50)),
    send(@p, display, new(@box6, box(100,100))),
    send(@p, display, new(@text6, text(E)), point(60,50)),
    send(@p, display, new(@box7, box(150,100))),
    send(@p, display, new(@text7, text(F)), point(110,50)),

    % Third row
    send(@p, display, new(@box9, box(50,150))),
    send(@p, display, new(@text9, text(G)), point(10,100)),
    send(@p, display, new(@box10, box(100,150))),
    send(@p, display, new(@text10, text(H)), point(60,100)),
    send(@p, display, new(@box11, box(150,150))),
    send(@p, display, new(@text11, text(I)), point(110,100)),

    % Display the answers
    send(@p, display, new(@answer1, text('The possible words are: ')), point(100,150)),
    solve_boggle_board([A,B,C,D,E,F,G,H,I], WordsWithDuplicate),
    sort(WordsWithDuplicate, Words),
    concat_all(Words, [], Result),
    flatten_and_concat(Result, FinalText),
    send(@p, display, new(@answer2, text(FinalText)), point(100,170)).

% Turns [['f', 'o', 'o'], ['b', 'a', 'r']] into ['foo', 'bar']
concat_all([], Result, Result).
concat_all([H|T], RSF, Result) :-
    atomic_list_concat(H, '', Atm),
    atom_string(Atm, Str),
    string_concat(Str, " ", Str_with_space),
    concat_all(T, [Str_with_space|RSF], Result).

% Turns ['foo', 'bar'] into "foo bar"
flatten_and_concat(A, Result) :-
    flatten(A, L),
    atomic_list_concat(L, ",", Result).
