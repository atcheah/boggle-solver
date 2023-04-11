:- [wn_s].
:- dynamic used/2.
:- use_module(library(pce)).

% dynamically define the list of characters
characters(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']).

% Facts about adjacent indexes for a board. First argument indicates the dimension
adjacent_indexes(3, 0, [1,3,4]).
adjacent_indexes(3, 1, [0,3, 4,5,2]).
adjacent_indexes(3, 2, [1,4,5]).
adjacent_indexes(3, 3, [0,1,4,7,6]).
adjacent_indexes(3, 4, [0,1,2,3,5,7,6,8]).
adjacent_indexes(3, 5, [2,1,4,7,8]).
adjacent_indexes(3, 6, [3,4,7]).
adjacent_indexes(3, 7, [6,3,4,5,8]).
adjacent_indexes(3, 8, [5,4,7]).

adjacent_indexes(4, 0, [1,5,4]).
adjacent_indexes(4, 3, [2,6,7]).
adjacent_indexes(4, 4, [0,1,5,9,8]).
adjacent_indexes(4, 8, [4,5,9,13,12]).
adjacent_indexes(4, 12, [8,9,13]).
adjacent_indexes(4, 7, [3,2,6,10,11]).
adjacent_indexes(4, 11, [7,6,10,14,15]).

adjacent_indexes(4, Index, AdjacentIndexes) :-
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



% generate N by N boggle board
generate_boggle_board(N, Result) :-
    length(M, N),
    maplist(generate_boggle_row(N), M),
    flatten(M, Result).

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

solve_boggle_board(Board, Words) :-
    findall(Word, (nth0(Index, Board, X), solve_boggle_board_helper(Board, Index, [Index], [X], Word)), Words).

solve_boggle_board_helper(_, _, _, Word, Word) :-
    atom_chars(Atom, Word),
    atom_length(Atom, Length),
    atom_string(Atom,X),
    min_word_length(L),
    Length > L,
    dictionary(X).

solve_boggle_board_helper(Board, Index, UsedIndexes, Word, Result) :-
    length(Board, Length),
    Dimension = sqrt(Length),
    adjacent_indexes(Dimension, Index, Positions), 
    member(NextIndex, Positions),
    \+ member(NextIndex, UsedIndexes),
    nth0(NextIndex, Board, Letter),
    append(Word, [Letter], NewWord),
    solve_boggle_board_helper(Board, NextIndex, [NextIndex|UsedIndexes], NewWord, Result).



dictionary(A) :-
    string_to_atom(A, X),
    s(_,_,X,_,_,_).

% boggle board solver where we pass in the board
run(3,M):-
    new(@p, picture('Boggle Board')),
    send(@p, open),

    nth0(0, M, A),
    nth0(1, M, B),
    nth0(2, M, C),
    nth0(3, M, D),
    nth0(4, M, E),
    nth0(5, M, F),
    nth0(6, M, G),
    nth0(7, M, H),
    nth0(8, M, I),

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
    flatten_and_concat(Words, FinalText),
    send(@p, display, new(@answer2, text(FinalText)), point(100,170)).

% boggle board solver where we board is randlomly generated for 3x3
run(3):-
    generate_boggle_board(3, M),
    run(3,M).


% run function for a 4x4 board.
run(4):-
    new(@p, picture('Boggle Board')),
    send(@p, open),

    % Maps generate boggle to gui
    generate_boggle_board(4, M),
    nth0(0, M, A),
    nth0(1, M, B),
    nth0(2, M, C),
    nth0(3, M, D),
    nth0(4, M, E),
    nth0(5, M, F),
    nth0(6, M, G),
    nth0(7, M, H),
    nth0(8, M, I),
    nth0(9, M, J),
    nth0(10, M, K),
    nth0(11, M, L),
    nth0(12, M, M1),
    nth0(13, M, N),
    nth0(14, M, O),
    nth0(15, M, P),

    %Row 1
    send(@p, display, new(@box1, box(50,50))),
    send(@p, display, new(@text1, text(A)), point(10,10)),
    send(@p, display, new(@box2, box(100,50))),
    send(@p, display, new(@text2, text(B)), point(60,10)),
    send(@p, display, new(@box3, box(150,50))),
    send(@p, display, new(@text3, text(C)), point(110,10)),
    send(@p, display, new(@box4, box(200,50))),
    send(@p, display, new(@text4, text(D)), point(160,10)),

    %2
    send(@p, display, new(@box5, box(50,100))),
    send(@p, display, new(@text5, text(E)), point(10,50)),
    send(@p, display, new(@box6, box(100,100))),
    send(@p, display, new(@text6, text(F)), point(60,50)),
    send(@p, display, new(@box7, box(150,100))),
    send(@p, display, new(@text7, text(G)), point(110,50)),
    send(@p, display, new(@box8, box(200,100))),
    send(@p, display, new(@text8, text(H)), point(160,50)),

    %3
    send(@p, display, new(@box9, box(50,150))),
    send(@p, display, new(@text9, text(I)), point(10,100)),
    send(@p, display, new(@box10, box(100,150))),
    send(@p, display, new(@text10, text(J)), point(60,100)),
    send(@p, display, new(@box11, box(150,150))),
    send(@p, display, new(@text11, text(K)), point(110,100)),
    send(@p, display, new(@box12, box(200,150))),
    send(@p, display, new(@text12, text(L)), point(160,100)),

    %4
    send(@p, display, new(@box13, box(50,200))),
    send(@p, display, new(@text13, text(M1)), point(10,150)),
    send(@p, display, new(@box14, box(100,200))),
    send(@p, display, new(@text14, text(N)), point(60,150)),
    send(@p, display, new(@box15, box(150,200))),
    send(@p, display, new(@text15, text(O)), point(110,150)),
    send(@p, display, new(@box16, box(200,200))),
    send(@p, display, new(@text16, text(P)), point(160,150)),

    % Display the answers
    send(@p, display, new(@answer1, text('The possible words are: ')), point(100,150)),
    solve_boggle_board([A,B,C,D,E,F,G,H,I], WordsWithDuplicate),
    sort(WordsWithDuplicate, Words),
    flatten_and_concat(Words, FinalText),
    send(@p, display, new(@answer2, text(FinalText)), point(100,170)).

% Turns [['f', 'o', 'o'], ['b', 'a', 'r']] into ['foo', 'bar']
concat_all([], []).
concat_all([H|T], Result) :-
    concat_all(T, Result2),    
    append(H, [',',' '], Result3),
    append(Result3, Result2, Result).

% Takes a list, uses concat_all on it, and returns the string version
flatten_and_concat(List, Result) :-
    concat_all(List, Result2),
    flatten(Result2, Result3),
    string_chars(Result, Result3).

