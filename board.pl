% Define a predicate to display the game board.
display_board(Board) :-
    nl,
    display_columns,
    display_separator,
    display_rows(Board, 1).

display_columns :-
    write('   A   B   C   D   E\n').

display_separator :-
    write('  -------------------\n').

display_rows([], 6).
display_rows([Row | Rest], RowNumber) :-
    display_separator,
    write(RowNumber),
    write(' '),
    display_row(Row),
    nl,
    NextRowNumber is RowNumber + 1,
    display_rows(Rest, NextRowNumber).

display_row([]).
display_row([Cell | Rest]) :-
    write('| '),
    display_cell(Cell),
    display_row(Rest).

display_cell(empty) :-
    write(' ').
display_cell(red(N)) :-
    format('R~d', [N]).
display_cell(black(N)) :-
    format('B~d', [N]).
