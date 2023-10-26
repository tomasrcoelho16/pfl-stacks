% Define a piece as a compound term with color (red or black) and stack size.
% Example: red(2) represents a stack of 2 red pieces.
% empty represents an empty cell.

initial_state([
    [red(2), red(2), red(2), red(2), red(2)],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [black(2), black(2), black(2), black(2), black(2)]
]).

% Define a predicate to display the game board.
display_board(Board) :-
    nl,
    display_columns,
    display_rows(Board, 1),
    display_separator.

display_columns :-
    write('     A    B    C    D    E\n').

display_separator :-
    write('  --------------------------\n').

display_rows([], 8).
display_rows([Row | Rest], RowNumber) :-
    display_separator,
    write(RowNumber),
    write(' '),
    display_row(Row),
    nl,
    NextRowNumber is RowNumber + 1,
    display_rows(Rest, NextRowNumber).

display_row([]) :- 
    write('|').
display_row([Cell | Rest]) :-
    write('| '),
    display_cell(Cell),
    display_row(Rest).

display_cell(empty) :-
    write('   ').
display_cell(red(N)) :-
    format('R~d ', [N]).
display_cell(black(N)) :-
    format('B~d ', [N]).



% Move a piece from one column to another.
move_piece([Row1|Rows], FromCol, ToCol, NewGameState) :-
    valid_move(FromCol, ToCol),
    select_stack(Row1, FromCol, Stack, RestRow),
    piece_size(Stack, Size), % Get the size of the stack
    % Update the stack size (if you are stacking or unstacking)
    NewSize is Size - 1, % For unstacking
    % NewSize is Size + 1, % For stacking
    update_stack(Stack, NewSize, NewStack),
    place_piece(RestRow, ToCol, NewStack, NewRow),
    append([NewRow], Rows, NewGameState).

% Select a stack from a row.
select_stack(Row, Col, Stack, RestRow) :-
    nth1(Col, Row, Stack),
    select(Stack, Row, RestRow).

% Get the size of the stack (0 if empty).
piece_size(empty, 0).
piece_size(red(Size), Size) :- Size > 0.
piece_size(black(Size), Size) :- Size > 0.

% Update the stack size.
update_stack(empty, _, empty). % Clear the cell if unstacked completely.
update_stack(red(Size), NewSize, NewStack) :- NewSize > 0, NewSize =< 4, NewStack = red(NewSize).
update_stack(black(Size), NewSize, NewStack) :- NewSize > 0, NewSize =< 4, NewStack = black(NewSize).
