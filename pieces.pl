:- use_module(library(lists)).

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


% Choose move for a human player (select piece and destination)
choose_move(GameState, human, From-To) :-
    %display_board(GameState),
    write('Select a piece (e.g., a1): '),
    read(From),    % Read the coordinate of the piece to move

    write('Select a destination (e.g., b2): '),
    read(To),      % Read the coordinate for the destination

    From-To = (From, To).

% para o bot
choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).


% Define a predicate to convert a user input like "A2" to column and row coordinates.
user_input_to_coordinates(UserInput, (Row, Col)) :-
    atom_chars(UserInput, [ColChar, RowDigit]),
    char_code(ColChar, ColCode),
    Col is ColCode - 64, % Convert ASCII value to column number (A=1, B=2, ...)
    number_chars(Row, [RowDigit]).

%MOVER A PUTA DAS PECAS

move_piece(Board, NewBoard) :-
    replace(Board, 1, 3, empty, TempBoard),
    replace(TempBoard, 4, 3, red(2), NewBoard).

replace(Board, Row, Column, Piece, NewBoard) :-
    replace_row(Board, Row, Column, Piece, NewBoard).

% Replace an element in a row
replace_row([Row | Rest], 1, Column, Piece, [NewRow | Rest]) :-
    replace_element(Row, Column, Piece, NewRow).

replace_row([Row | Rest], RowNum, Column, Piece, [Row | NewRest]) :-
    RowNum > 1,
    NextRowNum is RowNum - 1,
    replace_row(Rest, NextRowNum, Column, Piece, NewRest).

% Replace an element in a list
replace_element([_|Rest], 1, Piece, [Piece | Rest]).

replace_element([X | Rest], Column, Piece, [X | NewRest]) :-
    Column > 1,
    NextColumn is Column - 1,
    replace_element(Rest, NextColumn, Piece, NewRest).

% AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA


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


% TESTE

piece_value(red(N), N).
piece_value(black(N), N).
