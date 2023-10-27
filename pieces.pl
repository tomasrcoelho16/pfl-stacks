:- use_module(library(lists)).

% Define a piece as a compound term with color (red or black) and stack size.
% Example: red(2) represents a stack of 2 red pieces.
% empty represents an empty cell.

initial_state([
    [red(3), red(1), red(2), red(4), red(2)],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [black(1), black(2), black(3), black(2), black(2)]
]).

% Define a predicate to display the game board.
display_board(Board) :-
    nl,
    display_columns,
    display_rows(Board, 1),
    display_separator.

display_columns :-
    write('     a    b    c    d    e\n').

display_separator :-
    write('  --------------------------\n').

display_rows([], 8).
display_rows([Row | Rest], RowNumber) :-
    display_separator,
    write(RowNumber),
    write(' '),
    display_row(Row),
    (RowNumber =:= 1 -> write('   Max movement per stack: ') ; true),
    (RowNumber =:= 3 -> write('   Stack of 4: cannot move') ; true),
    (RowNumber =:= 4 -> write('   Stack of 3: one space') ; true),
    (RowNumber =:= 5 -> write('   Stack of 2: two spaces') ; true),
    (RowNumber =:= 6 -> write('   Stack of 1: three spaces') ; true),
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

calculate_possible(Val, Possible) :-
    Possible is 4 - Val.

% Choose move for a human player (select piece and destination)
choose_move(GameState, Player, Move) :-
    %display_board(GameState),
    repeat,
    write('Select a piece (e.g., a1): '),
    read(FromInput),

    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),

    ( (Player = black, Piece = black(_)) ;
      (Player = red, Piece = red(_)) ),

    piece_value(Piece, Val),
    
    user_input_to_coordinates(FromInput, (FromRow, FromCol)), 

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    user_input_to_coordinates(ToInput, (ToRow, ToCol)),

    write('How many pieces do you want (e.g., 1, 2, 3, 4): '),
    read(NPiecesInput), 

    (NPiecesInput =< Val),

    calculate_possible(NPiecesInput, Possible),
    (abs(FromRow - ToRow) =< Possible, abs(FromCol - ToCol) =< Possible),

    nth1(ToRow, GameState, RowEnemy),
    nth1(ToCol, RowEnemy, PieceEnemy),
    piece_value(PieceEnemy, ValEnemy),
    
    ( (Player = black, PieceEnemy = black(_), (NPiecesInput+ValEnemy) =< 4,    
        NewValue is NPiecesInput + ValEnemy,
        PieceTo = black(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-NPiecesInput))
        ) ;
      (Player = red, PieceEnemy = red(_), (NPiecesInput+ValEnemy) =< 4,
        NewValue is NPiecesInput + ValEnemy,
        PieceTo = red(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = red(Val-NPiecesInput))
        ) ;
      (Player = red, (PieceEnemy = black(_); PieceEnemy = empty), (NPiecesInput > ValEnemy),
         NewValue is NPiecesInput,
         PieceTo = red(NPiecesInput),
        ((NPiecesInput =:= Val -> PieceFrom = empty; PieceFrom = red(Val-NPiecesInput)))
         ) ;
      (Player = black, (PieceEnemy = red(_); PieceEnemy = empty), (NPiecesInput > ValEnemy),
        NewValue is NPiecesInput,
        PieceTo = black(NPiecesInput),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-NPiecesInput))
        )
    ),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),
    Move = (From, To, PieceFrom, PieceTo)
    .

% para o bot
%choose_move(GameState, computer-Level, Move):-
    %valid_moves(GameState, Moves),
    %choose_move(Level, GameState, Moves, Move).


% Define a predicate to convert a user input like "A2" to column and row coordinates.
user_input_to_coordinates(UserInput, (Row, Col)) :-
    atom_chars(UserInput, [ColChar, RowDigit]),
    char_code(ColChar, ColCode),
    Col is ColCode - 96, % Convert ASCII value to column number (A=1, B=2, ...)
    number_chars(Row, [RowDigit]).

%MOVER A PUTA DAS PECAS

% Define a predicate to move a piece from one position to another.
move(GameState, Move, NewGameState) :-
    % Split the coordinates into separate components
    Move = (From, To, PieceFrom, PieceTo),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),

    % Extract the piece from the source position
    nth1(FromRow, GameState, FromRowList),
    %nth1(FromCol, FromRowList, PieceTo),

    % Create the new board with the piece moved
    replace(GameState, FromRow, FromCol, PieceFrom, TempGameState),
    replace(TempGameState, ToRow, ToCol, PieceTo, NewGameState).


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
piece_value(empty, 0).
