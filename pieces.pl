:- use_module(library(lists)).

% Define a piece as a compound term with color (red or black) and stack size.
% Example: red(2) represents a stack of 2 red pieces.
% empty represents an empty cell.

initial_state([
    [red(3), black(1), red(2), red(4), red(2)],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [red(2), red(2), empty, empty, empty],
    [black(3), black(1), black(1), black(2), black(2)]
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

    (((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true);
     (write('That is not allowed.'), nl, fail)          % NAO FAZ SENTIDO SEU FILHO DA PUTA
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    user_input_to_coordinates(ToInput, (ToRow, ToCol)),

    (((Val > 1) -> write('How many pieces do you want (e.g., 1, 2, 3, 4): '), read(NPiecesInput)); NPiecesInput = 1),

    (NPiecesInput =< Val),

    ((ToRow =\= 1, Player = black);
     (ToRow =\= 7, Player = red)
    ),

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
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths),
    (Paths \= []),
    write('Paths: '),
    write(Paths), nl,


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
    %nth1(FromRow, GameState, FromRowList),
    %nth1(FromCol, FromRowList, PieceTo),

    nth1(ToRow, GameState, ToRowList),
    nth1(ToCol, ToRowList, EnemyPiece),

    piece_value(PieceTo, PieceToValue),
    piece_value(EnemyPiece, EnemyPieceValue),

    (PieceTo = red(_) -> Player = red ; Player = black),

    NewValue is (EnemyPieceValue - (PieceToValue - EnemyPieceValue)),



    (
        ((PieceTo = red(_),
         EnemyPiece = black(_), (EnemyPieceValue - (PieceToValue - EnemyPieceValue) > 0)) ->

        

         replace(GameState, ToRow - 1, ToCol, black(EnemyPieceValue - (PieceToValue - EnemyPieceValue)), GameState0) 
         
         
         );
         ((PieceTo = black(_),
         EnemyPiece = red(_), (EnemyPieceValue - (PieceToValue - EnemyPieceValue) > 0)) ->

        retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
        remove_empty_lists(RetreatPositions, RetreatPositionsFixed),
        write('RETREAT:'), nl,
        write_retreat(RetreatPositionsFixed),
        replace(GameState, ToRow - 1, ToCol, red(EnemyPieceValue - (PieceToValue - EnemyPieceValue)), GameState0) 
         
         );
         GameState0 = GameState
    ),


    % Create the new board with the piece moved
    replace(GameState0, FromRow, FromCol, PieceFrom, TempGameState),
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

% TESTE

piece_value(red(N), N).
piece_value(black(N), N).
piece_value(empty, 0).


%PATH

% Predicate to find all possible paths from (FromRow, FromCol) to (ToRow, ToCol) within a given number of moves.
find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths) :-
    findall(Path, possible_path(GameState, FromRow, FromCol, ToRow, ToCol, Possible, [], Path), Paths).

% Predicate to check if a position (X, Y) is valid and within the bounds of the game board.
valid_position(X, Y) :- X >= 1, X =< 7, Y >= 1, Y =< 5.

% Predicate to calculate the next position in a certain direction.
next_position((X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y.
next_position((X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y.
next_position((X, Y), (X1, Y1)) :- X1 is X, Y1 is Y + 1.
next_position((X, Y), (X1, Y1)) :- X1 is X, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X-1, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X+1, Y1 is Y + 1.
next_position((X, Y), (X1, Y1)) :- X1 is X+1, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X-1, Y1 is Y + 1.


% Recursive predicate to find possible paths.
possible_path(_, X, Y, X, Y, 0, Path, Path).
possible_path(_, X, Y, X, Y, 1, Path, Path).
possible_path(_, X, Y, X, Y, 2, Path, Path).
possible_path(GameState, X, Y, ToRow, ToCol, Possible, CurrentPath, Path) :-
    Possible > 0,
    next_position((X, Y), (X1, Y1)), % Get the next position
    valid_position(X1, Y1), % Check if it's a valid position
    %((X1 = ToRow, Y1 = ToCol) -> append(CurrentPath, [(X1, Y1)], NewPath); true),
    nth1(X1, GameState, Row),
    %nth1(Y1, Row, empty),
    (
        Possible > 1 -> 
        ((nth1(Y1, Row, empty)) ; (X1 = ToRow, Y1 = ToCol))
        ; nth1(Y1, Row, _) % se for o ultimo move o espaço nao tem que ser empty
    ), % Check if it's an empty space
    NewPossible is Possible - 1,
    append(CurrentPath, [(X1, Y1)], NewPath),
    possible_path(GameState, X1, Y1, ToRow, ToCol, NewPossible, NewPath, Path).

% Retreat positions for the black player
retreat_positions(black, ToRow, ToCol, RetreatPositions, GameState, NewValue) :-
    RetreatRow is ToRow - 1,
    RetreatCol is ToCol,
    RetreatColLeft is ToCol - 1,
    RetreatColRight is ToCol + 1,
    findall(R, (
        (valid_position(RetreatRow, RetreatCol), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatCol, RetreatRowList, RetreatSpace),
         (RetreatSpace = black(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatCol] ; R = []
    ), RetreatPositions1),
    findall(R, (
        (valid_position(RetreatRow, RetreatColRight), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatColRight, RetreatRowList, RetreatSpace),
         (RetreatSpace = black(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatColRight] ; R = []
    ), RetreatPositions2),
    findall(R, (
        (valid_position(RetreatRow, RetreatColLeft), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatColLeft, RetreatRowList, RetreatSpace),
         (RetreatSpace = black(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatColLeft] ; R = []
    ), RetreatPositions3),
    append([RetreatPositions1, RetreatPositions2, RetreatPositions3], RetreatPositions).



friendly_piece(Player, PieceTo) :-
    (Player = black, PieceTo = black(_));
    (Player = red, PieceTo = red(_)).


write_retreat([[],_]).
write_retreat([[Row, Col] | Rest]):-
    Code is Col + 96,
    char_code(Char, Code),
    write(Char),
    write(Row), nl,
    write(Rest),
    write_retreat(Rest).


remove_empty_lists([], []).
remove_empty_lists([[] | Rest], Result) :- 
    remove_empty_lists(Rest, Result).
remove_empty_lists([X | Rest], [X | Result]) :-
    X \= [],
    remove_empty_lists(Rest, Result).
