:- use_module(library(lists)).

% Define a piece as a compound term with color (red or black) and stack size.
% Example: red(2) represents a stack of 2 red pieces.
% empty represents an empty cell.

initial_state([
    [red(2), red(2), red(2), red(2), red(2)],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, black(1), red(1), empty, empty],
    [empty, black(1), empty, black(1), black(1)],
    [empty, empty, empty, empty, empty],
    [black(2), black(1), black(2), black(2), black(2)]
]).

% display_game(+Board)
% Display the game board on the console, including piece positions and a movement restriction tip in the right side.
display_game(Board) :-
    nl,
    display_columns,
    display_rows(Board, 1),
    display_separator.

% display_columns/0
% Display the column labels on the console.
display_columns :-
    write('     a    b    c    d    e\n').

% display_separator/0
% Display a separator line on the console.
display_separator :-
    write('  --------------------------\n').


% display_rows(+Board, +RowNumber)
% Display the rows of the game board, including piece positions and a movement restriction tip in the right side.
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


% display_row(+Row)
% Display a single row of the game board.
display_row([]) :- 
    write('|').
display_row([Cell | Rest]) :-
    write('| '),
    display_cell(Cell),
    display_row(Rest).

% display_cell(+Cell)
% Display the content of an individual cell.
display_cell(empty) :-
    write('   ').
display_cell(red(N)) :-
    format('R~d ', [N]).
display_cell(black(N)) :-
    format('B~d ', [N]).

% calculate_possible(+Val, -Possible)
% Calculates the maximum number of spaces a piece with the given value can move within the game rules.
calculate_possible(Val, Possible) :-
    Possible is 4 - Val.

% choose_move(+GameState, +Player, -Move, -TwoMovesGamestate)
% Allows a human player to choose between two move options: moving a single stack or moving two individual pieces up to 2 spaces each.
choose_move(GameState, Player, Move, TwoMovesGamestate) :-
    repeat,
    write('Choose one of the turn possibilities:') ,nl,
    write('1 - Move a single stack.') ,nl,
    write('2 - Move two individual pieces up to 2 spaces each.'),nl,
    read(Input),
    (integer(Input) ->
        (Input =:= 1 -> single_move(GameState, Player, Move, TwoMovesGamestate)
        ; Input =:= 2 -> double_move(GameState, Player, Move, TwoMovesGamestate)
        ;
          fail
        )
    ; 
      fail
    )
    .

% single_move(+GameState, +Player, -Move, -TwoMovesGamestate)
% Allows a player to make a single move, moving a piece or a stack of pieces up to a certain distance on the game board.
single_move(GameState,Player, Move, TwoMovesGamestate) :-
    repeat,
    write('Select a piece or stack (e.g., a1): '),
    read(FromInput),
    %i
    (integer(FromInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(FromInput, (FromRow, FromCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),
    %ii
    ( (Player = black, Piece = black(_)) ;
      (Player = red, Piece = red(_)) ),

    piece_value(Piece, Val),

    %iii
    (((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true);
     (write('That is not allowed.'), nl, fail)          % NAO FAZ SENTIDO SEU FILHO DA PUTA
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    % ATAUQE COMBINADO GOES HERE

    %iv
    (integer(ToInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(ToInput, (ToRow, ToCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    %v

    (((Val > 1) -> write('How many pieces do you want (e.g., 1, 2, 3, 4): '), read(NPiecesInput)); NPiecesInput = 1),

    %vi
    (\+integer(NPiecesInput) -> write('Invalid input format.'), nl, fail ; true),

    %vii
    (NPiecesInput =< Val),

    %viii
    calculate_possible(NPiecesInput, Possible),
    (abs(FromRow - ToRow) =< Possible, abs(FromCol - ToCol) =< Possible),

    nth1(ToRow, GameState, RowEnemy),
    nth1(ToCol, RowEnemy, PieceEnemy),
    piece_value(PieceEnemy, ValEnemy),


    %ix
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player),
    (Paths \= []),

    %x
    find_adjacent_pieces(GameState,Player,ToRow,ToCol,AdjacentPieces),
    (((Player = black, PieceEnemy = red(_), AdjacentPieces \= [[Piece-(FromRow,FromCol)]], AdjacentPieces \= []) ; 
    (Player = red, PieceEnemy = black(_), AdjacentPieces \= [[Piece-(FromRow,FromCol)]], AdjacentPieces \= [])) -> 
    is_possible_combinateds(GameState, Player,NPiecesInput, NewVal,AdjacentPieces, FromRow, FromCol, ToRow, ToCol, Paths, GameStateCombinated)
     ;
    true ),

    (
        (integer(NewVal), Yoo is NewVal, TwoMovesGamestate = GameStateCombinated);
        (Yoo is NPiecesInput, TwoMovesGamestate = GameState)
    ),

    %xi
    ( (Player = black, PieceEnemy = black(_), (Yoo+ValEnemy) =< 4,    
        NewValue is Yoo + ValEnemy,
        NewValue2 is Val-NPiecesInput,
        PieceTo = black(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(NewValue2))
        ) ;
      (Player = red, PieceEnemy = red(_), (Yoo+ValEnemy) =< 4,
        NewValue is Yoo + ValEnemy,
        NewValue2 is Val-NPiecesInput,
        PieceTo = red(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = red(NewValue2))
        ) ;
      (Player = red, (PieceEnemy = black(_); PieceEnemy = empty), (Yoo > ValEnemy),
         NewValue is Yoo,
         NewValue2 is Val-NPiecesInput,
         PieceTo = red(Yoo),
        ((NPiecesInput =:= Val -> PieceFrom = empty; PieceFrom = red(NewValue2)))
         ) ;
      (Player = black, (PieceEnemy = red(_); PieceEnemy = empty), (Yoo > ValEnemy),
        NewValue is Yoo,
        NewValue2 is Val-NPiecesInput,
        PieceTo = black(Yoo),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(NewValue2))
        )
    ),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),
    Move = (From, To, PieceFrom, PieceTo)
    .

% double_move(+GameState, +Player, -Move2, -TwoMovesGamestate)
% Allows a player to make a double move, moving two individual pieces up to 2 spaces each on the game board.
double_move(GameState, Player, Move2, TwoMovesGamestate) :-
    repeat,
    write('Select the first piece (e.g., a1): '),
    read(FromInput),

     (integer(FromInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(FromInput, (FromRow, FromCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),

    ( (Player = black, Piece = black(_)) ;
      (Player = red, Piece = red(_)) ),

    piece_value(Piece, Val),

    (((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true);
     (write('That is not allowed.'), nl, fail)
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    (integer(ToInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(ToInput, (ToRow, ToCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    (abs(FromRow - ToRow) =< 2, abs(FromCol - ToCol) =< 2),

    nth1(ToRow, GameState, RowEnemy),
    nth1(ToCol, RowEnemy, PieceEnemy),
    piece_value(PieceEnemy, ValEnemy),

    (
        (Player = black, PieceEnemy = black(_), (1+ValEnemy) =< 4,    
        NewValue is 1 + ValEnemy,
        NewValue3 is Val-1,
        PieceTo = black(NewValue),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = black(NewValue3))
        ) ;
        (Player = red, PieceEnemy = red(_), (1+ValEnemy) =< 4,
        NewValue is 1 + ValEnemy,
        NewValue3 is Val-1,
        PieceTo = red(NewValue),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = red(NewValue3))
        ) ;
        (Player = red, PieceEnemy = empty, (1 > ValEnemy),
         NewValue is 1,
         NewValue3 is Val-1,
         PieceTo = red(1),
        ((1 =:= Val -> PieceFrom = empty; PieceFrom = red(NewValue3)))
         ) ;
      (Player = black, PieceEnemy = empty, (1 > ValEnemy),
        NewValue is 1,
        NewValue3 is Val-1,
        PieceTo = black(1),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = black(NewValue3))
        )
    ),
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, 2, Paths, Player),
    (Paths \= []),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),
    Move = (From, To, PieceFrom, PieceTo),
    move(GameState, Move, TempGameState),


    display_game(TempGameState),
    repeat,
    write('CURRENT PLAYER: '),
    write(Player),nl,
    write('Select the second piece, you cannot move the same piece again(e.g., a1): '),
    read(FromInput2),

    (integer(FromInput2) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(FromInput2, (FromRow2, FromCol2)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    (ToRow =\= FromRow2; ToCol =\= FromCol2),
    nth1(FromRow2, TempGameState, Row2),
    nth1(FromCol2, Row2, Piece2),

    ( (Player = black, Piece2 = black(_)) ;
      (Player = red, Piece2 = red(_)) ),

    piece_value(Piece2, Val2),

    (((FromRow2 =\= 1, Player = black);
     (FromRow2 =\= 7, Player = red) -> true)
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput2),      % Read the coordinate for the destination

    (integer(ToInput2) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),

    (user_input_to_coordinates(ToInput2, (ToRow2, ToCol2)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),

    (abs(FromRow2 - ToRow2) =< 2, abs(FromCol2 - ToCol2) =< 2),

    nth1(ToRow2, TempGameState, RowEnemy2),
    nth1(ToCol2, RowEnemy2, PieceEnemy2),
    piece_value(PieceEnemy2, ValEnemy2),

    find_possible_paths(TempGameState, FromRow2, FromCol2, ToRow2, ToCol2, 2, Paths2, Player),
    find_adjacent_pieces(TempGameState,Player,ToRow2,ToCol2,AdjacentPieces),
    (((Player = black, PieceEnemy2 = red(_), AdjacentPieces \= [[Piece2-(FromRow2,FromCol2)]], AdjacentPieces \= []) ; (Player = red, PieceEnemy2 = black(_), AdjacentPieces \= [[Piece2-(FromRow2,FromCol2)]], AdjacentPieces \= [])) -> is_possible_combinateds(TempGameState, Player,1, NewVal,AdjacentPieces, FromRow2, FromCol2, ToRow2, ToCol2, Paths2, GameStateCombinated)
     ;
    true ),
    (
        (integer(NewVal), Yoo is NewVal, TwoMovesGamestate = GameStateCombinated);
        (Yoo is 1, TwoMovesGamestate = TempGameState)
    ),
    (
        (Player = black, PieceEnemy2 = black(_), (1+ValEnemy2) =< 4,    
        NewValue2 is 1 + ValEnemy2,
        NewValue4 is Val2-1,
        PieceTo2 = black(NewValue2),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = black(NewValue4))
        ) ;
        (Player = red, PieceEnemy2 = red(_), (1+ValEnemy2) =< 4,
        NewValue2 is 1 + ValEnemy2,
        NewValue4 is Val2-1,
        PieceTo2 = red(NewValue2),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = red(NewValue4))
        ) ;
        (Player = red, (PieceEnemy2 = empty; PieceEnemy2 = black(_)), (Yoo > ValEnemy2),
         NewValue2 is Yoo,
         NewValue4 is Val2-1,
         PieceTo2 = red(Yoo),
        ((1 =:= Val2 -> PieceFrom2 = empty; PieceFrom2 = red(NewValue4)))
         ) ;
      (Player = black, (PieceEnemy2 = empty; PieceEnemy2 = red(_)), (Yoo > ValEnemy2),
        NewValue2 is Yoo,
        NewValue4 is Val2-1,
        PieceTo2 = black(Yoo),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = black(NewValue4))
        )
    ),

    From2 = (FromRow2, FromCol2),
    To2 = (ToRow2, ToCol2),
    Move2 = (From2, To2, PieceFrom2, PieceTo2)
    .

% user_input_to_coordinates(+UserInput, (-Row, -Col))
% Define a predicate to convert a user input like "A2" to column and row coordinates.
user_input_to_coordinates(UserInput, (Row, Col)) :-
    atom_chars(UserInput, [ColChar, RowDigit]),
    char_code(ColChar, ColCode),
    Col is ColCode - 96, % Convert ASCII value to column number (A=1, B=2, ...)
    number_chars(Row, [RowDigit]).

% move(+GameState, +Move, -NewGameState)
% Define a predicate to move a piece from one position to another.
move(GameState, Move, NewGameState) :-
    % Split the move into separate components
    Move = (From, To, PieceFrom, PieceTo),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),

    nth1(ToRow, GameState, ToRowList),
    nth1(ToCol, ToRowList, EnemyPiece),

    piece_value(PieceTo, PieceToValue),
    piece_value(EnemyPiece, EnemyPieceValue),

    (PieceTo = red(_) -> Player = red ; Player = black),

    NewValue is (EnemyPieceValue - (PieceToValue - EnemyPieceValue)),

    retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
    remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
    (
        ((PieceTo = red(_),
         EnemyPiece = black(_), NewValue > 0) ->
         retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
         remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
         repeat,
         (
            (RetreatPositionsFixed \= [] -> (write('Choose a position to retreat the piece to: (  '),
         write_retreat(RetreatPositionsFixed),
         write(')'), nl,
         read(RetreatInput),
        (integer(RetreatInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),
        (user_input_to_coordinates(RetreatInput, (RetreatRow, RetreatCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),
         member([RetreatRow, RetreatCol], RetreatPositionsFixed),
        nth1(RetreatRow, GameState, RetreatRowList),
        nth1(RetreatCol, RetreatRowList, RetreatSpace),
        piece_value(RetreatSpace, RetreatSpaceVal),
        (RetreatSpaceVal + (EnemyPieceValue - (PieceToValue - EnemyPieceValue))) < 4,
        ((RetreatSpace \= empty -> replace(GameState, RetreatRow, RetreatCol, black(EnemyPieceValue - (PieceToValue - EnemyPieceValue) + RetreatSpaceVal), GameState0)
        ;
        replace(GameState, RetreatRow, RetreatCol, black(EnemyPieceValue - (PieceToValue - EnemyPieceValue)), GameState0))
        );GameState0 = GameState)) ; GameState0 = GameState
         )
         );
        ((PieceTo = black(_),
         EnemyPiece = red(_), NewValue > 0) ->
         retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
         remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
         repeat,
        (
            (RetreatPositionsFixed \= [] -> (write('Choose a position to retreat the piece to: (  '),
         write_retreat(RetreatPositionsFixed),
         write(')'), nl,
         read(RetreatInput),
         (integer(RetreatInput) -> write('Invalid input format. Please use the format "a1" or similar.'), nl, fail ; true),
        (user_input_to_coordinates(RetreatInput, (RetreatRow, RetreatCol)) -> true ; write('Invalid input format. Please use the format "a1" or similar.'), nl,
        fail),
         member([RetreatRow, RetreatCol], RetreatPositionsFixed),
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatCol, RetreatRowList, RetreatSpace),
         piece_value(RetreatSpace, RetreatSpaceVal),
         (RetreatSpaceVal + (EnemyPieceValue - (PieceToValue - EnemyPieceValue))) < 4,
         ((RetreatSpace \= empty -> replace(GameState, RetreatRow, RetreatCol, red(EnemyPieceValue - (PieceToValue - EnemyPieceValue) + RetreatSpaceVal), GameState0)
         ;
         replace(GameState, RetreatRow, RetreatCol, red(EnemyPieceValue - (PieceToValue - EnemyPieceValue)), GameState0))
         ); GameState0 = GameState)) ; GameState0 = GameState
         )
         );
         GameState0 = GameState
    ),


    % Create the new board with the piece moved
    replace(GameState0, FromRow, FromCol, PieceFrom, TempGameState),
    replace(TempGameState, ToRow, ToCol, PieceTo, NewGameState).


% replace(+Board, +Row, +Column, +Piece, -NewBoard)
% Replaces an element at a specified row and column with a new piece in the board.
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

% piece_value(+Piece, -Value)
% Retrieves the numeric value associated with a game piece.
piece_value(red(N), N).
piece_value(black(N), N).
piece_value(empty, 0).


%PATH

% find_possible_paths(+GameState, +FromRow, +FromCol, +ToRow, +ToCol, +Possible, -Paths, +Player) :-
% Predicate to find all possible paths from (FromRow, FromCol) to (ToRow, ToCol) within a given number of moves.
find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player) :-
    findall(Path, possible_path(GameState, FromRow, FromCol, ToRow, ToCol, Possible, [], Path, Player), Paths).

%valid_position(+X, +Y)
% Predicate to check if a position (X, Y) is valid and within the bounds of the game board.
valid_position(X, Y) :- X >= 1, X =< 7, Y >= 1, Y =< 5.

% next_position((+X, +Y), (-X1, -Y1))
% Predicate to calculate the next position in a certain direction.
next_position((X, Y), (X1, Y1)) :- X1 is X + 1, Y1 is Y.
next_position((X, Y), (X1, Y1)) :- X1 is X - 1, Y1 is Y.
next_position((X, Y), (X1, Y1)) :- X1 is X, Y1 is Y + 1.
next_position((X, Y), (X1, Y1)) :- X1 is X, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X-1, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X+1, Y1 is Y + 1.
next_position((X, Y), (X1, Y1)) :- X1 is X+1, Y1 is Y - 1.
next_position((X, Y), (X1, Y1)) :- X1 is X-1, Y1 is Y + 1.

% possible_path(+GameState, +X, +Y, +ToRow, +ToCol, +Possible, -CurrentPath, -Path, +Player)
% Recursive predicate to find possible paths.
possible_path(_, X, Y, X, Y, 0, Path, Path, Player):- !.
possible_path(_, X, Y, X, Y, 1, Path, Path, Player):- !.
possible_path(_, X, Y, X, Y, 2, Path, Path, Player):- !.
possible_path(GameState, X, Y, ToRow, ToCol, Possible, CurrentPath, Path, Player) :-
    Possible > 0,
    next_position((X, Y), (X1, Y1)), % Get the next position
    valid_position(X1, Y1), % Check if it's a valid position

    % Check for enemy pieces in the corresponding top/bottom left/right squares
    ((X1 =:= (X-1), Y1 =:= (Y - 1)) -> (check_diagonal(GameState, X, Y, -1, -1, Player))
    ; (X1 =:= (X+1), Y1 =:= (Y + 1)) -> (check_diagonal(GameState, X, Y, 1, 1, Player))
    ; (X1 =:= (X+1), Y1 =:= (Y - 1)) -> (check_diagonal(GameState, X, Y, 1, -1, Player))
    ; (X1 =:= (X-1), Y1 =:= (Y + 1)) -> (check_diagonal(GameState, X, Y, -1, 1, Player))
    ; true),

    nth1(X1, GameState, Row),
    (
        Possible > 1 -> 
        ((nth1(Y1, Row, empty)) ; (X1 = ToRow, Y1 = ToCol))
        ; nth1(Y1, Row, _) % if it's the last move it doesn't have to be empty (stacking terminates the move)
    ), % Check if it's an empty space
    NewPossible is Possible - 1,
    append(CurrentPath, [(X1, Y1)], NewPath),
    possible_path(GameState, X1, Y1, ToRow, ToCol, NewPossible, NewPath, Path, Player).

% retreat_positions(+Color, +ToRow, +ToCol, -RetreatPositions, +GameState, +NewValue)
% Finds retreat positions for a piece of the black Color and calculates the retreat positions
% based on its current position and value in the GameState.
retreat_positions(black, ToRow, ToCol, RetreatPositions, GameState, NewValue) :-
    (
        ((ToRow =\= 1) -> RetreatRow is ToRow - 1,RetreatCol is ToCol);
        (RetreatRow is ToRow, RetreatCol is 0)
    ),
    RetreatColLeft is ToCol - 1,
    RetreatColRight is ToCol + 1,
    findall(R, (
        (valid_position(RetreatRow, RetreatCol), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatCol, RetreatRowList, RetreatSpace),
         (RetreatSpace = red(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatCol] ; R = []
    ), RetreatPositions1),
    findall(R, (
        (valid_position(RetreatRow, RetreatColRight), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatColRight, RetreatRowList, RetreatSpace),
         (RetreatSpace = red(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatColRight] ; R = []
    ), RetreatPositions2),
    findall(R, (
        (valid_position(RetreatRow, RetreatColLeft), 
         nth1(RetreatRow, GameState, RetreatRowList),
         nth1(RetreatColLeft, RetreatRowList, RetreatSpace),
         (RetreatSpace = red(_) ; RetreatSpace = empty),
         piece_value(RetreatSpace, RetreatSpaceValue),
         (RetreatSpaceValue + NewValue) =< 4
        ) -> R = [RetreatRow, RetreatColLeft] ; R = []
    ), RetreatPositions3),
    append([RetreatPositions1, RetreatPositions2, RetreatPositions3], RetreatPositions).


% retreat_positions(+Color, +ToRow, +ToCol, -RetreatPositions, +GameState, +NewValue)
% Finds retreat positions for a piece of the red Color and calculates the retreat positions
% based on its current position and value in the GameState.
retreat_positions(red, ToRow, ToCol, RetreatPositions, GameState, NewValue) :-
    (
        ((ToRow =\= 7) -> RetreatRow is ToRow + 1,RetreatCol is ToCol);
        (RetreatRow is ToRow, RetreatCol is 0)
    ),
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


% write_retreat(+RetreatPositions)
% Writes the available retreat positions to the user.
write_retreat([]).

write_retreat([[Row, Col] | Rest]):-
    Code is Col + 96,
    char_code(Char, Code),
    write(Char),
    write(Row), write('  '),
    write_retreat(Rest), !.

% remove_empty_lists(+InputLists, -ResultLists)
% Removes empty lists from a list of lists.
remove_empty_lists([], []).
remove_empty_lists([[] | Rest], Result) :- 
    remove_empty_lists(Rest, Result).
remove_empty_lists([X | Rest], [X | Result]) :-
    X \= [],
    remove_empty_lists(Rest, Result).

% check_diagonal(+GameState, +X, +Y, +OffsetX, +OffsetY, +color)
% Checks if a black piece can make a valid diagonal move in the game.
check_diagonal(GameState, X, Y, OffsetX, OffsetY, black):-
    NewX is X+OffsetX,
    NewY is Y+OffsetY,
    nth1(NewX, GameState, RowList1),
    nth1(Y, RowList1, Piece1),
    nth1(X, GameState, RowList2),
    nth1(NewY, RowList2, Piece2),
    ((Piece1 = black(_) ; Piece1 = empty) ; (Piece2 = black(_) ; Piece2 = empty)).

% check_diagonal(+GameState, +X, +Y, +OffsetX, +OffsetY, +color)
% Checks if a red piece can make a valid diagonal move in the game.
check_diagonal(GameState, X, Y, OffsetX, OffsetY, red):-
    NewX is X+OffsetX,
    NewY is Y+OffsetY,
    nth1(NewX, GameState, RowList1),
    nth1(Y, RowList1, Piece1),
    nth1(X, GameState, RowList2),
    nth1(NewY, RowList2, Piece2),
    ((Piece1 = red(_) ; Piece1 = empty) ; (Piece2 = red(_) ; Piece2 = empty)).

% sum_red_pieces(+Board, -Sum)
% Calculate the total sum of red pieces values on the board.
sum_red_pieces(Board, Sum) :-
    sum_red_pieces(Board, 0, Sum).

% Base case: when the board is empty, the sum is 0.
sum_red_pieces([], Sum, Sum).

% Recursive case: count the red pieces in each row and accumulate the sum.
sum_red_pieces([Row | Rest], PartialSum, Sum) :-
    count_red_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + RowSum,
    sum_red_pieces(Rest, NewPartialSum, Sum).


% count_red_pieces_in_row(+Row, -RowSum)
% Count the number of red pieces in a given row. This predicate recursively traverses the row,
% accumulating the values of red pieces to calculate the total sum in RowSum.
count_red_pieces_in_row([], 0).
count_red_pieces_in_row([red(N) | Rest], RowSum) :-
    count_red_pieces_in_row(Rest, RestSum),
    RowSum is N + RestSum.
count_red_pieces_in_row([_ | Rest], RowSum) :-
    count_red_pieces_in_row(Rest, RowSum).

% sum_red_pieces_on_row7(+Board, -Sum)
% Predicate to calculate the sum of red pieces on row 7
sum_red_pieces_on_row7(Board, Sum) :-
    nth1(7, Board, Row7), % Get the 7th row
    count_red_pieces_in_row(Row7, Sum).

% sum_black_pieces(+Board, -Sum)
% Calculate the total sum of black pieces values on the board.
sum_black_pieces(Board, Sum) :-
    sum_black_pieces(Board, 0, Sum).

% Base case: when the board is empty, the sum is 0.
sum_black_pieces([], Sum, Sum).

% Recursive case: count the red pieces in each row and accumulate the sum.
sum_black_pieces([Row | Rest], PartialSum, Sum) :-
    count_black_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + RowSum,
    sum_black_pieces(Rest, NewPartialSum, Sum).

% count_black_pieces_in_row(+Row, -RowSum)
% Count the number of black pieces in a given row. This predicate recursively traverses the row,
% accumulating the values of black pieces to calculate the total sum in RowSum.
count_black_pieces_in_row([], 0).
count_black_pieces_in_row([black(N) | Rest], RowSum) :-
    count_black_pieces_in_row(Rest, RestSum),
    RowSum is N + RestSum.
count_black_pieces_in_row([_ | Rest], RowSum) :-
    count_black_pieces_in_row(Rest, RowSum).

% Predicate to calculate the sum of red pieces on row 7
sum_black_pieces_on_row1(Board, Sum) :-
    nth1(1, Board, Row1), % Get the 1st row
    count_black_pieces_in_row(Row1, Sum).
