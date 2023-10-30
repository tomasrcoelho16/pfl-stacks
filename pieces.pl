:- use_module(library(lists)).

% Define a piece as a compound term with color (red or black) and stack size.
% Example: red(2) represents a stack of 2 red pieces.
% empty represents an empty cell.

initial_state([
    [red(3), red(1), red(2), empty, red(2)],
    [black(2), empty, black(3), empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, red(1), red(1), red(1)],
    [empty, red(1), empty, red(2), empty],
    [red(3), empty, empty, black(3), empty],
    [black(2), red(1), black(1), empty, black(2)]
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
choose_move(GameState, Player, Move, TwoMovesGamestate) :-
    repeat,
    write('Choose one of the turn possibilities:') ,nl,
    write('1 - Move a single stack.') ,nl,
    write('2 - Move two individual pieces up to 2 spaces each.'),nl,
    read(Input),
    (
        (Input =:= 1, single_move(GameState,Player,Move, TwoMovesGamestate));
        (Input =:= 2, double_move(GameState,Player,Move, TwoMovesGamestate));
        write('Thats not a valid option.'), nl, fail
    )
    .

single_move(GameState,Player, Move, TwoMovesGamestate) :-
    repeat,
    write('Select a piece or stack (e.g., a1): '),
    read(FromInput),

    user_input_to_coordinates(FromInput, (FromRow, FromCol)), 

    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),

    ( (Player = black, Piece = black(_)) ;
      (Player = red, Piece = red(_)) ),

    piece_value(Piece, Val),

    (((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true);
     (write('That is not allowed.'), nl, fail)          % NAO FAZ SENTIDO SEU FILHO DA PUTA
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    % ATAUQE COMBINADO GOES HERE

    user_input_to_coordinates(ToInput, (ToRow, ToCol)),

    (((Val > 1) -> write('How many pieces do you want (e.g., 1, 2, 3, 4): '), read(NPiecesInput)); NPiecesInput = 1),

    (NPiecesInput =< Val),

    calculate_possible(NPiecesInput, Possible),
    (abs(FromRow - ToRow) =< Possible, abs(FromCol - ToCol) =< Possible),

    nth1(ToRow, GameState, RowEnemy),
    nth1(ToCol, RowEnemy, PieceEnemy),
    piece_value(PieceEnemy, ValEnemy),

    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player),
    (Paths \= []),
    find_adjacent_pieces(GameState,Player,ToRow,ToCol,AdjacentPieces),
    (((Player = black, PieceEnemy = red(_), AdjacentPieces \= [[Piece-(FromRow,FromCol)]], AdjacentPieces \= []) ; (Player = red, PieceEnemy = black(_), AdjacentPieces \= [[Piece-(FromRow,FromCol)]], AdjacentPieces \= [])) -> is_possible_combinateds(GameState, Player,NPiecesInput, NewVal,AdjacentPieces, FromRow, FromCol, ToRow, ToCol, Paths, GameStateCombinated)
     ;
    true ),
    (
        (integer(NewVal), Yoo is NewVal, TwoMovesGamestate = GameStateCombinated);
        (Yoo is NPiecesInput, TwoMovesGamestate = GameState)
    ),
    ( (Player = black, PieceEnemy = black(_), (Yoo+ValEnemy) =< 4,    
        NewValue is Yoo + ValEnemy,
        PieceTo = black(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-NPiecesInput))
        ) ;
      (Player = red, PieceEnemy = red(_), (Yoo+ValEnemy) =< 4,
        NewValue is Yoo + ValEnemy,
        PieceTo = red(NewValue),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = red(Val-NPiecesInput))
        ) ;
      (Player = red, (PieceEnemy = black(_); PieceEnemy = empty), (Yoo > ValEnemy),
         NewValue is Yoo,
         PieceTo = red(Yoo),
        ((NPiecesInput =:= Val -> PieceFrom = empty; PieceFrom = red(Val-NPiecesInput)))
         ) ;
      (Player = black, (PieceEnemy = red(_); PieceEnemy = empty), (Yoo > ValEnemy),
        NewValue is Yoo,
        PieceTo = black(Yoo),
        (NPiecesInput - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-NPiecesInput))
        )
    ),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),
    Move = (From, To, PieceFrom, PieceTo)
    .
double_move(GameState, Player, Move2, TwoMovesGamestate) :-
    repeat,
    write('Select the first piece (e.g., a1): '),
    read(FromInput),

    user_input_to_coordinates(FromInput, (FromRow, FromCol)), 

    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),

    ( (Player = black, Piece = black(_)) ;
      (Player = red, Piece = red(_)) ),

    piece_value(Piece, Val),

    (((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true);
     (write('That is not allowed.'), nl, fail)          % NAO FAZ SENTIDO SEU FILHO DA PUTA
    ),

    write('Select a destination (e.g., b2): '),
    read(ToInput),      % Read the coordinate for the destination

    % ATAUQE COMBINADO GOES HERE

    user_input_to_coordinates(ToInput, (ToRow, ToCol)),

    (abs(FromRow - ToRow) =< 2, abs(FromCol - ToCol) =< 2),

    nth1(ToRow, GameState, RowEnemy),
    nth1(ToCol, RowEnemy, PieceEnemy),
    piece_value(PieceEnemy, ValEnemy),

    (
        (Player = black, PieceEnemy = black(_), (1+ValEnemy) =< 4,    
        NewValue is 1 + ValEnemy,
        PieceTo = black(NewValue),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-1))
        ) ;
        (Player = red, PieceEnemy = red(_), (1+ValEnemy) =< 4,
        NewValue is 1 + ValEnemy,
        PieceTo = red(NewValue),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = red(Val-1))
        ) ;
        (Player = red, PieceEnemy = empty, (1 > ValEnemy),
         NewValue is 1,
         PieceTo = red(1),
        ((1 =:= Val -> PieceFrom = empty; PieceFrom = red(Val-1)))
         ) ;
      (Player = black, PieceEnemy = empty, (1 > ValEnemy),
        NewValue is 1,
        PieceTo = black(1),
        ((1 - Val) =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-1))
        )
    ),
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, 2, Paths, Player),
    (Paths \= []),

    From = (FromRow, FromCol),
    To = (ToRow, ToCol),
    Move = (From, To, PieceFrom, PieceTo),
    move(GameState, Move, TempGameState),


    display_board(TempGameState),
    repeat,
    write('CURRENT PLAYER: '),
    write(Player),nl,
    write('Select the second piece, you cannot move the same piece again(e.g., a1): '),
    read(FromInput2),

    user_input_to_coordinates(FromInput2, (FromRow2, FromCol2)), 
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

    user_input_to_coordinates(ToInput2, (ToRow2, ToCol2)),
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
        PieceTo2 = black(NewValue2),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = black(Val2-1))
        ) ;
        (Player = red, PieceEnemy2 = red(_), (1+ValEnemy2) =< 4,
        NewValue2 is 1 + ValEnemy2,
        PieceTo2 = red(NewValue2),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = red(Val2-1))
        ) ;
        (Player = red, (PieceEnemy2 = empty; PieceEnemy2 = black(_)), (Yoo > ValEnemy2),
         NewValue2 is Yoo,
         PieceTo2 = red(Yoo),
        ((1 =:= Val2 -> PieceFrom2 = empty; PieceFrom2 = red(Val2-1)))
         ) ;
      (Player = black, (PieceEnemy2 = empty; PieceEnemy2 = red(_)), (Yoo > ValEnemy2),
        NewValue2 is Yoo,
        PieceTo2 = black(Yoo),
        ((1 - Val2) =:= 0 -> PieceFrom2 = empty ; PieceFrom2 = black(Val2-1))
        )
    ),

    From2 = (FromRow2, FromCol2),
    To2 = (ToRow2, ToCol2),
    Move2 = (From2, To2, PieceFrom2, PieceTo2)
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

    retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
    remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
    (
        ((PieceTo = red(_),
         EnemyPiece = black(_), NewValue > 0) ->
         write('bbbbb'),
         retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
         remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
         repeat,
         write('RetreatPositionsFixed'), nl,
         write(RetreatPositionsFixed), nl,
         (
            (RetreatPositionsFixed \= [] -> (write('Choose a position to retreat the piece to: (  '),
         write_retreat(RetreatPositionsFixed),
         write(')'), nl,
         read(RetreatInput),
         user_input_to_coordinates(RetreatInput, (RetreatRow, RetreatCol)),
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
         write('aaaaa'),
         %retreat_positions(Player, ToRow, ToCol, RetreatPositions, GameState, NewValue),
         %remove_empty_lists(RetreatPositions, RetreatPositionsFixed), nl,
         repeat,
         write('RetreatPositionsFixed'), nl,
         write(RetreatPositionsFixed), nl,
        (
            (RetreatPositionsFixed \= [] -> (write('Choose a position to retreat the piece to: (  '),
         write_retreat(RetreatPositionsFixed),
         write(')'), nl,
         read(RetreatInput),
         user_input_to_coordinates(RetreatInput, (RetreatRow, RetreatCol)),
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
find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player) :-
    findall(Path, possible_path(GameState, FromRow, FromCol, ToRow, ToCol, Possible, [], Path, Player), Paths).

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
possible_path(_, X, Y, X, Y, 0, Path, Path, Player):- !.
possible_path(_, X, Y, X, Y, 1, Path, Path, Player):- !.
possible_path(_, X, Y, X, Y, 2, Path, Path, Player):- !.
possible_path(GameState, X, Y, ToRow, ToCol, Possible, CurrentPath, Path, Player) :-
    Possible > 0,
    %repeat,
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
        ; nth1(Y1, Row, _) % se for o ultimo move o espaço nao tem que ser empty
    ), % Check if it's an empty space
    NewPossible is Possible - 1,
    append(CurrentPath, [(X1, Y1)], NewPath),
    possible_path(GameState, X1, Y1, ToRow, ToCol, NewPossible, NewPath, Path, Player).

% Retreat positions for the black player
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


% Retreat positions for the black player
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

write_retreat([]).

write_retreat([[Row, Col] | Rest]):-
    Code is Col + 96,
    char_code(Char, Code),
    write(Char),
    write(Row), write('  '),
    write_retreat(Rest), !.


remove_empty_lists([], []).
remove_empty_lists([[] | Rest], Result) :- 
    remove_empty_lists(Rest, Result).
remove_empty_lists([X | Rest], [X | Result]) :-
    X \= [],
    remove_empty_lists(Rest, Result).


check_diagonal(GameState, X, Y, OffsetX, OffsetY, black):-
    NewX is X+OffsetX,
    NewY is Y+OffsetY,
    nth1(NewX, GameState, RowList1),
    nth1(Y, RowList1, Piece1),
    nth1(X, GameState, RowList2),
    nth1(NewY, RowList2, Piece2),
    ((Piece1 = black(_) ; Piece1 = empty) ; (Piece2 = black(_) ; Piece2 = empty)).

check_diagonal(GameState, X, Y, OffsetX, OffsetY, red):-
    NewX is X+OffsetX,
    NewY is Y+OffsetY,
    nth1(NewX, GameState, RowList1),
    nth1(Y, RowList1, Piece1),
    nth1(X, GameState, RowList2),
    nth1(NewY, RowList2, Piece2),
    ((Piece1 = red(_) ; Piece1 = empty) ; (Piece2 = red(_) ; Piece2 = empty)).

