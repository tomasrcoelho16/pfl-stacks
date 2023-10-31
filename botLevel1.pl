choose_move(GameState,Player, Level, Move):-
    valid_moves(GameState, Moves, Player),
    choose_move(Level, GameState, Moves, Move).

choose_move(1, _GameState, Moves, Move):-
    write('somezing'),
    random_select(Move, Moves, _Rest),
    write(Move).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).

valid_moves(GameState, Moves, Player):-
    now(X),
    setrand(X),
    write(X),
    findall(Move,  (between(1, 5, FromCol), between(1, 7, FromRow),
             between(1, 5, ToCol), between(1, 7, ToRow),
             validate_move(GameState,Player, FromRow-FromCol, ToRow-ToCol, Move)
            ), Moves),
            \+length(Moves, 0), write('teste'), !.

validate_move(GameState,Player,FromRow-FromCol,ToRow-ToCol, Move) :-
    (FromRow \= ToRow ; FromCol \= ToCol),
    valid_position(FromRow, FromCol),
    valid_position(ToRow, ToCol),
    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),
    ((FromRow =\= 1, Player = black);
     (FromRow =\= 7, Player = red) -> true),
    (
        (Player = red, Piece = red(_));
        (Player = black, Piece = black(_))
    ),
    piece_value(Piece, Val),
    nth1(ToRow, GameState, Row1),
    nth1(ToCol, Row1, PieceTo),
    piece_value(PieceTo, PieceToVal),
    %write('   From: '),
    %write(FromRow), write('-'),
    %write(FromCol),
    %write('   To: '),
    %write(ToRow), write('-'),
    %write(ToCol), 
    %write(' Val:'), write(Val),
    %write(' Piece:'), write(Piece),
    (
        (Val =:= 1) -> (NPiecesMoving = 1) ;
        random(1, Val, NPiecesMoving)
    ),
    

    calculate_possible(NPiecesMoving,Possible),
    (abs(FromRow - ToRow) =< Possible, abs(FromCol - ToCol) =< Possible),
    (
        (Player = red, PieceTo = red(_), (NPiecesMoving + PieceToVal =< 4),
        NewValue is NPiecesMoving + PieceToVal,
        %NewValue2 is Val-NPiecesMoving,
        PieceTo2 = red(NewValue),
        (NPiecesMoving - Val =:= 0 -> PieceFrom = empty ; PieceFrom = red(Val-NPiecesMoving))
        );
        (Player = black, PieceTo = black(_), (NPiecesMoving + PieceToVal =< 4),
        NewValue is NPiecesMoving + PieceToVal,
        %NewValue2 is Val-NPiecesMoving,
        PieceTo2 = black(NewValue),
        (NPiecesMoving - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(Val-NPiecesMoving))
        );
        (Player = red, (PieceTo = black(_); PieceTo = empty), (NPiecesMoving > PieceToVal),
        %NewValue2 is Val-NPiecesMoving,
         PieceTo2 = red(NPiecesMoving),
        ((NPiecesMoving =:= Val -> PieceFrom = empty; PieceFrom = red(Val-NPiecesMoving)))
        );
        (Player = black, (PieceTo = red(_); PieceTo = empty), (NPiecesMoving > PieceToVal),
        %NewValue2 is Val-NPiecesMoving,
         PieceTo2 = black(NPiecesMoving),
        ((NPiecesMoving =:= Val -> PieceFrom = empty; PieceFrom = black(Val-NPiecesMoving)))
        )
    ),
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player),
    (Paths \= []),
    From = (FromRow,FromCol),
    To = (ToRow, ToCol),
    Move = (From,To, PieceFrom, PieceTo2),
    !.

move_bot(GameState, Move, NewGameState) :-
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
         (
            (RetreatPositionsFixed \= [] -> (
        random_member(Element, RetreatPositionsFixed),
        Element = (RetreatRow,RetreatCol),
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
        (
            (RetreatPositionsFixed \= [] -> (
         random_member(Element, RetreatPositionsFixed),
         Element = (RetreatRow,RetreatCol),
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



