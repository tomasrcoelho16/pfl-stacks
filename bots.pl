% choose_move_bot(+GameState, +Player, +Level, -Move)
% Chooses a move for a bot player based on the given level of intelligence (Level).
choose_move_bot(GameState, Player, Level, Move):-
    valid_moves(GameState, Moves, Player),
    choose_move_bot(Level, Player, GameState, Moves, Move).

% choose_move_bot(+Level, +Player, +GameState, +Moves, -Move)
% Chooses a move for a bot level 1, meaning it does it by random selection.
choose_move_bot(1,_Player, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).

% choose_move_bot(+Level, +Player, +GameState, +Moves, -Move)
% Chooses a move for a bot level 2, meaning it evaluates the moves and selects the best one.
choose_move_bot(2,Player, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move_bot(GameState, Mv, NewState),
    evaluate_board(GameState, NewState, Player, Value)), [_V-Move|_]).

% valid_moves(+GameState, -Moves, +Player)
% Finds all valid moves for the given player in the current game state.
valid_moves(GameState, Moves, Player):-
    now(X),
    setrand(X),
    findall(Move,  (between(1, 5, FromCol), between(1, 7, FromRow),
             between(1, 5, ToCol), between(1, 7, ToRow), between(1, 4, NPiecesMoving),
             validate_move(GameState,Player, FromRow-FromCol, ToRow-ToCol,NPiecesMoving, Move)
            ), Moves),
            \+length(Moves, 0), !.

% validate_move(+GameState, +Player, +FromRow-FromCol, +ToRow-ToCol, +NPiecesMoving, -Move)
% Validates a potential move based on the game rules and player's choice.
validate_move(GameState,Player,FromRow-FromCol,ToRow-ToCol,NPiecesMoving, Move) :-
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
    (NPiecesMoving =< Val),
    nth1(ToRow, GameState, Row1),
    nth1(ToCol, Row1, PieceTo),
    piece_value(PieceTo, PieceToVal),
    calculate_possible(NPiecesMoving,Possible),
    (abs(FromRow - ToRow) =< Possible, abs(FromCol - ToCol) =< Possible),
    (
        (Player = red, PieceTo = red(_), (NPiecesMoving + PieceToVal =< 4),
        NewValue is NPiecesMoving + PieceToVal,
        NewValue2 is Val-NPiecesMoving,
        PieceTo2 = red(NewValue),
        (NPiecesMoving - Val =:= 0 -> PieceFrom = empty ; PieceFrom = red(NewValue2))
        );
        (Player = black, PieceTo = black(_), (NPiecesMoving + PieceToVal =< 4),
        NewValue is NPiecesMoving + PieceToVal,
        NewValue2 is Val-NPiecesMoving,
        PieceTo2 = black(NewValue),
        (NPiecesMoving - Val =:= 0 -> PieceFrom = empty ; PieceFrom = black(NewValue2))
        );
        (Player = red, (PieceTo = black(_); PieceTo = empty), (NPiecesMoving > PieceToVal),
        NewValue2 is Val-NPiecesMoving,
         PieceTo2 = red(NPiecesMoving),
        ((NPiecesMoving =:= Val -> PieceFrom = empty; PieceFrom = red(NewValue2)))
        );
        (Player = black, (PieceTo = red(_); PieceTo = empty), (NPiecesMoving > PieceToVal),
        NewValue2 is Val-NPiecesMoving,
         PieceTo2 = black(NPiecesMoving),
        ((NPiecesMoving =:= Val -> PieceFrom = empty; PieceFrom = black(NewValue2)))
        )
    ),
    find_possible_paths(GameState, FromRow, FromCol, ToRow, ToCol, Possible, Paths, Player),
    (Paths \= []),
    From = (FromRow,FromCol),
    To = (ToRow, ToCol),
    Move = (From,To, PieceFrom, PieceTo2),
    !.

% move_bot(+GameState, +Move, -NewGameState)
% Applies a move generated by the bot to the game state.
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


% evaluate_board(+GameState, +NewState, +Player, -Value)
% Evaluates the game board and assigns a value to the board position.
evaluate_board(GameState, NewState, Player, Value):-
    sum_red_pieces_on_row7(NewState, SumRed7), !,
    sum_black_pieces_on_row1(NewState, SumBlack1), !,
    sum_red_pieces(NewState, SumRedTotal), !,  
    sum_red_pieces(GameState, SumRed), !,  
    sum_black_pieces(GameState, SumBlack), !,
    sum_black_pieces(NewState, SumBlackTotal), !,
    (
        (Player = black, SumRedTotal < 5, PointsVic is -200
        );
        (Player = black, SumBlack1 >= 4, PointsVic is -200 
        );
        (Player = red, SumRed7 >= 4, PointsVic is -200 
        );
        (Player = red, SumBlackTotal < 5, PointsVic is -200
        );
        PointsVic is 0
    ),
    (
        (Player = black, (SumRedTotal =\= SumRed), Extra is -50, write('yayyyyyyyyyyyyyyyyyyyy'));
        (Player = red, (SumBlackTotal =\= SumBlack), Extra is -50, write('yooooooooooooooooooooo'));
        Extra is 0
    ),
    (
        (Player = black, sum_black_pieces_bot(NewState, Val))
        ; (Player = red, sum_red_pieces_bot(NewState, Val));
        Val is 0
    ),
    Value is -Val + PointsVic + Extra.


% sum_black_pieces_bot(+Board, -Sum)
% Calculate the total sum of black pieces values on the board.
sum_black_pieces_bot(Board, Sum) :-
    sum_black_pieces_bot(Board, 0,_Count, Sum).

sum_black_pieces_bot([], Sum,_Count, Sum).

sum_black_pieces_bot([Row | Rest], PartialSum, Count, Sum) :-
    (
        (integer(Count) -> NewCount is Count - 1);
        NewCount is 7
    ),
    count_black_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + (RowSum*NewCount),
    sum_black_pieces_bot(Rest, NewPartialSum,NewCount, Sum).

% sum_red_pieces_bot(+Board, -Sum)
% Calculate the total sum of red pieces values on the board.
sum_red_pieces_bot(Board, Sum) :-
    sum_red_pieces_bot(Board, 0,_Count, Sum).

sum_red_pieces_bot([], Sum,_Count, Sum).

sum_red_pieces_bot([Row | Rest], PartialSum, Count, Sum) :-
    (
        (integer(Count) -> NewCount is Count + 1);
        NewCount is 1
    ),
    count_red_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + (RowSum*NewCount),
    sum_red_pieces_bot(Rest, NewPartialSum,NewCount, Sum).
