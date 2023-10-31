choose_move(GameState,Player, Level, Move):-
    valid_moves(GameState, Moves, Player),
    choose_move(Level, GameState, Moves, Move).

choose_move(1, _GameState, Moves, Move):-
    write('somezing'),
    random_select(Move, Moves, _Rest).

choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).

valid_moves(GameState, Moves, Player):-
    findall(Move,  (between(1, 5, FromCol), between(1, 7, FromRow),
             between(1, 5, ToCol), between(1, 7, ToRow),
             validate_move(GameState,Player, FromRow-FromCol, ToRow-ToCol, Move)
            ), Moves),
            \+length(Moves, 0), write('teste'), !.

validate_move(GameState,Player,FromRow-FromCol,ToRow-ToCol, Move) :-
    valid_position(FromRow, FromCol),
    valid_position(ToRow, ToCol),
    nth1(FromRow, GameState, Row),
    nth1(FromCol, Row, Piece),
    (
        (Player = red, Piece = red(_));
        (Player = black, Piece = black(_))
    ),
    write(FromRow),
    write(FromCol), nl,
    piece_value(Piece, Val),
    nth1(ToRow, GameState, Row1),
    nth1(ToCol, Row1, PieceTo),
    piece_value(PieceTo, PieceToVal),
    From = (FromRow,FromCol),
    To = (ToRow, ToCol),
    Move = (From,To).





