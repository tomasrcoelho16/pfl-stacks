is_possible_combinateds(GameState, Player,Val,NewVal, FromRow, FromCol, ToRow, ToCol, Paths, GameStateCombinated):-
    find_adjacent_pieces(GameState,Player,ToRow,ToCol,AdjacentPieces),
    AdjacentPieces \= [],
    write('AdjacentPieces'), nl,
    write(AdjacentPieces), nl,
    save_coordinates(Paths, PenultimeCoordinates, FromRow, FromCol), nl,
    write('PenultimeCoordinates'), nl,
    remove_duplicates(PenultimeCoordinates, PenultimeCoordinatesFixed),
    write(PenultimeCoordinatesFixed), nl,
    find_possible_combinations(PenultimeCoordinatesFixed, AdjacentPieces, PossibleCombinations, FromRow, FromCol),
    write('Possible Combinations:'), nl,
    write(PossibleCombinations), nl,
    PossibleCombinations \= [],
    write('You can combine your attack! Do you want to? (yes/no)'), nl,

    %read user input, if yes, do the things after, if no, (; true)

    read(UserInput),
    (UserInput == yes ->

        write('Choose what pieces you want to combine your attack with: '),

        format_possible_combinations(PossibleCombinations, FriendlyOptions),
        remove_empty_and_duplicates(FriendlyOptions, FriendlyOptionsCleaned),
        write(FriendlyOptionsCleaned), nl,
        write('Enter the position you want to use: '),
        read(SelectedPosition),
        read_position(FriendlyOptionsCleaned, SelectedPosition, OtherPieces),
        write('You selected: '), write(SelectedPosition), nl,
        user_input_to_coordinates(SelectedPosition, (SelPosRow, SelPosCol)),
        nth1(SelPosRow, GameState, Row2),
        nth1(SelPosCol, Row2, PieceSelected),
        piece_value(PieceSelected, ValSelected),
        ValSelected + Val =< 4,
        Skrt is Val + ValSelected,
        max_list_length(OtherPieces, MaxLength),
        replace(GameState, SelPosRow, SelPosCol, empty, TempGameState),
        (MaxLength > 1 -> write('Want to combine another one? Select from these groups: '), write(OtherPieces), write(' - or "no".'),
            read(SelectedPosition2),
            (SelectedPosition2 \= no -> ((SelectedPosition2 \= SelectedPosition),
            read_position(OtherPieces, SelectedPosition2, OtherPieces2),
            write('You selected: '), write(SelectedPosition2), nl,
            user_input_to_coordinates(SelectedPosition2, (SelPosRow2, SelPosCol2)),
            nth1(SelPosRow2, GameState, Row22),
            nth1(SelPosCol2, Row22, PieceSelected2),
            piece_value(PieceSelected2, ValSelected2),
            ValSelected2 + Val + ValSelected =< 4,
            Skrt2 is (ValSelected2 + Val + ValSelected),
            write(Skrt2),nl,
            max_list_length(OtherPieces2, MaxLength2),
            replace(TempGameState, SelPosRow2, SelPosCol2, empty, NewTempGameState)) ; MaxLength2 = 0, true) 
            ; true
        ),
        (MaxLength2 > 2 -> write('Want to combine another one? Select from these groups: '), write(OtherPieces2), write(' - or "no".'),
            read(SelectedPosition3),
            (SelectedPosition3 \= no -> ((SelectedPosition3 \= SelectedPosition, SelectedPosition3 \= SelectedPosition2),
            read_position(OtherPieces2, SelectedPosition3, OtherPieces3),
            write('You selected: '), write(SelectedPosition3), nl,
            user_input_to_coordinates(SelectedPosition3, (SelPosRow3, SelPosCol3)),
            nth1(SelPosRow3, GameState, Row23),
            nth1(SelPosCol3, Row23, PieceSelected3),
            piece_value(PieceSelected3, ValSelected3),
            ValSelected2 + Val + ValSelected + ValSelected3 =< 4,
            Skrt3 is (ValSelected2 + Val + ValSelected + ValSelected3),
            write(Skrt3),nl,
            replace(NewTempGameState, SelPosRow3, SelPosCol3, empty, UfGameState)) ; true)
            ; true
        ),
        (
            (integer(Skrt3), NewVal = Skrt3, GameStateCombinated = UfGameState);
            (integer(Skrt2), NewVal = Skrt2 , GameStateCombinated = NewTempGameState);
            (NewVal = Skrt, GameStateCombinated = TempGameState)
        )
        % Separate pieces based on the list
        %separate_pieces_by_list(SelectedPosition, FriendlyOptionsCleaned, SameListPieces, DifferentListPieces),
        %write('Same List Pieces: '), write(SameListPieces), nl,
        %write('Different List Pieces: '), write(DifferentListPieces), nl

    ; % If the user enters 'no' or other input, the code execution can continue
        write(UserInput), true
    )
    .
    %here I want to present the user with the different options


% Define a predicate to find possible combinations
find_possible_combinations([], _, [], FromRow, FromCol).
find_possible_combinations([PenultimeCoord | RestPenultimeCoords], AdjacentPieces, [PenultimeCoord-CoordAdjacentPieces | RestPossibleCombinations], FromRow, FromCol) :-
    find_adj_pieces_for_coord(PenultimeCoord, AdjacentPieces, CoordAdjacentPieces, FromRow, FromCol),
    find_possible_combinations(RestPenultimeCoords, AdjacentPieces, RestPossibleCombinations, FromRow, FromCol).

find_adj_pieces_for_coord(_, [], [], FromRow, FromCol).
find_adj_pieces_for_coord(PenultimeCoord, [Element | RestAdjacentPieces], CoordAdjacentPieces, FromRow, FromCol) :-
    [Piece-Coord] = Element,
    (are_adjacent(PenultimeCoord, Coord), (Coord \= PenultimeCoord), (Coord \= (FromRow, FromCol)) ->
        CoordAdjacentPieces = [Piece-Coord | RestCoordAdjacentPieces]
    ; 
        CoordAdjacentPieces = RestCoordAdjacentPieces
    ),
    find_adj_pieces_for_coord(PenultimeCoord, RestAdjacentPieces, RestCoordAdjacentPieces, FromRow, FromCol).

    

% Predicate to save the last or penultimate coordinates of paths
save_coordinates([], [], _, _).
save_coordinates([Path|Rest], Coordinates, FromRow, FromCol) :-
    (length(Path, 1) ->  % If the length of the path is 1
        Coordinates = [(FromRow, FromCol)|RestCoordinates] % Save the original position
    ; % Otherwise, the length is not 1
        length(Path, Len),
        LenMinus2 is Len - 2,
        nth0(LenMinus2, Path, PenultimateCoord),  % Save the penultimate coordinate
        Coordinates = [PenultimateCoord|RestCoordinates]
    ),
    save_coordinates(Rest, RestCoordinates, FromRow, FromCol).

remove_duplicates([], []).
remove_duplicates([H | T], Result) :-
    (member(H, T) -> 
        remove_duplicates(T, Result)
    ; 
        Result = [H | RestResult],
        remove_duplicates(T, RestResult)
    ).

are_adjacent((X, Y), (X1, Y1)) :-
    abs(X - X1) =< 1,
    abs(Y - Y1) =< 1.

write_test([Element | RestAdjacentPieces]):-
    [Piece-Coord] = Element,
    write('Piece: '), write(Piece), nl,
    write('Coord: '), write(Coord), nl,
    write(RestAdjacentPieces).


coords_to_user_friendly([], []).
coords_to_user_friendly([Coord | RestCoords], [FriendlyOption | RestFriendlyOptions]) :-
    (Row, Col) = Coord,
    Code is Col + 96,
    Test is Row + 48,
    char_code(Char, Code),
    char_code(Yam, Test),
    atom_concat(Char, Yam, FriendlyOption),
    coords_to_user_friendly(RestCoords, RestFriendlyOptions).

format_possible_combinations([], []).
format_possible_combinations([Element | RestCombinations], [FriendlyOptions | RestFriendlyOptions]) :-
    Element = (_-Pieces),
    extract_coordinates(Pieces, ExtractedCoords),
    coords_to_user_friendly(ExtractedCoords, FriendlyOptions),
    format_possible_combinations(RestCombinations, RestFriendlyOptions).

extract_coordinates([], []).
extract_coordinates([_-Coord | RestPieces], [Coord | RestCoords]) :-
    extract_coordinates(RestPieces, RestCoords).

% Remove empty lists and duplicates from a list of lists
remove_empty_and_duplicates([], []).
remove_empty_and_duplicates([H|T], CleanedList) :-
    (is_list(H), H = [] -> remove_empty_and_duplicates(T, CleanedList)
    ; member(H, T) -> remove_empty_and_duplicates(T, CleanedList)
    ; CleanedList = [H|RestCleaned], remove_empty_and_duplicates(T, RestCleaned)
    ).

adjacent(X, Y, AdjacentX, AdjacentY) :-
    AdjacentX is X - 1, AdjacentY is Y - 1;  % Top-left
    AdjacentX is X - 1, AdjacentY is Y;      % Top
    AdjacentX is X - 1, AdjacentY is Y + 1;  % Top-right
    AdjacentX is X,     AdjacentY is Y - 1;  % Left
    AdjacentX is X,     AdjacentY is Y + 1;  % Right
    AdjacentX is X + 1, AdjacentY is Y - 1;  % Bottom-left
    AdjacentX is X + 1, AdjacentY is Y;      % Bottom
    AdjacentX is X + 1, AdjacentY is Y + 1.  % Bottom-right

% Find adjacent pieces to a given position (X, Y) on the game board.
find_adjacent_pieces(GameState, Player, X, Y, AdjacentPieces) :-
    findall([Piece-(AdjX,AdjY) ], (
        adjacent(X, Y, AdjX, AdjY),
        nth1(AdjX, GameState, Row),
        nth1(AdjY, Row, Piece),
        ((Player = black, Piece = black(_));(Player = red, Piece = red(_)))
    ), AdjacentPieces).

write_attack_combinateds(GameState, Val, []).
write_attack_combinateds(GameState,Val,[Option - [Fpiece-Coord | Rest1]| Rest] ) :-
    length(Rest1,Len),
    piece_value(Fpiece,Value),
    write(Fpiece),nl,
    write(Rest1), nl,
    write(Val),
    write(Value), nl,
    (
        ((Len > 0), piece_value(Fpiece2,Value2), write(Value2),nl); true
    ),
    write_attack_combinateds(GameState,Val, Rest).

% Initialize the accumulator as an empty list
read_position(FriendlyOptionsCleaned, SelectedPosition, OtherPieces) :-
    read_position(FriendlyOptionsCleaned, SelectedPosition, [], OtherPieces).

% Base case: If we've reached the end of the list, set OtherPieces to the accumulated pieces.
read_position([], _, Accumulator, OtherPieces) :-
    OtherPieces = Accumulator.

% Check the current list for SelectedPosition, if found, append it as a sublist to the accumulator.
read_position([FriendlyOptionsCleaned | RestFriendlyOptionsCleaned], SelectedPosition, Accumulator, OtherPieces) :-
    (member(SelectedPosition, FriendlyOptionsCleaned) ->
        append(Accumulator, [FriendlyOptionsCleaned], NewAccumulator)
    ;   % If not found, keep the accumulator as is.
        NewAccumulator = Accumulator
    ),
    % Continue with the next list.
    read_position(RestFriendlyOptionsCleaned, SelectedPosition, NewAccumulator, OtherPieces).




% Separate pieces into SameListPieces and DifferentListPieces
separate_pieces_by_list(SelectedPosition, FriendlyOptions, SameListPieces, DifferentListPieces) :-
    find_pieces_in_same_list(SelectedPosition, FriendlyOptions, SameListPieces),
    find_pieces_in_different_lists(SelectedPosition, FriendlyOptions, DifferentListPieces).

% Find pieces in the same list as SelectedPosition
find_pieces_in_same_list(_, [], []).
find_pieces_in_same_list(SelectedPosition, [List | Rest], SameListPieces) :-
    (member(SelectedPosition, List) ->
        SameListPieces = List
    ; find_pieces_in_same_list(SelectedPosition, Rest, SameListPieces)
    ).

% Find pieces in different lists from SelectedPosition
find_pieces_in_different_lists(_, [], []).
find_pieces_in_different_lists(SelectedPosition, [List | Rest], DifferentListPieces) :-
    (member(SelectedPosition, List) ->
        find_pieces_in_different_lists(SelectedPosition, Rest, DifferentListPieces)
    ; append(List, RestDifferentListPieces, DifferentListPieces),
      find_pieces_in_different_lists(SelectedPosition, Rest, RestDifferentListPieces)
    ).

max_list_length(Lists, MaxLength) :-
    max_list_length(Lists, 0, MaxLength).

max_list_length([], MaxLength, MaxLength).
max_list_length([L|Rest], CurrentMaxLength, MaxLength) :-
    length(L, Length),
    NewMaxLength is max(CurrentMaxLength, Length),
    max_list_length(Rest, NewMaxLength, MaxLength).