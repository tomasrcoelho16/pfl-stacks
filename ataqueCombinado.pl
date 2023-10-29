is_possible_combinateds(GameState, Player, FromRow, FromCol, ToRow, ToCol, Paths):-
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
    write('You can combine your attack!'), nl,
    write('Choose what pieces you want to combine your attack with: '), nl,
    format_possible_combinations(PossibleCombinations, FriendlyOptions), nl,
    write('TESTE:'),
    write(FriendlyOptions),nl
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

format_possible_combinations([], RestFriendlyOptions).
format_possible_combinations([Element | RestCombinations], [FriendlyOptions | RestFriendlyOptions]):-
    Element = (Coord-Pieces),
    extract_coordinates(Pieces, ExtractedCoords, UpdatedExtractedCoords),
    write('Coords:'),nl,
    write(UpdatedExtractedCoords), nl,
    % Format the coordinates into user-friendly format
    coords_to_user_friendly(UpdatedExtractedCoords, FriendlyOptions),
    format_possible_combinations(RestCombinations, RestFriendlyOptions).

extract_coordinates([], ExtractedCoords, ExtractedCoords).
extract_coordinates([Piece-Coord | RestPieces], ExtractedCoords, UpdatedExtractedCoords) :-  
    append([Coord], ExtractedCoords, TempExtractedCoords),
    extract_coordinates(RestPieces, TempExtractedCoords, UpdatedExtractedCoords).