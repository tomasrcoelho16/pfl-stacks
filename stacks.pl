% Load the predicates from pieces.pl
:- use_module(library(lists)).
:- consult(pieces).

% Define the main predicate to start the game.
play :-
    display_menu,
    get_menu_option(Option),
    handle_menu_option(Option).

% Predicate to calculate the sum of red pieces on the board
sum_red_pieces(Board, Sum) :-
    sum_red_pieces(Board, 0, Sum).

% Base case: when the board is empty, the sum is 0.
sum_red_pieces([], Sum, Sum).

% Recursive case: count the red pieces in each row and accumulate the sum.
sum_red_pieces([Row | Rest], PartialSum, Sum) :-
    count_red_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + RowSum,
    sum_red_pieces(Rest, NewPartialSum, Sum).

% Predicate to count the red pieces in a row
count_red_pieces_in_row([], 0).
count_red_pieces_in_row([red(N) | Rest], RowSum) :-
    count_red_pieces_in_row(Rest, RestSum),
    RowSum is N + RestSum.
count_red_pieces_in_row([_ | Rest], RowSum) :-
    count_red_pieces_in_row(Rest, RowSum).

% Predicate to calculate the sum of red pieces on row 7
sum_red_pieces_on_row7(Board, Sum) :-
    nth1(7, Board, Row7), % Get the 7th row
    count_red_pieces_in_row(Row7, Sum).

%FOR THE BLACKS!

% Predicate to calculate the sum of red pieces on the board
sum_black_pieces(Board, Sum) :-
    sum_black_pieces(Board, 0, Sum).

% Base case: when the board is empty, the sum is 0.
sum_black_pieces([], Sum, Sum).

% Recursive case: count the red pieces in each row and accumulate the sum.
sum_black_pieces([Row | Rest], PartialSum, Sum) :-
    count_black_pieces_in_row(Row, RowSum),
    NewPartialSum is PartialSum + RowSum,
    sum_black_pieces(Rest, NewPartialSum, Sum).

% Predicate to count the red pieces in a row
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


% Display the game menu.
display_menu :-
    write('Welcome to Stacks!\n'),
    write('1. Human vs Human\n'),
    write('2. Human vs Bot\n'),
    write('3. Bot vs Bot\n'),
    write('Select an option (1/2/3): ').

% Get the user's menu option.
get_menu_option(Option) :-
    read(Option).

% Handle the selected menu option.
handle_menu_option(1) :- % Human vs Human
    start_human_vs_human_game.
handle_menu_option(2) :- % Human vs Bot
    start_human_vs_bot_game.
handle_menu_option(3) :- % Bot vs Bot
    start_bot_vs_bot_game.
handle_menu_option(_) :- % Handle invalid input
    write('Invalid option. Please select a valid option.\n'),
    play.

% Define game modes as placeholders (you need to implement these).
start_human_vs_human_game :-
    write('yoo'),
    play_game.
    %initial_state(GameState),
    %display_board(GameState),
    %sum_red_pieces(GameState, SumRedTotal),
    %write('Total reds: '),
    %write(SumRedTotal).
    %sum_black_pieces_first_line(Board, Sum),
    %write(Sum),
    %write('teste'),


start_human_vs_bot_game :-
    initial_state(GameState),
    display_board(GameState),
    choose_move(GameState, human, Move),
    move(GameState, Move, NewGameState),
    display_board(NewGameState),
    write('Starting Human vs Bot game...\n').

start_bot_vs_bot_game :-
    write('Starting Bot vs Bot game...\n').

% Example of the main predicate to start the game.
% You can modify this to fit your game's entry point.
main :- play.

play_game:-
    initial_state(GameState),
    display_board(GameState),
    game_cycle(GameState-black).   

game_cycle(GameState-Player):-
    game_over(GameState, Winner), !.

:- dynamic to_play/1.
to_play(true).

game_cycle(GameState-Player):-
    write('CURRENT PLAYER:'),
    write(Player), nl,
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_board(NewGameState),
    !,
    game_cycle(NewGameState-NextPlayer).

next_player(black, red).
next_player(red, black).

game_over(GameState, Winner) :-
    sum_red_pieces(GameState, SumRedTotal), !,
    sum_black_pieces(GameState, SumBlackTotal), !,
    sum_red_pieces_on_row7(GameState, SumRed7), !,
    sum_black_pieces_on_row1(GameState, SumBlack1), !,

    (   (SumRedTotal < 5, Winner = 'Black') ->
        write('Black Wins!'), nl
    ;   (SumBlackTotal < 5, Winner = 'Red') ->
        write('Red Wins!'), nl
    ;   (SumRed7 > 3, Winner = 'Red') ->
        write('Red Wins!'), nl
    ;   (SumBlack1 > 3, Winner = 'Black') ->
        write('Black Wins!'), nl
    ).




