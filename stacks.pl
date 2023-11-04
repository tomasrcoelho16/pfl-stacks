:- use_module(library(lists)).
:- use_module(library(between)).
:- consult(pieces).
:- consult(ataqueCombinado).
:- consult(bots).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).

% play
% The main predicate to start the game. It displays the game menu, gets the user's menu option,
% and handles the selected option to initiate gameplay.
play :-
    display_menu,
    get_menu_option(Option),
    handle_menu_option(Option).

% display_menu/0
% Display the game menu.
display_menu :-
    write('Welcome to Stacks!\n'),
    write('1. Human vs Human\n'),
    write('2. Human vs Bot\n'),
    write('3. Bot vs Bot\n'),
    write('Select an option (1/2/3): ').

% get_menu_option(+Option)
% Get the user's menu option.
get_menu_option(Option) :-
    read(Option).

% handle_menu_option(+Option)
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

% start_human_vs_human_game/0
% Define game modes as placeholders (you need to implement these).
start_human_vs_human_game :-
    play_game.

% start_human_vs_bot_game/0
% Entry point for starting a game where a human player plays against a bot.
start_human_vs_bot_game :-
    repeat,
    write('Difficulty to play against: '), nl,
    write('1 - Random Moves'), nl,
    write('2 - Greedy Moves'), nl,
    read(Input),
    (
        (Input == 1; Input == 2);
        write('Invalid input.'), nl, fail
    ),
    play_game_hvb(Input).

% start_bot_vs_bot_game/0
% Entry point for starting a game where two bots play against each other.
start_bot_vs_bot_game :-
    repeat,
    write('Difficulty: '), nl,
    write('1 - Random Moves'), nl,
    write('2 - Greedy Moves'), nl,
    read(Input),
    (
        (Input == 1; Input == 2);
        write('Invalid input.'), nl, fail
    ),
    play_game_bot(Input).

% play_game/0
% Entry point for starting a human vs. human game.
play_game:-
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState-black).   

% game_cycle/1
% Main game loop that controls the flow of the game.
game_cycle(GameState-Player):-
    game_over(GameState, Winner), !.

game_cycle(GameState-Player):-
    write('CURRENT PLAYER:'),
    write(Player), nl,
    choose_move(GameState, Player, Move, TwoMovesGamestate),
    move(TwoMovesGamestate, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState),
    !,
    game_cycle(NewGameState-NextPlayer).

% next_player/2
% Predicate to determine the next player in the game.
next_player(black, red).
next_player(red, black).

% game_over/2
% Predicate to check if the game is over and determine the winner.
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

% play_game_bot/1
% Initializes and starts a game between two bots with a specified level of play.
play_game_bot(Level):-
    initial_state(GameState),
    display_game(GameState),
    game_cycle_bot(GameState-black,Level).   

% game_cycle_bot/2
% Main game loop for a game between two bots.
game_cycle_bot(GameState-Player, _Level):-
    game_over(GameState, Winner), !.

game_cycle_bot(GameState-Player, Level):-
    write('CURRENT PLAYER:'),
    write(Player), nl,
    choose_move_bot(GameState, Player, Level, Move),
    move_bot(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(NewGameState),
    game_cycle_bot(NewGameState-NextPlayer, Level).

% play_game_hvb/1
% Initializes and starts a game between a human player and a bot with a specified level of play.
play_game_hvb(Level):-
    initial_state(GameState),
    display_game(GameState),
    game_cycle_hvb(GameState-black,Level).   

% game_cycle_hvb/2
% Main game loop for a game between a human player and a bot.
game_cycle_hvb(GameState-Player,Level):-
    game_over(GameState, Winner), !.

game_cycle_hvb(GameState-Player,Level):-
    write('CURRENT PLAYER:'),
    write(Player), nl,
    (
    (Player = black , choose_move(GameState, Player, Move, TwoMovesGamestate),
        move(TwoMovesGamestate, Move, NewGameState),
        next_player(Player, NextPlayer),
        write('next:'), write(NextPlayer), nl
    );
    (Player = red , write('TOU AQUI'), choose_move_bot(GameState, Player, 1, Move),
        move_bot(GameState, Move, NewGameState),
        next_player(Player, NextPlayer),
        write('next:'), write(NextPlayer), nl
    )
    ),
    display_game(NewGameState),
    !,
    game_cycle_hvb(NewGameState-NextPlayer,Level).


