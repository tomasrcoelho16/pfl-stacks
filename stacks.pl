% Load the predicates from pieces.pl
:- use_module(library(lists)).
:- consult(pieces).

% Define the main predicate to start the game.
play :-
    display_menu,
    get_menu_option(Option),
    handle_menu_option(Option).

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
    initial_state(Board),
    display_board(Board),
    write('teste').


start_human_vs_bot_game :-
    write('Starting Human vs Bot game...\n').

start_bot_vs_bot_game :-
    write('Starting Bot vs Bot game...\n').

% Example of the main predicate to start the game.
% You can modify this to fit your game's entry point.
main :- play.








board:-   write('|1b2|2b2|3b2|4b2|5b2|'),nl,
          write('---------------------'), nl,
          write('|   |   |   |   |   |'),nl,
          write('---------------------'),nl,
          write('|   |   |   |   |   |'),nl,
          write('---------------------'),nl,
          write('|   |   |   |   |   |'),nl,
          write('---------------------'),nl,
          write('|   |   |   |   |   |'),nl,
          write('---------------------'),nl,
          write('|   |   |   |   |   |'),nl,
          write('---------------------'),nl,
          write('|1r2|2r2|3r2|4r2|5r2|').