initial_state(
    [
        'X',                        /* Player turn */
        [
            ['.', '.', '.', '.'],
            ['.', '.', '.', '.'],
            ['.', '.', '.', '.'],
            ['.', '.', '.', '.']
        ],                          /* Board representation */
        [8, 8],                     /* Available pieces */
        [0, 0]                      /* Captured pieces */
    ]
).

won('X', [_, _, _, [C, _]]) :- C >= 6.
won('0', [_, _, _, [_, C]]) :- C >= 6.
won('X', []).

get_player(GameState, Player) :-
    nth0(0, GameState, Player).

get_board(GameState, Board) :-
    nth0(1, GameState, Board).

get_available(GameState, Available) :-
    nth0(2, GameState, Available).

get_captured(GameState, Captured) :-
    nth0(3, GameState, Captured).

move(GameState, ['Drop', [Row, Col]], NewGameState) :-
    

choose_move(GameState, human, Move) :-
    nl.
choose_move(GameState, computer, Move) :-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).
choose_move(1, _GameState, Moves, Moves) :-
    random_select(Move, Moves, _Rest)

display_game(GameState) :- nl.

game_cycle(GameState) :-
    won(Winner, GameState), !, false.
game_cycle(GameState) :-
    choose_move(GameState, Player, Move),
    move(GameState, move, NewGameState),
    display_game(GameState), !,
    game_cycle(NewGameState).

play :-
    initial_state(GameState),
    display_game(GameState),
    game_cycle(GameState).
