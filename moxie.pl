:-use_module(library(lists)).

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

%move(GameState, ['Drop', [Row, Col]], NewGameState) :-
    

choose_move(GameState, human, Move) :-
    nl.
choose_move(GameState, computer, Move) :-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).
choose_move(1, _GameState, Moves, Moves) :-
    random_select(Move, Moves, _Rest).


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
% --------------------------------------------------
%         U S E R     I N T E R F A C E
% --------------------------------------------------
printn(_,0).
printn(S,N):-
    write(S),
    N1 is N - 1,
    printn(S,N1).

printCode([]).
printCode([X | XS]):-
    put_code(X),
    printCode(XS).
print_text(Text,Symbol,Padding):-
    write(Symbol),
    printn(' ',Padding),
    printCode(Text),
    printn(' ',Padding),
    write(Symbol).

print_texts([],_,_,_).
print_texts([Text | T],Symbol,Size,Padding):-
        length(Text,TextSize),
        ExtraPadding = (Size - TextSize) / 2,
        TotPad = Padding + round(ExtraPadding),
        print_text(Text,Symbol,TotPad),
        nl,
        print_texts(T,Symbol,Size,Padding).

biggest([], []).
biggest([Biggest | T], Biggest) :- 
        length(Biggest, BiggestLen),
        biggest(T, SecondBiggest),
        length(SecondBiggest, SecondBiggestLen),
        SecondBiggestLen =< BiggestLen,
        !.

biggest([_ | T], Biggest) :- biggest(T, Biggest).


print_multi_banner(LoL,Symbol,Padding):-   % prints banners
    biggest(LoL,BigText),
    length(BigText,N),
    TotLen is N + 2 * Padding + 2,
    printn(Symbol,TotLen),
    nl,
    write(Symbol),
    TotPad is TotLen - 2,
    printn(' ',TotPad),
    write(Symbol),
    nl,
    print_texts(LoL,Symbol,N,Padding),
    write(Symbol),
    printn(' ',TotPad),
    write(Symbol),
    nl,
    printn(Symbol,TotLen).

intersperse([X],_,X).
intersperse([X,Y|XS],Sep,Res):-
    append(X, Sep,Res1),
    append(Res1,Y,Res2),
    intersperse([Res2 | XS],Sep,Res).

print_board([X | XS],1):-
    intersperse(X," -- ",Line),
    int_to_string(1,NumberPrint),
    append(NumberPrint,"   ",Coordinate),
    append(Coordinate,Line,Printable),
    print_text(Printable,'*',13),nl,
    print_text("",'*',23),nl,
    print_text("    A    B    C    D",'*',13),nl.

print_board([X | XS],BoardSize):-
    intersperse(X," -- ",Line),
    int_to_string(BoardSize,NumberPrint),
    append(NumberPrint,"   ",Coordinate),
    append(Coordinate,Line,Printable),
    print_text(Printable,'*',13),nl,
    print_text("    |    |    |    |",'*',13),nl,
    NewSize is BoardSize - 1,
    print_board(XS,NewSize).

int_to_string(Int,[Str]):- Str is Int + "0".

get_printable_info(GameState,[S8,S9]):-
    get_player(GameState,Player),
    get_available(GameState,Available),
    get_captured(GameState,Captured),
    nth0(0,Available,AvXi), int_to_string(AvXi,AvX),
    nth0(1,Available,AvOi), int_to_string(AvOi,AvO),
    nth0(0,Captured,CapXi), int_to_string(CapXi,CapX),
    nth0(1,Captured,CapOi), int_to_string(CapOi,CapO),
    append("X: ",AvX,S1),
    append(S1,"    O: ",S2),
    append(S2,AvO,S3),
    append(S3,"             X: ",S4),
    append(S4,CapX,S5),
    append(S5,"     O: ",S6),
    append(S6,CapO,S7),
    append(S7,"  ",S8),
    append("Player Turn:   ",Player,S9).

display_game(GameState):- 
    print_multi_banner([" M O X I E"],'*',18),nl,
    get_board(GameState,Board),
    length(Board,BoardSize),
    print_text("",'*',23),nl,
    print_board(Board,BoardSize),
    print_text("",'*',23),nl,
    % computing strings based on GameState
    get_printable_info(GameState,[A,B]),
    print_multi_banner(["Avaliable pieces:        Captured pieces: ",A,"",B],'*',2). 