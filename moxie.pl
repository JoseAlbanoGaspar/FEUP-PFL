:-use_module(library(lists)).
:-use_module(library(random)).

initial_state(
    [
        "X",
        [
            [" ", " ", " ", " "],
            [" ", " ", " ", " "],
            [" ", " ", " ", " "],
            [" ", " ", " ", " "]
        ],
        [8, 8],
        [0, 0]
    ]
).

list_shift_rotate(List,N,Res):-
    append(ToRotate,ToKeep,List),
    append(ToKeep,ToRotate,Res),
    length(ToRotate,N).

rotate_matrix([],_,[]).
rotate_matrix([L | T],N,[Rotated | Res]):-
    list_shift_rotate(L,N,Rotated),
    N1 is N - 1,
    rotate_matrix(T,N1,Res).

get_diags([],_,_,[]).
get_diags([Row | T], N,Size,[Bigger, Smaller | Rest]) :-
    append(Smaller, Bigger, Row),
    length(Bigger,N),
    SmallerLen is Size - N,
    length(Smaller,SmallerLen),
    N1 is N - 1,
    get_diags(T, N1, Size,Rest).
    
check_one_side_diags(Player, Board) :-
    length(Board, Len),
    Size is Len - 1,
    rotate_matrix(Board, Size, TmpBoard),
    transpose(TmpBoard, NewBoard),
    get_diags(NewBoard, Len,Len,NewerBoard),
    check_rows(Player,NewerBoard).

check_diags(Player, Board) :-
    check_one_side_diags(Player, Board), !.
check_diags(Player, Board) :-
    reverse(Board, NewBoard),
    check_one_side_diags(Player, NewBoard).

check_row(Player, Row, Start) :-
    nth0(Start, Row, Sq),
    Sq == Player,
    Start1 is Start + 1,
    nth0(Start1, Row, Sq),
    Sq == Player,
    Start2 is Start1 + 1,
    nth0(Start2, Row, Sq),
    Sq == Player, !.
check_row(Player, Row, PrevStart) :-
    Start is PrevStart + 1,
    length(Row,Iterationstmp),
    Iterations is Iterationstmp - 3,
    Start =< Iterations,
    check_row(Player, Row, Start).

check_rows(Player, [Row | T]) :-
    \+ check_row(Player, Row, 0),
    check_rows(Player, T).
check_rows(Player, [Row | _]) :-
    check_row(Player, Row, 0).

check_cols(Player, Board) :-
    transpose(Board, NewBoard),
    check_rows(Player, NewBoard).  

check_board(Player, Board) :-
    check_cols(Player, Board).
check_board(Player, Board) :-
    check_rows(Player, Board).
check_board(Player, Board) :-
    check_diags(Player, Board).

game_over([_, _, _, [C, _]], "X") :- C >= 6.
game_over([_, _, _, [_, C]], "O") :- C >= 6.
game_over(GameState, "X") :-
    get_board(GameState, Board),
    check_board("X", Board).
game_over(GameState, "O") :-
    get_board(GameState, Board),
    check_board("O", Board).

get_player(GameState, Player) :-
    nth0(0, GameState, Player).

get_board(GameState, Board) :-
    nth0(1, GameState, Board).

get_available(GameState, Available) :-
    nth0(2, GameState, Available).
get_available(1, ["X", _, [Available, _], _], Available).
get_available(1, ["O", _, [_, Available], _], Available).
get_available(1, "X", [_, _, [Available, _], _], Available).
get_available(1, "O", [_, _, [_, Available], _], Available).

get_captured(GameState, Captured) :-
    nth0(3, GameState, Captured).
get_captured(1, ["X", _, _, [Captured, _]], Captured).
get_captured(1, ["O", _, _, [_, Captured]], Captured).
get_captured(1, "X", [_, _, _, [Captured, _]], Captured).
get_captured(1, "O", [_, _, _, [_, Captured]], Captured).

get_square([_, Board, _, _], Row, Col, Square) :-
    nth0(Row, Board, RowList),  
    nth0(Col, RowList, Square).

change_player(["X",_,_,_],"O").
change_player(["O",_,_,_],"X").

update_available("X", Value, [_, E], [Value, E]).
update_available("O", Value, [E, _], [E, Value]).

replace_nth(N,I,V,O) :-
    nth0(N,I,_,T),
    nth0(N,O,V,T).

replace_row_col(M,Row,Col,Cell,N) :-
    nth0(Row,M,Old),
    replace_nth(Col,Old,Cell,Upd),
    replace_nth(Row,M,Upd,N).    

is_next_to(RowSrc, ColSrc, RowDest, ColSrc, [-1, 0]) :-
    RowSrc > 0,
    RowDest is RowSrc - 1.
is_next_to(RowSrc, ColSrc, RowSrc, ColDest, [0, -1]) :-
    ColSrc > 0,
    ColDest is ColSrc - 1.
is_next_to(RowSrc, ColSrc, RowDest, ColSrc, [1, 0]) :-
    RowSrc < 3,
    RowDest is RowSrc + 1.
is_next_to(RowSrc, ColSrc, RowSrc, ColDest, [0, 1]) :-
    ColSrc < 3,
    ColDest is ColSrc + 1.
is_next_to(RowSrc, ColSrc, RowDest, ColDest, [-1, -1]) :-
    is_next_to(RowSrc, ColSrc, RowDest, _, [-1, 0]),
    is_next_to(RowSrc, ColSrc, _, ColDest, [0, -1]).
is_next_to(RowSrc, ColSrc, RowDest, ColDest, [1, 1]) :-
    is_next_to(RowSrc, ColSrc, RowDest, _, [1, 0]),
    is_next_to(RowSrc, ColSrc, _, ColDest, [0, 1]).
is_next_to(RowSrc, ColSrc, RowDest, ColDest, [-1, 1]) :-
    is_next_to(RowSrc, ColSrc, RowDest, _, [-1, 0]),
    is_next_to(RowSrc, ColSrc, _, ColDest, [0, 1]).
is_next_to(RowSrc, ColSrc, RowDest, ColDest, [1, -1]) :-
    is_next_to(RowSrc, ColSrc, RowDest, _, [1, 0]),
    is_next_to(RowSrc, ColSrc, _, ColDest, [0, -1]).

inbounds(GameState, Row, Col) :-
    get_board(GameState, Board),
    replace_row_col(Board,Row,Col,_,_).

format_coordinates(InRow, InCol, OutRow, OutCol) :-
    number_chars(RowIntTmp, [InRow]),
    OutRow is 4 - RowIntTmp,
    letter_to_number(InCol, OutCol).

unformat_coordinates([InRow, InCol], Str) :-
    InColAscii is InCol + 97,
    InRow1 is 4 - InRow,
    number_codes(InRow1, InRowStr),
    append([InColAscii], InRowStr, Str).

is_empty_square(GameState, Row, Col) :-
    get_square(GameState, Row, Col, Square),
    Square == " ".

is_piece(Piece, GameState, Row, Col) :-
    get_square(GameState, Row, Col, Square),
    Square == Piece.

make_jump(GameState, RowPiece, ColPiece, [Player, NewBoard, NewAvailable, NewCaptured], RowDest, ColDest) :-
    get_available(GameState, NewAvailable),
    get_board(GameState, Board),
    get_player(GameState, Player),
    is_piece(Player, GameState, RowPiece, ColPiece),
    is_next_to(RowPiece, ColPiece, RowEnemy, ColEnemy, Dir),
    change_player(GameState, EnemyPlayer),
    is_piece(EnemyPlayer, GameState, RowEnemy, ColEnemy),
    is_next_to(RowEnemy, ColEnemy, RowDest, ColDest, Dir),
    is_empty_square(GameState, RowDest, ColDest),
    replace_row_col(Board, RowPiece, ColPiece, " ", TmpBoard),          % executing move  
    replace_row_col(TmpBoard, RowEnemy, ColEnemy, " ", TmpBoard2),
    replace_row_col(TmpBoard2, RowDest, ColDest, Player, NewBoard),
    get_captured(GameState, Captured),
    get_captured(1, GameState, CapturedValue),
    NewCapturedValue is CapturedValue + 1,
    update_available(Player, NewCapturedValue, Captured, NewCaptured).
move(GameState, [jump, [RowPiece, ColPiece], [[RowPiece, ColPiece] | [[RowDest, ColDest]]]], [NewPlayer, Board, Available, Captured]) :-
    make_jump(GameState, RowPiece, ColPiece, [Player, Board, Available, Captured], RowDest, ColDest), 
    \+ make_jump([Player, Board, Available, Captured], RowDest, ColDest, _, _, _),
    change_player(GameState, NewPlayer).
move(GameState, [jump, [RowPiece, ColPiece], [[RowPiece, ColPiece] | T]], NewerGameState) :-
    make_jump(GameState,RowPiece, ColPiece, NewGameState, RowDest, ColDest),
    move(NewGameState, [jump, [RowDest, ColDest], T], NewerGameState).

move(GameState, [place, [Row, Col]], [NewPlayer, NewBoard, NewAvailable, Captured]) :-
    get_available(1, GameState, Available),
    get_available(GameState, AvailableList),
    get_captured(GameState, Captured),
    Available > 0,
    get_player(GameState, Player),
    Available1 is Available - 1,
    update_available(Player, Available1, AvailableList, NewAvailable),
    is_empty_square(GameState, Row, Col),
    get_board(GameState, Board),
    replace_row_col(Board, Row, Col, Player, NewBoard),
    change_player(GameState, NewPlayer).
move(GameState, [move, [RowSrc, ColSrc], [RowDest, ColDest]], [NewPlayer, NewBoard, Available, Captured]) :- 
    get_available(GameState, Available),
    get_captured(GameState, Captured),
    get_board(GameState, Board),
    get_player(GameState, Player),
    inbounds(GameState, RowSrc, ColSrc),
    is_next_to(RowSrc, ColSrc, RowDest, ColDest, _Dir),             % check if the squares are adjacent
    is_empty_square(GameState, RowDest, ColDest),                   % check if Destination is empty
    get_square(GameState, RowSrc, ColSrc, Square),                  % check if the players are moving their pieces
    Square == Player,
    replace_row_col(Board, RowSrc, ColSrc, " ", TmpBoard),          % executing move
    replace_row_col(TmpBoard, RowDest, ColDest, Player, NewBoard),
    change_player(GameState, NewPlayer).
    
validate_jump_opt(OptNum, Len) :-
    OptNum =< Len,
    OptNum >= 1.

ask_for_move(Jumps, Move) :-
    display_jumps(Jumps, 1),
    length(Jumps, Len),
    repeat,
    print_code("Choose a jump:"), nl,
    get_char(Opt), skip_line,
    number_chars(OptNum, [Opt]),
    validate_jump_opt(OptNum, Len),
    nth1(OptNum, Jumps, Move), !.
ask_for_move('P', [place, [Row, Col]]) :-
    repeat,
    print_code("Choose a square to place a piece (e.g. a3)"), nl,
    get_char(ColTmp),
    get_char(RowTmp), skip_line,
    format_coordinates(RowTmp, ColTmp, Row, Col).
ask_for_move('M', [move, [RowSrc, ColSrc], [RowDest, ColDest]]) :-
    repeat,
    print_code("Choose a piece to move (e.g. a3)"), nl,
    get_char(ColSrcTmp),
    get_char(RowSrcTmp), skip_line,
    print_code("Choose a square to move the piece to (e.g. c2)"), nl,
    get_char(ColDestTmp),
    get_char(RowDestTmp), skip_line,
    format_coordinates(RowSrcTmp, ColSrcTmp, RowSrc, ColSrc),
    format_coordinates(RowDestTmp, ColDestTmp, RowDest, ColDest).

valid_moves(GameState, Jumps) :-
    findall(Move, move(GameState, Move, _NewGameState), Moves),
    separated(Moves, is_jump, Jumps, _),
    length(Jumps, Len),
    Len > 0.
valid_moves(GameState, Moves) :-
    findall(Move, move(GameState, Move, _NewGameState), Moves).

separated([],_Pred,[],[]).

separated([H | T],Pred,Yes,[H | Nos]):-
    G =.. [Pred,H],
    \+ G, !, separated(T,Pred,Yes,Nos).

separated([H | T],Pred,[H | Yeses],No):-
    G =.. [Pred,H],
    G,
    separated(T,Pred,Yeses,No).

is_jump([jump, _, _]).

value(GameState, 10000) :-
    game_over(GameState, "X").
value(GameState, -10000) :-
    game_over(GameState, "O").
value(GameState, Value) :-
    get_available(1, "X", GameState, AvailableX),
    get_captured(1, "X", GameState, CapturedX),
    get_available(1, "O", GameState, AvailableO),
    get_captured(1, "O", GameState, CapturedO),
    InFieldX = 8 - AvailableX - CapturedO,
    InFieldO = 8 - AvailableO - CapturedX,
    Value is InFieldX - InFieldO + CapturedX - CapturedO.

choose_move(GameState, 'A', Move) :-
    get_player(GameState, Player),
    Player = "X",
    choose_move(GameState, human, Move).
choose_move(GameState, 'A', Move) :-
    choose_move(1, GameState, Move).
choose_move(GameState, 'B', Move) :-
    get_player(GameState, Player),
    Player = "X",
    choose_move(GameState, human, Move).
choose_move(GameState, 'B', Move) :-
    choose_move(2, GameState, Move).
choose_move(GameState, 'C', Move) :-
    choose_move(GameState, human, Move).
choose_move(GameState, human, Move) :-
    findall(Move, move(GameState, Move, _NewGameState), Moves),
    separated(Moves, is_jump, Jumps, _),
    length(Jumps, Len),
    Len > 0,
    ask_for_move(Jumps, Move), !.
choose_move(_, human, Move) :-
    repeat,
    print_code("Choose a type of move [(P)lace, (M)ove]"), nl,
    get_char(Type), skip_line,
    ask_for_move(Type, Move), !.
choose_move(1, GameState, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(2, GameState, Move) :-
    minimax(GameState, NewGameState, 5, _),
    move(GameState, Move, NewGameState).

game_cycle(GameState, _) :-
    game_over(GameState, Winner),
    congratulate_winner(Winner).
game_cycle(GameState, Mode) :-
    choose_move(GameState, Mode, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState, Mode).
game_cycle(GameState, Mode) :-
    print_code("Invalid move!"), nl,
    game_cycle(GameState, Mode).

play :-
    initial_state(GameState),
    get_game_mode(Mode),
    display_game(GameState),
    game_cycle(GameState, Mode).
% --------------------------------------------------
%         U S E R     I N T E R F A C E
% --------------------------------------------------
printn(_,0).
printn(S,N):-
    write(S),
    N1 is N - 1,
    printn(S,N1).

print_code([]).
print_code([X | XS]):-
    put_code(X),
    print_code(XS).
print_text(Text,Symbol,Padding):-
    write(Symbol),
    printn(' ',Padding),
    print_code(Text),
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

print_board([X | _],1):-
    intersperse(X," -- ",Line),
    int_to_string(1,NumberPrint),
    append(NumberPrint,"   ",Coordinate),
    append(Coordinate,Line,Printable),
    print_text(Printable,'*',13),nl,
    print_text("",'*',23),nl,
    print_text("    a    b    c    d",'*',13),nl.

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
letter_to_number(Str,Int):- 
    char_code(Str,A),
    Int is A - 97.

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
    print_multi_banner(["Avaliable pieces:        Captured pieces: ",A,"",B],'*',2),nl.

congratulate_winner(Winner):- 
    append("The winner is  ",Winner,Msg),
    print_multi_banner([Msg],'*',15).

verify_opt(Opt) :-
    Opt \= 'A', Opt \= 'B', Opt \= 'C', 
    print_code("Please enter a valid mode!"), nl.

get_game_mode(Opt) :-
    print_multi_banner([" A - 1P  vs  CPU-Easy"," B - 1P  vs  CPU-Hard", "   C - 1P  vs  2P        "],'*',16),nl,
    repeat,
    print_code("Choose a game mode:"), nl,
    get_char(Opt), skip_line,
    \+ verify_opt(Opt), !.

print_jump([H]) :-
    unformat_coordinates(H, PrintableJump),
    print_code(PrintableJump).
print_jump([H | T]) :-
    unformat_coordinates(H, PrintableJump),
    print_code(PrintableJump),
    print_code(" -> "),
    print_jump(T).


display_jumps([],_).
display_jumps([Jump | T],N) :- 
    nth0(2,Jump,[Piece | Places]),
    unformat_coordinates(Piece, PrintablePiece),
    number_codes(N,PrintableNumber),
    print_code(PrintableNumber),
    print_code(" - "),
    print_code("Piece: "),
    print_code(PrintablePiece),
    print_code(" "),
    print_code("Jumps: "),
    print_code(PrintablePiece),
    print_code(" -> "),
    print_jump(Places),
    nl,
    N1 is N + 1,
    display_jumps(T,N1).

% --------------------------------------------------
%    A R T I F I C I A L  I N T E L I G E N C E
% --------------------------------------------------



minimax(GameState, NewGameState, Depth, Val) :-
    Depth > 0,
    Depth1 is Depth - 1,
    bagof(GameStateTmp, move(GameState, _, GameStateTmp), NewGameStateList),
    best(NewGameStateList, NewGameState, Depth1, Val), !.
minimax(GameState, _, _, Val) :-        
    value(GameState, Val).

best([GameState], GameState, Depth, Val) :-
    minimax(GameState, _, Depth, Val), !.
best([GameState1 | GameStateList], NewGameState, Depth, BestVal) :-
    minimax(GameState1, _, Depth, Val1),
    best(GameStateList, GameState2, Depth, Val2),
    betterOf(GameState1, Val1, GameState2, Val2, NewGameState, BestVal).

betterOf(GameState0, Val0, _, Val1, GameState0, Val0) :-   % Pos0 better than Pos1
    min_to_move(GameState0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(GameState0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value
betterOf(_, _, GameState1, Val1, GameState1, Val1).        % Otherwise Pos1 better than Pos0

min_to_move(["O", _, _, _]).
max_to_move(["X", _, _, _]).