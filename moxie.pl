:-use_module(library(lists)).
:-use_module(library(random)).

% initial_state(+Size, -GameState)
% returns the initial game state with a board Size x Size
initial_state(Size, ["X", Board, [8, 8], [0, 0]]) :-
    replicate(Size," ", Row),
    replicate(Size,Row,Board).

% --------------------------------------------------
%        U T I L I T Y   F U N C T I O N S 
% --------------------------------------------------
% replicate(+N, +Elem, -Res)
% replicates Elem N times in list Res
replicate(0, _ , []).
replicate(N, Elem, Res):- N1 is N-1, 
                          replicate(N1, Elem, Res1),
                          Res = [Elem|Res1].

% distance(+Row1, +Col1, +Row2, +Col2, ?D)
% computes the distance between point (Row1,Col1) and (Row2,Col2)
distance(Row1, Col1, Row2, Col2, D) :- 
    D is sqrt((Row2-Row1)^2 + (Col2-Col1)^2).

% intersperse(+List,+Sep,?Res)
% toggle an element with value Sep between each member of List
intersperse([X],_,X).
intersperse([X,Y|XS],Sep,Res):-
    append(X, Sep,Res1),
    append(Res1,Y,Res2),
    intersperse([Res2 | XS],Sep,Res).

% list_shift_rotate(+List, +N, ?Res)
% rotates a list by N elements
list_shift_rotate(List,N,Res):-
    append(ToRotate,ToKeep,List),
    append(ToKeep,ToRotate,Res),
    length(ToRotate,N).

% int_to_string(+Int,-Str)
% transforms an integer between 0 and 9 in a string with same value
int_to_string(Int,[Str]):- Str is Int + "0".

% letter_to_number(+Str,-Int)
% transform a string into it's position in the alphabeth  e.g: "a" -> 1
letter_to_number(Str,Int):- 
    char_code(Str,A),
    Int is A - 97.

% filter_coordinates(+GameState, +Player, +Coordinates, ?Filtered)
% filters the coordinates of the pieces of the player given in Player
filter_coordinates(_, _, [], []).
filter_coordinates(GameState, Player, [Row-Col | T], K) :-
    \+ is_piece(Player, GameState, Row, Col), !,
    filter_coordinates(GameState, Player, T, K).
filter_coordinates(GameState, Player, [Row-Col | T], [Row-Col | K]) :-
    filter_coordinates(GameState, Player, T, K).

% inbounds(+GameState, ?Row, ?Col)
% returns a valid Row Col based on Board dimention
inbounds(GameState, Row, Col) :-
    get_board(GameState, Board),
    replace_row_col(Board,Row,Col,_,_).

% is_next_to(+RowSrc, +ColSrc, +RowDest, +ColSrc, ?Dir)
% verifies if position (RowSrc,ColSrc) and (RowDest,ColDest) are adjacent and if yes returns vector direction
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

% separated(+List,+Pred,?Yeses,?No)
% filters the elements that make Pred true and return those in Yeses. The other ones are returned in No
separated([],_Pred,[],[]).

separated([H | T],Pred,Yes,[H | Nos]):-
    G =.. [Pred,H],
    \+ G, !, separated(T,Pred,Yes,Nos).

separated([H | T],Pred,[H | Yeses],No):-
    G =.. [Pred,H],
    G,
    separated(T,Pred,Yeses,No).

% is_jump(+Move)
% checks if Move is a jump
is_jump([jump, _, _]).

% is_numeber(+X,?Digit)
% checks if X is a char convertible to number and if it is returns the number in Digit
is_number(X,Digit) :-
    char_code('0',Zero),
    char_code('9',Nine),
    char_code(X,Digit),
    Digit >= Zero,
    Digit =< Nine.

% is_char(+X)
% checks if X is a lowercase letter
is_char(X) :- 
    char_code('a',A),
    char_code('z',Z),
    char_code(X,Char),
    Char >= A,
    Char =< Z.

%  check_boundaries(+X,?Min,?Max)
%  checks if a number X is in between Min and Max
check_boundaries(X,Min,Max) :-
    X >= Min ,
    X =< Max.
check_boundaries(_,_,_) :-
    print_code("Choose a valid option!"),nl,
    fail.

% read_number_acc(+Acc,?X)
% reads one char at a time and construct a number 
read_number_acc(X, X) :- peek_code(10), !.
read_number_acc(Acc, X) :- \+ peek_code(10),
                           get_code(Code),
                           char_code('0', Zero),
                           Digit is Code-Zero,
                           Digit >= 0,
                           Digit < 10,
                           NewAcc is Acc*10 + Digit,
                           read_number_acc(NewAcc, X).
% read_number(?X)
% tries to read a number from input
read_number(X) :- read_number_acc(0, X),!,
                  get_code(10).
read_number(_X):- skip_line,fail.

% read_until_between(+Min, +Max, ?X)
% asks for a number until it can read one that is between Min and Max
read_until_between(Min, Max, X):-
    repeat,
    print_code("Choose a jump:"), nl,
    read_number(X),
    check_boundaries(X, Min, Max),
    !.


% printn(+S, +N)
% prints S, N times
printn(_,0).
printn(S,N):-
    write(S),
    N1 is N - 1,
    printn(S,N1).

% print_code(+Str)
% prints the string on the screen
print_code([]).
print_code([X | XS]):-
    put_code(X),
    print_code(XS).

% print_text(+Text,+Symbol,+Padding)
% print Text with a padding of Padding chars and delimited by Symbol
print_text(Text,Symbol,Padding):-
    write(Symbol),
    printn(' ',Padding),
    print_code(Text),
    printn(' ',Padding),
    write(Symbol).

% print_texts(+Texts,+Symbol,+Size,+Padding)
% prints multiple texts
print_texts([],_,_,_).
print_texts([Text | T],Symbol,Size,Padding):-
        length(Text,TextSize),
        ExtraPadding = (Size - TextSize) / 2,
        TotPad = Padding + round(ExtraPadding),
        print_text(Text,Symbol,TotPad),
        nl,
        print_texts(T,Symbol,Size,Padding).

% get_extra_padding(+BoardSize, -Extra)
% gets extra padding based on the size of the board
get_extra_padding(BoardSize,Extra) :-
    Extra is BoardSize - 4,
    Extra =:= 0.
get_extra_padding(BoardSize, Extra) :-
    Extra is ceiling((BoardSize - 4) / 2).

% biggest(+ListOfLists, ?Biggest)
% returns the list of ListOfLists with bigger lenght
biggest([], []).
biggest([Biggest | T], Biggest) :- 
        length(Biggest, BiggestLen),
        biggest(T, SecondBiggest),
        length(SecondBiggest, SecondBiggestLen),
        SecondBiggestLen =< BiggestLen,
        !.

biggest([_ | T], Biggest) :- biggest(T, Biggest).

% --------------------------------------------------
%               G A M E    O V E R
% --------------------------------------------------

% rotate_matrix(+Board,+N,?Res)
% rotate the first list by N elements, second list by N - 1 elements and so on
rotate_matrix([],_,[]).
rotate_matrix([L | T],N,[Rotated | Res]):-
    list_shift_rotate(L,N,Rotated),
    N1 is N - 1,
    rotate_matrix(T,N1,Res).

% get_diags(+Board, +N, +Size, ?Rest)
% get all the diags on a preparated Board (prepared in check_one_side_diags())
get_diags([],_,_,[]).
get_diags([Row | T], N,Size,[Bigger, Smaller | Rest]) :-
    append(Smaller, Bigger, Row),
    length(Bigger,N),
    SmallerLen is Size - N,
    length(Smaller, SmallerLen),
    N1 is N - 1,
    get_diags(T, N1, Size, Rest).

% check_one_side_diags(+Player, +Board)
% check 3 in a row in the diagonals (one side) of the Board 
check_one_side_diags(Player, Board) :-
    length(Board, Len),
    Size is Len - 1,
    rotate_matrix(Board, Size, TmpBoard),
    transpose(TmpBoard, NewBoard),
    get_diags(NewBoard, Len,Len,NewerBoard),
    check_rows(Player,NewerBoard).

% check_diags(+Player, +Board)
% checks 3 in a row in all diagonals of the board
check_diags(Player, Board) :-
    check_one_side_diags(Player, Board), !.
check_diags(Player, Board) :-
    reverse(Board, NewBoard),
    check_one_side_diags(Player, NewBoard).

% check_row(+Player, +Row, +Start)
% check 3 in a row in a given Row
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

% check_rows(+Player,+Board)
% check 3 in a row in all Board's rows
check_rows(Player, [Row | T]) :-
    \+ check_row(Player, Row, 0),
    check_rows(Player, T).
check_rows(Player, [Row | _]) :-
    check_row(Player, Row, 0).

% check_cols(+Player,+Board)
% check 3 in a row in all Board's cols
check_cols(Player, Board) :-
    transpose(Board, NewBoard),
    check_rows(Player, NewBoard).  

% check_board(+Player,+Board)
% check 3 in a row in the board
check_board(Player, Board) :-
    check_cols(Player, Board).
check_board(Player, Board) :-
    check_rows(Player, Board).
check_board(Player, Board) :-
    check_diags(Player, Board).

% game_over(+GameState, +Player)
% checks if the game is over
game_over([_, _, _, [C, _]], "X") :- C >= 6.
game_over([_, _, _, [_, C]], "O") :- C >= 6.
game_over(GameState, "X") :-
    get_board(GameState, Board),
    check_board("X", Board).
game_over(GameState, "O") :-
    get_board(GameState, Board),
    check_board("O", Board).

% --------------------------------------------------
%        G A M E S T A T E    G E T T E R S
% --------------------------------------------------

% get_player(+GameState, -Player)
% gets the current player
get_player(GameState, Player) :-
    nth0(0, GameState, Player).

% get_board(+GameState, -Board)
% gets the current Board
get_board(GameState, Board) :-
    nth0(1, GameState, Board).

% get_pieces(+GameState, +Player, -Pieces)
% gets the current position in form Row-Col of Player's pieces
get_pieces(GameState, Player, Pieces) :-
    findall(Row-Col, inbounds(GameState, Row, Col), Coordinates),
    filter_coordinates(GameState, Player, Coordinates, Pieces).

% get_avaliable(+GameState, -Available)
% gets the current Available pieces
get_available(GameState, Available) :-
    nth0(2, GameState, Available).
get_available(1, ["X", _, [Available, _], _], Available).
get_available(1, ["O", _, [_, Available], _], Available).
get_available(1, "X", [_, _, [Available, _], _], Available).
get_available(1, "O", [_, _, [_, Available], _], Available).

% get_captured(+GameState, -Captured)
% gets the current captured pieces
get_captured(GameState, Captured) :-
    nth0(3, GameState, Captured).
get_captured(1, ["X", _, _, [Captured, _]], Captured).
get_captured(1, ["O", _, _, [_, Captured]], Captured).
get_captured(1, "X", [_, _, _, [Captured, _]], Captured).
get_captured(1, "O", [_, _, _, [_, Captured]], Captured).

% get_square(+GameState, +Row, +Col, -Square)
% gets the value of the position (Row, Col) in the board
get_square([_, Board, _, _], Row, Col, Square) :-
    nth0(Row, Board, RowList),  
    nth0(Col, RowList, Square).

% --------------------------------------------------
%        U P D A T I N G   G A M E S T A T E
% --------------------------------------------------

% change_player(+GameState,-Player)
% changes the current player
change_player(["X",_,_,_],"O").
change_player(["O",_,_,_],"X").

% update_available(+Player,+Value, +Available, -NewAvailable)
% updates the available pieces of the players
update_available("X", Value, [_, E], [Value, E]).
update_available("O", Value, [E, _], [E, Value]).

% replace_nth(+N,+I,+V,?O)
% replace the index I with value V
replace_nth(N,I,V,O) :-
    nth0(N,I,_,T),
    nth0(N,O,V,T).

% replace_row_col(+M,?Row,?Col,?Cell,?N)
% replace in matrix M the position (Row, Col) with value Cell
replace_row_col(M,Row,Col,Cell,N) :-
    nth0(Row,M,Old),
    replace_nth(Col,Old,Cell,Upd),
    replace_nth(Row,M,Upd,N).    

% ------------------------------------------------------------------
%         I N P U T  V S .  I N T E R N A L  C O O R D I N A T E S
% -------------------------------------------------------------------

% format_coordinates(+GameState, +InRow, +InCol, -OutRow, -OutCol)
% translate coordinates from type a4 to [0,0]
format_coordinates(GameState, InRow, InCol, OutRow, OutCol) :-
    get_board(GameState, Board),
    length(Board, Size),
    number_chars(RowIntTmp, [InRow]),
    OutRow is Size - RowIntTmp,
    letter_to_number(InCol, OutCol).

% unformat_coordinates(+GameState, +[InRow, InCol], -Str)
% translate coordinates from type [0,0] to a4
unformat_coordinates(GameState, [InRow, InCol], Str) :-
    get_board(GameState, Board),
    length(Board, Size),
    InColAscii is InCol + 97,
    InRow1 is Size - InRow,
    number_codes(InRow1, InRowStr),
    append([InColAscii], InRowStr, Str).
% --------------------------------------------------
%         A N A L Y S I N G  P O S I T I O N
% --------------------------------------------------

% is_empty_square(+GameState, +Row, +Col)
% checks if the square (Row, Col) is empty
is_empty_square(GameState, Row, Col) :-
    get_square(GameState, Row, Col, Square),
    Square == " ".

% is_piece(+Piece, +GameState, +Row, +Col)
% cheks if the square (Row, Col) has the value of Piece
is_piece(Piece, GameState, Row, Col) :-
    get_square(GameState, Row, Col, Square),
    Square == Piece.

% --------------------------------------------------
%                M O V E M E N T S
% --------------------------------------------------
% make_jump(+GameState, ?RowPiece, ?ColPiece, ?NewGameState, ?RowDest, ?ColDest)
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

% move(+GameState, +Move, -NewGameState)
% executes a move
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
    
% --------------------------------------------------
%         H U M A N   M O V E   I N P U T
% --------------------------------------------------

% ask_for_move(+GameState, +Type,-Move)
% asks the player to insert a syntaticaly correct move
ask_for_move(GameState, Jumps, Move) :-
    display_jumps(GameState, Jumps, 1),
    length(Jumps, Len),
    read_until_between(1,Len,OptNum),
    nth1(OptNum, Jumps, Move), !.
ask_for_move(GameState, 'P', [place, [Row, Col]]) :-
    repeat,
    print_code("Choose a square to place a piece (e.g. a3)"), nl,
    get_char(ColTmp),
    get_char(RowTmp), skip_line,
    is_number(RowTmp,_),
    is_char(ColTmp),
    format_coordinates(GameState, RowTmp, ColTmp, Row, Col).
ask_for_move(GameState, 'M', [move, [RowSrc, ColSrc], [RowDest, ColDest]]) :-
    repeat,
    print_code("Choose a piece to move (e.g. a3)"), nl,
    get_char(ColSrcTmp),
    get_char(RowSrcTmp), skip_line,
    is_number(RowSrcTmp,_),
    is_char(ColSrcTmp),
    print_code("Choose a square to move the piece to (e.g. c2)"), nl,
    get_char(ColDestTmp),
    get_char(RowDestTmp), skip_line,
    is_number(RowDestTmp,_),
    is_char(ColDestTmp),
    format_coordinates(GameState, RowSrcTmp, ColSrcTmp, RowSrc, ColSrc),
    format_coordinates(GameState, RowDestTmp, ColDestTmp, RowDest, ColDest).

% --------------------------------------------------
%        V A L I D   M O V E S   F O R  C P U 
% --------------------------------------------------

% valid_moves(+GameState, -Moves)
% list all possible moves
valid_moves(GameState, Jumps) :-
    findall(Move, move(GameState, Move, _NewGameState), Moves),
    separated(Moves, is_jump, Jumps, _),
    length(Jumps, Len),
    Len > 0.
valid_moves(GameState, Moves) :-
    findall(Move, move(GameState, Move, _NewGameState), Moves).

% --------------------------------------------------
%     G E T T I N G   H U M A N / C P U   M O V E
% --------------------------------------------------

% choose_move(+GameState, +Mode, +Move)
% choose human/cpu move based on the choosen mode
choose_move(GameState, 'A', Move) :-
    get_player(GameState, Player),
    Player = "X", !,
    choose_move(GameState, human, Move).
choose_move(GameState, 'A', Move) :-
    choose_move(1, GameState, Move).
choose_move(GameState, 'B', Move) :-
    get_player(GameState, Player),
    Player = "X", !,
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
    ask_for_move(GameState, Jumps, Move), !.
choose_move(GameState, human, Move) :-
    repeat,
    print_code("Choose a type of move [(P)lace, (M)ove]"), nl,
    get_char(Type), skip_line,
    ask_for_move(GameState, Type, Move), !.
choose_move(1, GameState, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(2, GameState, Move) :-
    %valid_moves(GameState, Moves),
    %setof(Value-Mv, NewState^( member(Mv, Moves),move(GameState, Mv, NewState),value(NewState, Value) ), [_V-Move|_]).
    minimax(GameState,4,Move).

% --------------------------------------------------
%         G A M E   C Y C L E
% --------------------------------------------------

% game_cycle(+GameState, +Mode)
% executes game cycle
game_cycle(GameState, _) :-
    game_over(GameState, Winner),
    congratulate_winner(GameState, Winner).
game_cycle(GameState, Mode) :-
    choose_move(GameState, Mode, Move),
    move(GameState, Move, NewGameState),
    display_game(NewGameState), !,
    game_cycle(NewGameState, Mode).
game_cycle(GameState, Mode) :-
    print_code("Invalid move!"), nl,
    game_cycle(GameState, Mode).

% play
% used to initiate game
play :-
    get_game_mode(Mode),
    get_size(Size),
    initial_state(Size, GameState),
    display_game(GameState),
    game_cycle(GameState, Mode).
% --------------------------------------------------
%         U S E R     I N T E R F A C E
% --------------------------------------------------

% print_multi_banner(+LoL, +Symbol, +Padding)
% displays all texts in LoL, with padding of Padding chars and delimited by Symbol
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

% get_row_divisor(+Size, +InitSize, -Divisor)
% used to separate the print of the board's rows. Replicates Size times the string "    |"
get_row_divisor(0,InitSize, "") :- 
    0 =:= InitSize mod 2.
get_row_divisor(0,_InitSize," ").
get_row_divisor(N, InitSize, New) :-
    append("    |",Res, New),
    N1 is N - 1,
    get_row_divisor(N1, InitSize, Res).

% get_col_coordinates(+Size, +InitSize, +Code, -Coordinates)
% gets column coordinates for board in the format "    a    b    c"...
get_col_coordinates(0, InitSize, _, "") :-
    0 =:= InitSize mod 2.
get_col_coordinates(0, _InitSize, _, " ").
get_col_coordinates(N, InitSize, Code, Coordinates) :-
    append("    ",[Code],Col),
    append(Col,Res,Coordinates),
    N1 is N - 1,
    Code1 is Code + 1,
    get_col_coordinates(N1,InitSize, Code1, Res).

% get_col_coordinates(+Size, +InitSize, -Coordinates)
% gets column coordinates for board in the format "    a    b    c"...
get_col_coordinates(N, InitSize, Coordinates) :- 
    char_code('a',Code),
    get_col_coordinates(N, InitSize, Code, Coordinates).

% add_space_if_odd(+Line, -Line)
% if Line has odd length then appends a space in the end of Line, otherwise do nothing
add_space_if_odd(Line, Line) :-
    length(Line,N),
    0 =:= N mod 2.
add_space_if_odd(TmpLine, Line) :-
    append(TmpLine, " ", Line).

% print_board(+Board, +RowNumber,+BoardSize)
% print the board and coordinates
print_board([X | _],1,InitSize):-
    intersperse(X," -- ",TmpLine),
    add_space_if_odd(TmpLine, Line),
    number_codes(1, NumberPrint),
    append(NumberPrint,"   ",Coordinate),
    append(Coordinate,Line,Printable),
    Pad is 13 - (InitSize - 4)*2,
    print_text(Printable,'*',Pad),nl,
    get_extra_padding(InitSize, ExtraPadding),
    OtherPad is 23 + ExtraPadding,
    print_text("",'*',OtherPad),nl,
    get_col_coordinates(InitSize, InitSize, ColCoords),
    print_text(ColCoords,'*',Pad),nl.

print_board([X | XS],BoardSize,InitSize):-
    intersperse(X," -- ",TmpLine),
    add_space_if_odd(TmpLine, Line),
    number_codes(BoardSize,NumberPrint),
    append(NumberPrint,"   ",Coordinate),
    append(Coordinate,Line,Printable),
    Pad is 13 - (InitSize - 4)*2,
    print_text(Printable,'*',Pad),
    nl,
    get_row_divisor(InitSize, InitSize, RowDivisor),
    print_text(RowDivisor,'*',Pad),nl,
    NewSize is BoardSize - 1,
    print_board(XS,NewSize,InitSize).

% get_printable_info(+GameState, +Msg)
% Based on GameState, extracts player, available and captured pieces and construct 2 printable strings in Msg
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

% display_game(+GameState)
% displays the game
display_game(GameState):- 
    get_board(GameState,Board),
    length(Board,BoardSize),
    get_extra_padding(BoardSize, ExtraPadding),
    TitlePadding is 18 + ExtraPadding,
    print_multi_banner([" M O X I E"],'*',TitlePadding),nl,
    Padding is 23 + ExtraPadding,
    print_text("",'*',Padding),nl,
    print_board(Board,BoardSize,BoardSize),
    print_text("",'*',Padding),nl,
    % computing strings based on GameState
    get_printable_info(GameState,[A,B]),
    InfoPadding is 2 + ExtraPadding,
    print_multi_banner(["Avaliable pieces:        Captured pieces: ",A,"",B],'*',InfoPadding),nl.

% congratulate_winner(+GameState, +Winner)
% prints a banner with a congratulation message to the winner of the game
congratulate_winner(GameState, Winner):- 
    append("The winner is  ",Winner,Msg),
    get_board(GameState,Board),
    length(Board,Len),
    get_extra_padding(Len,Extra),
    Pad is 15 + Extra,
    print_multi_banner([Msg],'*',Pad).

% verify_opt(+Opt)
% verifies if game mode option is valid
verify_opt(Opt) :-
    Opt \= 'A', Opt \= 'B', Opt \= 'C', 
    print_code("Please enter a valid mode!"), nl.

% get_game_mode(-Opt)
% gets in Opt the mode of the game
get_game_mode(Opt) :-
    print_multi_banner([" A - 1P  vs  CPU-Easy"," B - 1P  vs  CPU-Hard", "   C - 1P  vs  2P        "],'*',16),nl,
    repeat,
    print_code("Choose a game mode:"), nl,
    get_char(Opt), skip_line,
    \+ verify_opt(Opt), !.

% get_size(+Size)
% gets the size of the board by user input
get_size(Size) :-
    repeat,
    print_code("Choose a size for the board (between 3 and 9):"), nl,
    read_number(Size),
    check_boundaries(Size, 3, 9),
    !.

% print_jump(+GameState, +Jump)
% displays one jump possibility
print_jump(GameState, [H]) :-
    unformat_coordinates(GameState, H, PrintableJump),
    print_code(PrintableJump).
print_jump(GameState, [H | T]) :-
    unformat_coordinates(GameState, H, PrintableJump),
    print_code(PrintableJump),
    print_code(" -> "),
    print_jump(T).

% display_jumps(+GameState, +Jumps,+OptNumber)
% displays all jump possibilities
display_jumps(_, [], _).
display_jumps(GameState, [Jump | T],N) :- 
    nth0(2,Jump,[Piece | Places]),
    unformat_coordinates(GameState, Piece, PrintablePiece),
    number_codes(N,PrintableNumber),
    print_code(PrintableNumber),
    print_code(" - "),
    print_code("Piece: "),
    print_code(PrintablePiece),
    print_code(" "),
    print_code("Jumps: "),
    print_code(PrintablePiece),
    print_code(" -> "),
    print_jump(GameState, Places),
    nl,
    N1 is N + 1,
    display_jumps(T,N1).

% --------------------------------------------------
%    A R T I F I C I A L  I N T E L I G E N C E
% --------------------------------------------------

% min4 (+V1, +V2, +V3, +V4, -Min)
% finds the minimum between 4 values
min4(V1, V2, V3, V4, V1) :-
    V1 =< V2,
    V1 =< V3,
    V1 =< V4.
min4(_, V2, V3, V4, V2) :-
    V2 =< V3,
    V2 =< V4.
min4(_, _, V3, V4, V3) :-
    V3 =< V4.
min4(_, _, _, V4, V4).

% value_piece(+GameState, +Position, -Value)
% gives a value to a piece based on it's position on the board
value_piece(GameState, Row-Col, Value) :-
    get_board(GameState, Board),
    length(Board,TmpLen),
    Len is TmpLen - 1,
    distance(Row,Col,0,0,D1),
    distance(Row,Col,Len,Len,D2),
    distance(Row,Col,0,Len,D3),
    distance(Row,Col,Len,0,D4),
    min4(D1,D2,D3,D4,Min),
    Value is 5 + (Min * 2) / TmpLen.
    
% value_pieces(+Player, +GameState, -Value)
% gives a value to all pieces of the Player and sums all values in Value
value_pieces(Player, GameState, Value) :-
    get_pieces(GameState, Player, Pieces),
    value_pieces(Player, GameState, Pieces, Value).
value_pieces(_Player, _GameState, [], 0).
value_pieces(_Player, GameState, [Piece], Value) :-
    value_piece(GameState, Piece, Value).
value_pieces(Player, GameState, [Piece | Rest], Value) :-
    value_pieces(Player, GameState, Rest, ValueRest),
    value_piece(GameState, Piece, ValuePiece),
    Value is ValueRest + ValuePiece.

% value(+GameState, -Value)
% evaluates the board based on pieces' position on the board and on the number of captured pieces
value(GameState, 10000) :-
    game_over(GameState, "X").
value(GameState, -10000) :-
    game_over(GameState, "O").
value(GameState, Value) :-
    get_captured(1, "X", GameState, CapturedX),
    get_captured(1, "O", GameState, CapturedO),
    value_pieces("X", GameState, ValuePiecesX),
    value_pieces("O", GameState, ValuePiecesO), !,
    Value is ValuePiecesX - ValuePiecesO + CapturedX * 7 - CapturedO * 7.
    
% change_max_min(+MinOrMax, TheOther)
% Changes the MinMax atom.
change_max_min(max, min).
change_max_min(min, max).

% compare_moves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -BetterValue)
% Chooses the move with the higher value.
compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

% best_move(+MinMax, +AllMoves, -BestMove, -BestValue)
% Chooses the next move.
best_move(MinMax, GameState, [Move], Move, Value, Depth) :-
    move(GameState, Move, NewGameState),
    change_max_min(MinMax, Other),
    minimax_step(Other, NewGameState, _, Value, Depth).
best_move(MinMax, GameState, [Move | RestMoves], BestMove, BestValue, Depth) :-
	best_move(MinMax, GameState, RestMoves, CurrentBestM, CurrentBestV, Depth),
	change_max_min(MinMax, Other),
    move(GameState, Move, NewGameState),
	minimax_step(Other, NewGameState, _, BottomBestV, Depth),
	compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).


% minimax_step(+MinMax, +Board, -BestMove, -BestValue)
% Chooses the best possible move for the current board.
minimax_step(_, GameState, _, BestValue, 0) :-
    value(GameState, BestValue).
minimax_step(_, GameState, _, BestValue, _) :-
	valid_moves(GameState, []),
    value(GameState, BestValue).
minimax_step(MinMax, GameState, BestMove, BestValue, Depth) :-
    Depth1 is Depth - 1,
	valid_moves(GameState, AllMoves),
    best_move(MinMax, GameState, AllMoves, BestMove, BestValue, Depth1).

% minimax(+Board, -BestMove)
% Matches the next move based on the current board.
minimax(GameState, Depth, BestMove) :-
	minimax_step(min, GameState, BestMove, _, Depth).