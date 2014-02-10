%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%  Part 1  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_strategy(N, S1, S2) :-
  statistics(walltime, _),
  test_strategy_helper(N, S1, S2, Outcome, Time),
  winner_stats(Outcome, Red, Blue, Draw),
  length(Red, RWin),
  length(Blue, BWin),
  length(Draw, DWin),
  format('Number of wins for Blue: ~w~n', [BWin]),
  format('Number of wins for Red: ~w~n', [RWin]),
  format('Number of Draws: ~w~n', [DWin]),
  duration_stats(Red, Blue, Draw),
  time_stats(Time).

test_strategy_helper(0, _, _, [], []).
test_strategy_helper(N, S1, S2, [(Win, Mov)| Rest], [T | Time]) :-
  play(quiet, S1, S2, Mov, Win),
  statistics(walltime, [_, T]),
  Ndec is N - 1,
  test_strategy_helper(Ndec, S1, S2, Rest, Time).  

winner_stats([], [], [], []).
winner_stats([(r, N) | Rest], [N | Red], Blue, Draw) :-
  winner_stats(Rest, Red, Blue, Draw).
winner_stats([(b, N) | Rest], Red, [N | Blue], Draw) :-
  winner_stats(Rest, Red, Blue, Draw).
winner_stats([(W, N) | Rest], Red, Blue, [N | Draw]) :-
  (W == draw ; W == exhaust ; W == stalemate), 
  winner_stats(Rest, Red, Blue, Draw).

duration_stats(L1, L2, L3) :-
  append(L1, L2, Temp),
  append(Temp, L3, GameMoves),
  min_member(Min, GameMoves),
  max_member(Max, GameMoves),
  average(Av, GameMoves),
  Max < 250,
  format('Longest Game (non-exhaustive): ~w~n', [Max]),
  format('Shortest Game: ~w~n', [Min]),
  format('Average game length (inc exhaustives): ~w~n', [Av]).

time_stats(Time) :-
  average(Av, Time),
  S is Av/1000,
  format('The Average Game time was ~wsecs ~n', [S]).

average(Av, List) :-
  length(List, Len),
  Len > 0,
  sumlist(List, Sum),
  Av is Sum / Len.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%  Part 3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bloodlust(Col, Board, NewBoard, Move) :-
  player_opponent(Col, Board, Player, Opponent),
  valid_moves(Player, Opponent, ValidMoves),
  make_all_moves(Col, Player, Opponent, ValidMoves, MadeMoves),
  bl_get_move(Col, MadeMoves, ([], 64), Move),
  gen_new_board(Col, Move, Player, Opponent, NewBoard).

bl_get_move(_, [], (Move, _), Move).
bl_get_move(Col, [[ValidM, P1, P2] | ValidMs], (BestM, N), FinalM) :-
  length(P1, LenP1),
  length(P2, LenP2),
  (Col == b ->
    New is LenP2;
    New is LenP1),
  (New < N ->
    bl_get_move(Col, ValidMs, (ValidM, New), FinalM);
    bl_get_move(Col, ValidMs, (BestM, N), FinalM)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

self_preservation(Col, Board, NewBoard, Move) :-
  player_opponent(Col, Board, Player, Opponent),
  valid_moves(Player, Opponent, ValidMoves),
  make_all_moves(Col, Player, Opponent, ValidMoves, MadeMoves),
  sp_get_move(Col, MadeMoves, ([], 0), Move),
  gen_new_board(Col, Move, Player, Opponent, NewBoard).

sp_get_move(_, [], (Move, _), Move).
sp_get_move(Col, [[ValidM, P1, P2] | ValidMs], (BestM, N), FinalM) :-
  length(P1, LenP1),
  length(P2, LenP2),
  (Col == b ->
    New is LenP1;
    New is LenP2),
  (New > N ->
    sp_get_move(Col, ValidMs, (ValidM, New), FinalM);
    sp_get_move(Col, ValidMs, (BestM, N), FinalM)).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

land_grab(Col, Board, NewBoard, Move) :-
  player_opponent(Col, Board, Player, Opponent),
  valid_moves(Player, Opponent, ValidMoves),
  make_all_moves(Col, Player, Opponent, ValidMoves, MadeMoves),
  lg_get_move(Col, MadeMoves, ([], 0), Move),
  gen_new_board(Col, Move, Player, Opponent, NewBoard).

lg_get_move(_, [], (Move, _), Move).
lg_get_move(Col, [[ValidM, P1, P2] | ValidMs], (BestM, N), FinalM) :-
  length(P1, LenP1),
  length(P2, LenP2),
  (Col == b ->
    New is LenP1 - LenP2;
    New is LenP2 - LenP1),
  (New > N ->
    lg_get_move(Col, ValidMs, (ValidM, New), FinalM);
    lg_get_move(Col, ValidMs, (BestM, N), FinalM)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minimax(Col, Board, NewBoard, Move) :-
  player_opponent(Col, Board, Player, Opponent),
  valid_moves(Player, Opponent, ValidM),
  make_all_moves(Col, Player, Opponent, ValidM, MadeM),
  valid_moves_2(MadeM, ValidM2),
  make_all_moves_2(Col, Player, Opponent, ValidM2, MadeM2),
  mm_get_move_2(Col, MadeM2, Move),
  gen_new_board(Col, Move, Player, Opponent, NewBoard).

mm_get_move_2(Col, [[Vm1, NextM] | Ms], Move) :-
  findall((Vm1, N), mm_get_move(Col, NextM, ([], 64), (_, N)),
           IndexM),
  get_max(IndexM, Move).

get_max([], (M, _), M).
get_max([(Mv, MvN) | Moves], (M, N), Move) :-
  (MvN > N ->
   get_max(Moves, (Mv, MvN), Move);
   get_max(Moves, (M, N), Move)).
  
mm_get_move(_, [], X, X).
mm_get_move(Col, [[ValidM, P1, P2] | ValidMs], (BestM, N), FinalM) :-
  length(P1, LenP1),
  length(P2, LenP2),
  (Col == b ->
    New is LenP2 - LenP1;
    New is LenP1 - LenP2),
  (New < N ->
    mm_get_move(Col, ValidMs, (ValidM, New), FinalM);
    mm_get_move(Col, ValidMs, (BestM, N), FinalM)).

valid_moves_2(Col, MadeM, ValidM) :-
  findall([Vm1, Vm2, CP1, CO1], 
         (member([Vm1, CP1, CO1], ValidM),
          player_opponent(Col, [CP1, CO1], Player, Opponent),
          valid_moves(Opponent, Player, Vm2)), ValidM).

make_all_moves_2(Col, P1, P2, ValidM, MadeM) :-
  findall([Vm1, Level2],
         (member([Vm1, Vm2, CP1, OP1], ValidM),
          player_opponent(Col, [CP1, CO1], Player, Opponent),
          make_all_moves(Col, Opponent, Player, Vm2, Level2)), 
          MadeM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%  Useful Utils  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_new_board(b, Move, Blue, Red, [NewBlue, Red]) :-
  alter_board(Move, Blue, NewBlue).
gen_new_board(r, Move, Red, Blue, [Blue, NewRed]):-
  alter_board(Move, Red, NewRed).

make_all_moves(Col, P1, P2, ValidMoves, MovesMade) :-
  findall([Vm, CP1, CP2], (member(Vm, ValidMoves), 
          alter_board(Vm, P1, NewP1), 
          next_generation([NewP1, P2], [CP1, CP2])),
          Moves), 
  (Col == b ->
    MovesMade = Moves;
    switch(Moves, MovesMade)).

switch([], []).
switch([[M, P1, P2] | ColIndep], [[M, P2, P1] | Moves]) :-
  switch(ColIndep, Moves).

valid_moves(Player, Opponent, ValidMoves) :-
  findall([A, B, MA, MB], (member([A,B], Player),
          neighbour_position(A,B,[MA, MB]),
          \+member([MA, MB], Player),
          \+member([MA, MB], Opponent)), ValidMoves).

player_opponent(Col, [P1, P2], Player, Opponent) :-
  Col == b ->
  Player = P1, Opponent = P2;
  Player = P2, Opponent = P1.

