%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%  Question 2  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_strategy(N, S1, S2) :-
  statistics(walltime, _),
  test_strategy_helper(N, S1, S2, Outcome, Time),
  winner_stats(Outcome, Red, Blue, Draw),
  length(Red, RWin),
  length(Blue, BWin),
  length(Draw, DWin),
  format('Number of wins for Red: ~w~n', [RWin]),
  format('Number of wins for Blue: ~w~n', [BWin]),
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
%%%%%%%%%%%%%%%%%%%%  Question 3  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move). 
self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move). 
land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move). 
minimax(PlayerColour, CurrentBoardState, NewBoardState, Move).




