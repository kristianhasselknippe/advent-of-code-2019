:- use_module(library(mavis)).
:- use_module(library(achelois)).
:- use_module(library(function_expansion)).
:- use_module(aoc10).

%% even(+X:integer) is semidet.
even(X) :-
    0 is X mod 2.

%% foreach_asteroid(++Rows:list, -Out:list) is det
foreach_asteroid(Rows, Out) :-
	ground(Rows),
	%write(Rows),
	length(Rows, Len),
	maplist(
		[Row, ROut]>>(
			string_chars(Row, Positions),
			length(Positions, RowLen),
			maplist([Asteroid,O]>>( O = 2 ), Positions, A)
		), Rows, Out).

user:function_expansion(len(A), Out, length(A, Out)).

map_asteroids(Rows, Out) :-
	ground(Rows)
	maplist(
		{Rows}/[Row,MappedRow]>>(
			string_chars(Row, Positions)
			maplist([Item]
		), Rows, Out).

main(_) :-
	read_lines_from_file('./input.txt', Rows),
	write(len(Rows)),
	maplist({Rows}/[X,O]>>(
				foreach_asteroid(Rows, O),
				nl
			), Rows, OO).
