:- use_module(library(apply)).

read_lines(Stream, []) :-
	at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Line),
	read_lines(Stream, Rest).

sum(A, B, Out) :-
	Out is A + B.

sum_of_numbers(List, Out) :-
	foldl(sum, List, 0, Out).

calcRequiredFuel(Mass, 0) :-
	Mass < 0.
calcRequiredFuel(Mass, Fuel) :-
	Y is Mass div 3,
	Fuel is max((Y - 2), 0).

requiredFuel(0, 0).
requiredFuel(0, Mass) :-
	Mass < 0.
requiredFuel(Fuel, Mass) :-
	calcRequiredFuel(Mass, SubTot),
	write(SubTot), nl,
	requiredFuel(Tot, SubTot),
	Fuel is Tot + SubTot.

main :-
	open("./input.txt", read, Stream),
	read_lines(Stream, Lines),
	close(Stream),
	maplist(number_codes, Masses, Lines),
	write(Masses), nl,
	maplist(requiredFuel, Fuel, Masses),
	write(Fuel), nl,
	sum_of_numbers(Fuel, TotalFuelRequired),
	write(TotalFuelRequired).

