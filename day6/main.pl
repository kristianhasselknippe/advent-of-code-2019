:- use_module(aoc).
:- use_module(library(yall)).
:- use_module(library(lists)).

orbit(A, B) :-
	string(A),
	string(B).

decode_orbit(Line, Orbit) :-
	split_string(Line, ")", "", [Center, Orbiter]),
	Orbit = orbit(Center, Orbiter).

path(A,A,Orbits,0).
path(To,From,Orbits,Length) :-
	format('Walking to orbiter: ~w, from ~w~n', [To, From]),
	walk(Orbits,To,From,[],[],Path),
	length(Path, Length).

walk(Orbits,A,B,Visited,AccPath,Out) :-
	member(Member, Orbits),
	Member = orbit(A,X),
	Out = [Member|AccPath],
	length(Out, Len),
	B = X.

walk(Orbits,A,B,Visited,AccPath,Path) :-
	length(AccPath, PathLength),
	member(Member, Orbits),
	(
		orbit(A,X) = Member;
		orbit(X,A) = Member
	),
	not(member(X,Visited)),
	(
		(
			Out = [Member|AccPath],
			B = X
		);
		(
			walk(Orbits,X,B,[A|Visited],[orbit(A,X)|AccPath],Path)
		)
	).

sum(A, B, Out) :-
	Out is A + B.

sum_of_numbers(List, Out) :-
	foldl(sum, List, 0, Out).


part_one(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(decode_orbit, Lines, Orbits),
	maplist([X,[A,B]]>>(X = orbit(A,B)), Orbits, Pairs),
	flatten(Pairs, OrbitersWithDups),
	sort(OrbitersWithDups, Orbiters),
	%format('Orbits: ~w~n', [Orbits]),
	maplist([Orbiter]>>path("COM", Orbiter, Orbits), Orbiters, Lengths),
	%path("COM", "D", Orbits, Length),
	format('Lengths: ~w~n', Lengths),
	sum_of_numbers(Lengths, Result),
	format('Total length: ~w~n', Result).

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(decode_orbit, Lines, Orbits),
	%format('Orbits ~w~n', [Orbits]),
	path("SAN", "YOU", Orbits, Length),
	WithoutFirstSteps is Length - 2,
	format('Length from YOU to SAN: ~w~n', WithoutFirstSteps).
