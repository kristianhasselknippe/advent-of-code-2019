:- use_module(aoc).
:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

orbit(Center, Orbiter) :-
	string(Center), string(Orbiter).

decode_orbit(Line, Orbit) :-
	split_string(Line, ")", "", [Center, Orbiter]),
	Orbit = orbit(Center, Orbiter).

indirect_orbit(A,C) :-
	orbit(A,B),
	orbit(B,C).

path(A,A,Orbits,0).   % two nodes are connected, if
path(A,B,Orbits,Length) :-   % two nodes are connected, if
	%format('Walking for orbiter: ~w~n', B),
	walk(Orbits,A,B,[],[],Path), % - if we can walk from one to the other,
	length(Path, Length).

walk(Orbits,A,B,Visited,AccPath,Out) :-
	member(Member, Orbits),
	Member = orbit(A,X),
	Out = [Member|AccPath],
	length(Out, Len),
	%format('   found path~w\n', Len),
	B = X.

walk(Orbits,A,B,Visited,AccPath,Path) :-
	length(AccPath, PathLength),
	%format('path length ~w~n', [PathLength]),
	member(Member, Orbits),
	Member = orbit(A,X),
	not(member(X,Visited)),
	(
		(
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

indirect_orbits(A, B, Count, Num) :-
	indirect_orbit(A,B).

main(_) :-
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
