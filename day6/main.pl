:- use_module(aoc).
:- use_module(library(yall)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

orbit(Center, Orbiter).

decode_orbit(Line, Orbit) :-
	split_string(Line, ")", "", [Center, Orbiter]),
	Orbit = orbit(Center, Orbiter).

indirect_orbit(A,C) :-
	orbit(A,B),
	orbit(B,C).

path(A,B,Orbits,Length) :-   % two nodes are connected, if
	walk(A,B,[],Orbits), % - if we can walk from one to the other,
	length(Edges, Length).

walk(A,B,Orbits,Orbits).
walk(A,B,V,Orbits) :-
  orbit(A,X) ,
  not(member(X,V)) ,
  (
	B = X
  ;
	walk(X,B,[A|V])
  ).

indirect_orbits(A, B, Count, Num) :-
	indirect_orbit(A,B).

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(decode_orbit, Lines, Orbits),
	maplist([X,[A,B]]>>(X = orbit(A,B)), Orbits, Pairs),
	flatten(Pairs, OrbitersWithDups),
	sort(OrbitersWithDups, Orbiters),
	path('Z58', 'COM', Orbits, Length),
	write(Length).
