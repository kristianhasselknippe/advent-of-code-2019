:- use_module(library(achelois)).
:- use_module(library(function_expansion)).
:- use_module(aoc7).
:- use_module(moon).

all_pairs(In, Out) :-
	findall([X,Y], (member(X, In), member(Y,In)), Out).

apply_gravity(In, Out) :-
	all_pairs(In, Pairs),
	maplist([[X,Y],Updated]>>(
			   Updated = X.add_gravity(Y)
			), Pairs, Out),
	write('done gravity\n').

apply_velocity(In, Out) :-
	format('Doing it ~w~n', [In]),
	maplist([X,Updated]>>(
				format('X: ~w, ~n', [X]),
				add_velocity(X, Updated),
				format('Updated: ~w', [Updated])
			), In, Out).

test1() :-
	make_moon(-1, 0, 2, A),
	make_moon(2, -10, -7, B),
	make_moon(4, -8, 8, C),
	make_moon(3, 5, -1, D),
	Moons = [A,B,C,D],
	apply_gravity(Moons, WithGravityApplied),
	write('foooooo\n'),
	apply_velocity(WithGravityApplied, WithVelocity),
	format('OUTPUT ~w~n', [WithVelocity]).

main(_) :-
	make_moon(13, 9, 5,A),
	make_moon(8, 14, -2,B),
	make_moon(-5, 4, 11,C),
	make_moon(2, -6, 1,D),
	Moons = [A,B,C,D],
	format('Moons ~w~n', [Moons]),
	apply_gravity(Moons, WithGravityApplied),
	write('foooooo\n'),
	apply_velocity(WithGravityApplied, WithVelocity),
	format('With gravity ~w~n', [WithVelocity]).
