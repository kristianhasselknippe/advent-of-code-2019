:- use_module(library(achelois)).
:- use_module(library(function_expansion)).
:- use_module(aoc12).

add_gravity_for(A,B,Out) :-
	format('A: ~w, B: ~w~n', [A,B]),
	(A =< B -> Out is A + 1 ; Out is A - 1).

make_moon(X,Y,Z, Out) :-
	Out = moon{x:X, y:Y, z:Z, velX:0, velY:0, velZ:0}.

add_gravity(M, O, Out) :-
	Out = moon{x:M.x, y:M.y, z:M.z, velX:VelX, velY:VelY, velZ:VelZ},
	(M.x =< O.x -> VelX is M.velX + 1 ; VelX is M.velX - 1),
	(M.y =< O.y -> VelY is M.velY + 1 ; VelY is M.velY - 1),
	(M.z =< O.z -> VelZ is M.velZ + 1 ; VelZ is M.velZ - 1).

add_velocity(A,moon{x:NewX, y:NewY, z:NewZ, velX:A.velX, velY:A.velY, velZ:A.velZ}) :-
	  NewX is A.x + A.velX,
	  NewY is A.y + A.velY,
	  NewZ is A.z + A.velZ.


all_pairs(In, Out) :-
	findall([X,Y], (member(X, In), member(Y,In)), Out).

apply_gravity_for_all_moons(X, In, Out) :-
	foldl({X}/[A,B,O]>>(
			  add_gravity(X,A,O)
		  ), In, X, Updated).

apply_gravity(In, Out) :-
	maplist({In}/[X,Updated]>>(
				apply_gravity_for_all_moons(X, In, Updated)
			), In, Out).

apply_velocity(In, Out) :-
	maplist([X,Updated]>>(
				add_velocity(X, Updated)
			), In, Out).

test1() :-
	make_moon(-1, 0, 2, A),
	make_moon(2, -10, -7, B),
	make_moon(4, -8, 8, C),
	make_moon(3, 5, -1, D),
	Moons = [A,B,C,D],
	apply_gravity(Moons, WithGravityApplied),
	format('After applying~w~n', WithGravityApplied),
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
	format('After applying~w~n', WithGravityApplied),
	apply_velocity(WithGravityApplied, WithVelocity),
	format('With gravity ~w~n', [WithVelocity]).
