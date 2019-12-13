:- module(moon, [make_moon/4,add_velocity/2]).

add_gravity_for(A,B,Out) :-
	format('A: ~w, B: ~w~n', [A,B]),
	(A =< B -> Out is A + 1 ; Out is A - 1).

M.len() := Len :-
	  Len is M.x + M.y + M.z.

make_moon(X,Y,Z, Out) :-
	Out = moon{x:X, y:Y, z:Z, velX:0, velY:0, velZ:0}.

M.add_gravity(O) := moon{x:M.x, y:M.y, z:M.z, velX:VelX, velY:VelY, velZ:VelZ} :-
	  (M.x =< O.x -> VelX is M.velX + 1 ; VelX is M.velX - 1),
	  (M.y =< O.y -> VelY is M.velY + 1 ; VelY is M.velY - 1),
	  (M.z =< O.z -> VelZ is M.velZ + 1 ; VelZ is M.velZ - 1).

add_velocity(A,moon{x:NewX, y:NewY, z:NewZ, velX:A.velX, velY:A.velY, velZ:A.velZ}) :-
	  NewX is A.x + A.velX,
	  NewY is A.y + A.velY,
	  NewZ is A.z + A.velZ.
