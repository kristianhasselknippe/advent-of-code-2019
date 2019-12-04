:- use_module(aoc).
:- use_module(library(yall)).
:- use_module(library(clpfd)).

string_to_op_list(String, OpList) :-
	split_string(String, ",", "", OpList).

point(X,Y) :-
	number(X),
	number(Y).

print_point(P) :-
	point(X, Y) = P,
	format('X: ~w, Y: ~w', X, Y).

line(A,B) :-
	point(_, _) = A,
	point(_, _) = B.

command(_Direction, _Distance).

apply_command(Command, From, Line) :-
	command(Direction, Distance) = Command,
	point(X,Y) = From,
	(
		(Direction = "U", NY is Y - Distance, Line = line(From, point(X, NY) ));
		(Direction = "D", NY is Y + Distance, Line = line(From, point(X, NY) ));
		(Direction = "L", NX is X - Distance, Line = line(From, point(NX, Y) ));
		(Direction = "R", NX is X + Distance, Line = line(From, point(NX, Y) ))
	).

decode_command(CommandString, Command) :-
	string_chars(CommandString, [Direction| Distance]),
	string_chars(DirString, [Direction]),
	number_chars(DistNum, Distance),
	Command = command(DirString, DistNum).

coordinates_from_description([],[], _).
coordinates_from_description([WH|WRest], [CH|CRest], LastOrigin) :-
	point(OX,OY) = LastOrigin,
	decode_command(WH, Command),
	apply_command(Command, LastOrigin, CH),
	%format('The line: ~w \n', CH),
	line(_, NewOrigin) = CH,
	coordinates_from_description(WRest, CRest, NewOrigin).

coordinates_from_description(Wire, Coordinates) :-
	Origin = point(0,0),
	coordinates_from_description(Wire, Coordinates, Origin).

% all lines are either horizontal or vertical
is_horizontal(line(point(XA, YA), point(XB,YB))) :-
	XA #\= XB,
	YA = YB.
is_vertical(A) :-
	\+ is_horizontal(A).


perpendicular(LA, LB) :-
	is_horizontal(LA),
	is_vertical(LB).

are_perpendicular(LA,LB) :-
	perpendicular(LA,LB);
	perpendicular(LB,LA).

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(string_to_op_list, Lines, Wires),
	[Wire1,_Wire2] = Wires,
	%maplist([Input]>>format('~w,\n', Input), Wire1),
	coordinates_from_description(Wire1, Coords1),
	coordinates_from_description(Wire2, Coords2).


