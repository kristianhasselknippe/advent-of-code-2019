:- use_module(aoc).
:- use_module(library(yall)).

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
		(Direction = "U", Line = line(From, point(X, Y - Distance) ));
		(Direction = "D", Line =line(From, point(X, Y + Distance) ));
		(Direction = "L", Line = line(From, point(X - Distance, Y) ));
		(Direction = "R", Line = line(From, point(X + Distance, Y) ))
	).

decode_command(CommandString, Command) :-
	format('Decoding command ~w\n', CommandString),
	format('Is string: ~w\n', string(CommandString),
	string_chars(CommandString, [Direction| Distance]),
	format('Direction: ~w, Distance ~w\n', Direction, Distance),
	string_chars(DirString, [Direction]),
	number_chars(DistNum, [Distance]),
	Command = command(DirString, DistNum),
	format('Command is: ~w\n', Command).

coordinates_from_description([],[], _).
coordinates_from_description([WH|WRest], [CH|CRest], LastOrigin) :-
	write('Last origin: '), write(LastOrigin), nl,
	point(OX,OY) = LastOrigin,
	%format('X: ~w, Y: ~w\n', OX, OY),
	decode_command(WH, Command),
	apply_command(Command, LastOrigin, CH),
	format('The line: ~w \n', CH).

coordinates_from_description(Wire, Coordinates) :-
	write('The wire: '), write(Wire),nl,
	Origin = point(0,0),
	coordinates_from_description(Wire, Coordinates, Origin).

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(string_to_op_list, Lines, Wires),
	[Wire1,_Wire2] = Wires,
	%maplist([Input]>>format('~w,\n', Input), Wire1),
	coordinates_from_description(Wire1, Coords1),
	write(Coords1).

foo([A|B]) :-
	write(A), nl, write(B), nl.

