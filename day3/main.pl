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

are_perpendicular(LA,LB, Horizontal, Vertical) :-
	(
		(perpendicular(LA,LB), Horizontal = LA, Vertical = LB);
		(perpendicular(LB,LA), Vertical = LA, Horizontal = LB)
	).

range(A,B).

sorted_range(range(A,A), range(A,A)).
sorted_range(range(A,B), range(A,B)) :-
	A < B.
sorted_range(range(A,B), range(B,A)) :-
	A > B.

sorted_on_start(range(A1,A2), range(B1,B2), First, Last) :-
	(
		A1 #< B1,
		First = range(A1,A2),
		Last = range(B1,B2)
	);
	(
		First = range(B1,B2),
		Last = range(A1,A2)
	).

range_overlap_impl(range(A,B), range(A,B), range(A,B)).
range_overlap_impl(range(A,B), range(B,C), range(B,B)).
range_overlap_impl(range(A,C), range(B,C), range(B,C)) :-
	A < B.
range_overlap_impl(range(A1,A2), range(B1,B2), Out) :-
	B1 #>= A1,
	B1 #=< A2,
	(
		(
			write('foo'), nl,
			B2 #=< A2,
			Out = range(B1,B2)
		);
		(
			write('bar'), nl,
			A2 #=< B2,
			Out = range(B1, A2)
		)
	).

range_overlap(A,B,Out) :-
	sorted_range(A, ASorted),
	sorted_range(B, BSorted),
	format('A: ~w, B: ~w~n', [ASorted,BSorted]),
	sorted_on_start(ASorted,BSorted,L,R),
	format('Left ~w, Right: ~w~n', [L,R]),
	range_overlap_impl(L,R,Out).

are_crossing([LA, LB], CrossPoints) :-
	(
		are_perpendicular(
			LA,LB,
			line(point(HX1, HY1), point(HX2, HY2)),
			line(point(VX1, VY1), point(VX2, VY2))
		),
		MinHorX is min(HX1, HX2),
		MaxHorX is max(HX1, HX2),
		VX1 #= VX2,
		MinHorX =< VX1, VX1 =< MaxHorX,
		CrossPoints = [point(HY1, VX1)]
	),
	(
		is_horizontal(LA),
		line(point(XA1, _), point(XA2, _)) = LA,
		line(point(XB1, _), point(XB2, _)) = LB
		%MinHorX is min(min(HX1, HX2), mina(HX2
	)
	.
%format('And crossing at: ~w\n', CrossPoint).

cross_point_list([], Out, Out).
cross_point_list([H|T], CrossPoints, Out) :-
	(
		are_crossing(H, Crossing),
		append(CrossPoints, Crossing, AllSoFar),
		cross_point_list(T, AllSoFar, Out)
	);
	cross_point_list(T, CrossPoints, Out).

cross_point_list(X, Out) :-
	cross_point_list(X, [], Out).


find_crossing_points(ListA, ListB, CrossPoints) :-
	length(ListA, LALen),
	length(ListB, LBLen),
	format("Finding crossing, ListA Len ~w, ListB Len: ~w", [LALen, LBLen]),nl,
	findall([A,B], (member(A, ListA), member(B, ListB)), Pairs),
	length(Pairs, LenPairs),
	format("Lenght of pairs list: ~w\n", LenPairs),
	cross_point_list(Pairs, CrossPoints),
	length(CrossPoints, Len).

manhattan_distance(point(X,Y), Dist) :-
	Dist is abs(X) + abs(Y).

closest_cross_on_wires(W1, W2) :-
	coordinates_from_description(W1, Coords1),
	coordinates_from_description(W2, Coords2),
	find_crossing_points(Coords1,Coords2,CrossingWithOrigin),
	exclude([point(X,X)]>>(X #= 0), CrossingWithOrigin, Crossing),
	length(Crossing, LengthCrossing),
	format('Num crossings ~w~n', [LengthCrossing]),
	maplist(manhattan_distance, Crossing, Distances),
	min_list(Distances, Min),
	format('Done. Min is ~w', Min), nl.

test_data(Wire1, Wire2) :-
	Wire1 = ['U6','R12','D6', 'L6'],
	Wire2 = ['R4', 'U12','R4', 'D14'].

with_test_data :-
	test_data(W1,W2),
	closest_cross_on_wires(W1, W2).

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	maplist(string_to_op_list, Lines, Wires),
	[Wire1,Wire2] = Wires,
	%maplist([Input]>>format('~w,\n', Input), Wire1),
	closest_cross_on_wires(Wire1, Wire2).
