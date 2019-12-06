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

lines_from_write_description([],[], _).
lines_from_write_description([WH|WRest], [CH|CRest], LastOrigin) :-
	decode_command(WH, Command),
	apply_command(Command, LastOrigin, CH),
	line(_, NewOrigin) = CH,
	lines_from_write_description(WRest, CRest, NewOrigin).

lines_from_write_description(Wire, Coordinates) :-
	Origin = point(0,0),
	lines_from_write_description(Wire, Coordinates, Origin).

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

range(_A,_B).

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
range_overlap_impl(range(_,B), range(B,_), range(B,B)).
range_overlap_impl(range(A,C), range(B,C), range(B,C)) :-
	A < B.
range_overlap_impl(range(A1,A2), range(B1,B2), Out) :-
	B1 #>= A1,
	B1 #=< A2,
	(
		(
			B2 #=< A2,
			Out = range(B1,B2)
		);
		(
			A2 #=< B2,
			Out = range(B1, A2)
		)
	).

range_overlap(A,B,Out) :-
	sorted_range(A, ASorted),
	sorted_range(B, BSorted),
	sorted_on_start(ASorted,BSorted,L,R),
	range_overlap_impl(L,R,Out).


integers_in_range(R, IntList) :-
	sorted_range(R, range(A,B)),
	findall(X, between(A,B, X), IntList).

in_range(R, X) :-
	sorted_range(R, range(A,B)),
	X #>= A,
	X #=< B.

are_crossing([LA, LB], CrossPoints) :-
	%format('Testing pair: ~w and ~w~n', [LA,LB]),
	(
		are_perpendicular(
			LA,LB,
			line(point(HX1, HY), point(HX2, HY)),
			line(point(VX, VY1), point(VX, VY2))
		),
		%write('are perpendicular'), nl,
		%write(VX), write(' between '), write(HX1), write(' and '), write(HX2), nl,
		%write(HY), write(' between '), write(VY1), write(' and '), write(VY2), nl,
		in_range(range(HX1, HX2), VX),
		in_range(range(VY1, VY2), HY),
		CrossPoints = [point(VX, HY)]
		%write('Cross point: '), write(CrossPoints), nl
	);
	(
		is_horizontal(LA),
		is_horizontal(LB),
		line(point(XA1, Y), point(XA2, Y)) = LA,
		line(point(XB1, Y), point(XB2, Y)) = LB,
		range_overlap(range(XA1, XA2), range(XB1,XB2), Overlap),
		range(O1,O2) = Overlap,
		%format('Overlap between (~w,~w) and (~w,~w) => (~w,~w)~n', [XA1,XA2,XB1,XB2,O1,O2]),
		integers_in_range(Overlap, Ints),
		findall(point(X, Y), member(X, Ints), CrossPoints)
	);
	(
		is_vertical(LA),
		is_vertical(LB),
		line(point(X,YA1), point(X,YA2)) = LA,
		line(point(X,YB1), point(X,YB2)) = LB,
		range_overlap(range(YA1, YA2), range(YB1,YB2), Overlap),
		range(O1,O2) = Overlap,
		%format('Overlap between (~w,~w) and (~w,~w) => (~w,~w)~n', [YA1,YA2,YB1,YB2,O1,O2]),
		integers_in_range(Overlap, Ints),
		findall(point(X, Y), member(Y, Ints), CrossPoints)
	).

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
	%length(ListA, LALen),
	%length(ListB, LBLen),
	%format("Finding crossing, ListA Len ~w, ListB Len: ~w", [LALen, LBLen]),nl,
	%format("Finding crossing, ListA ~w, ListB: ~w", [ListA, ListB]),nl,
	findall([A,B], (member(A, ListA), member(B, ListB)), Pairs),
	%length(Pairs, LenPairs),
	%format('Length pairs: ~w~n', LenPairs),
	%format("Pairs: ~w\n", [Pairs]),
	cross_point_list(Pairs, CrossPoints).

manhattan_distance(point(X,Y), Dist) :-
	Dist is abs(X) + abs(Y).

closest_cross_on_wires(W1, W2) :-
	lines_from_write_description(W1, Coords1),
	lines_from_write_description(W2, Coords2),
	find_crossing_points(Coords1,Coords2,CrossingWithOrigin),
	exclude([point(X,X)]>>(X #= 0), CrossingWithOrigin, Crossing),
	length(Crossing, LengthCrossing),
	format('Num crossings ~w~n', [LengthCrossing]),
	format('Crossings ~w~n', [Crossing]),
	maplist(manhattan_distance, Crossing, Distances),
	min_list(Distances, Min),
	format('Done. Min is ~w', Min), nl.

%Example data
%R75,D30,R83,U83,L12,D49,R71,U7,L72
%U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
%R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
%U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

test_data(Wire1, Wire2) :-
	Wire1 = ['R98','U47','R26','D63','R33','U87','L62','D20','R33','U53','R51'],
	Wire2 = ['U98','R91','D20','R16','D67','R40','U7','R15','U6','R7'].

test_data3(Wire1, Wire2) :-
	Wire1 = ['R75','D30','R83','U83','L12','D49','R71','U7','L72'],
	Wire2 = ['U62','R66','U55','R34','D71','R55','D58','R83'].

test_data_2(Wire1, Wire2) :-
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
