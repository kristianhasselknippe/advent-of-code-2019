:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dialect/hprolog)).

if_(If_1, Then_0, Else_0) :-
   call(If_1, T),
   (  T == true -> call(Then_0)
   ;  T == false -> call(Else_0)
   ;  nonvar(T) -> throw(error(type_error(boolean,T),_))
   ;  /* var(T) */ throw(error(instantiation_error,_))
   ).

=(X, Y, T) :-
   (  X == Y -> T = true
   ;  X \= Y -> T = false
   ;  T = true, X = Y
   ;  T = false,
      dif(X, Y)                             % ISO extension
      % throw(error(instantiation_error,_)) % ISO strict
   ).

equal_t(X, Y, T) :-
   =(X, Y, T).

read_lines(Stream, []) :-
	at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Line),
	read_lines(Stream, Rest).

read_lines_from_file(File, Lines) :-
	open(File, read, Stream),
	read_lines(Stream, Lines).

chars_to_number(CharsList, Number) :-
	number_codes(CharsList, Number).

add_op(A,B,Sum) :-
	Sum is A + B.

mul_op(A,B,Sum) :-
	Sum is A * B.


opcode(1, add_op).
opcode(2, mul_op).
opcode(99, done_op).


value_at_pos(NumbersList, Pos, Value) :-
	nth0(Pos, NumbersList, Value).

apply_opcode_to_values(Code,Left,Right,Result) :-
	opcode(Code, Operation),
	call(Operation, Left, Right, Result).

to_numbers_list(String, NumbersList) :-
	split_string(String, ",", "", ListOfStrings),
	maplist(number_string, NumbersList, ListOfStrings).

replace_item_at_pos([_|T],0,E,[E|T]).
replace_item_at_pos([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replace_item_at_pos(T,NP,E,R).

compute_operation(I, LeftToProcess, Res) :-
	[99|_] = LeftToProcess,
	nth0(0, I, Res), !.
compute_operation(AllNumbers, LeftToProcess, FinalResult) :-
	length(LeftToProcess, X),
	[Code,LeftIndex,RightIndex,ResultIndex|_] = LeftToProcess,
	value_at_pos(AllNumbers, LeftIndex, Left),
	value_at_pos(AllNumbers, RightIndex, Right),
	apply_opcode_to_values(Code, Left, Right, Result),
	%write("Applying: "), write(Code), write(" to "), write(Left), write(" and "), write(Right), write(" with result: "), write(Result), nl,
	replace_item_at_pos(AllNumbers, ResultIndex, Result, UpdatedNumbers),
	nth0(ResultIndex, UpdatedNumbers, Res),
	length(AllNumbers, TotalLength),
	PlaceToSplit is TotalLength - X + 4,
	drop(PlaceToSplit, UpdatedNumbers, RestToCompute),
	compute_operation(UpdatedNumbers, RestToCompute, FinalResult).

compute(NumbersList, ArgA, ArgB, Result) :-
	replace_item_at_pos(NumbersList, 1, ArgA, A),
	replace_item_at_pos(A, 2, ArgB, FinalInput),
	%write("Final input: "), write(FinalInput), nl,
	compute_operation(FinalInput, FinalInput, Result).

get_input(Input) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, Numbers),
	nth0(0, Numbers, Input).

part1 :-
	get_input(Input),
	write(Input), nl,
	compute(Input, 12, 2, Out),
	write("Result part1: "), write(Out), nl.

search_for_result(Input, ValueToFind, A, B) :-
	compute(Input, A, B, Result),
	( Result = ValueToFind, write("A: "), write(A), write(" B: "), write(B), write(" Res: "), write(Result), nl, ! );
	( A > 0, A1 is A - 1, search_for_result(Input, ValueToFind, A1, B), ! );
	( A = 0, B1 is B - 1, search_for_result(Input, ValueToFind, 99, B1), ! );
	( A = 0, B = -1, !).

main(_) :-
	get_input(Input),
	write(Input), nl,
	search_for_result(Input, 19690720, 99, 99).
