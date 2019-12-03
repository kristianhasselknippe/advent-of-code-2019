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

compute_operation(_, LeftToProcess) :-
	[99|_] = LeftToProcess,
	!.
compute_operation(AllNumbers, LeftToProcess) :-
	length(LeftToProcess, X),
	write("Items left to process: "), write(X), nl,
	write("Left to process "), write(LeftToProcess), nl,
	[Code,LeftIndex,RightIndex,ResultIndex|_] = LeftToProcess,
	value_at_pos(AllNumbers, LeftIndex, Left),
	value_at_pos(AllNumbers, RightIndex, Right),
	apply_opcode_to_values(Code, Left, Right, Result),
	write("Applying: "), write(Code), write(" to "), write(Left), write(" and "), write(Right), write(" with result: "), write(Result), nl,
	write("Result will be placed at: "), write(ResultIndex), nl,
	replace_item_at_pos(AllNumbers, ResultIndex, Result, UpdatedNumbers),
	nth0(ResultIndex, UpdatedNumbers, Res),
	write("Result in updated numbers is: "), write(Res), nl,
	write("Updated numbers: "), write(UpdatedNumbers), nl,
	length(AllNumbers, TotalLength),
	PlaceToSplit is TotalLength - X + 4,
	write("Place to split: "), write(PlaceToSplit), nl,
	drop(PlaceToSplit, UpdatedNumbers, RestToCompute),
	write("RestToCompute: "), write(RestToCompute),nl,nl,
	compute_operation(UpdatedNumbers, RestToCompute).

compute(NumbersList) :-
	replace_item_at_pos(NumbersList, 1, 12, A),
	replace_item_at_pos(A, 2, 2, FinalInput),
	write("Final input: "), write(FinalInput), nl,
	compute_operation(FinalInput, FinalInput).

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, Numbers),
	write(Numbers), nl,
	maplist(compute, Numbers).

%try 1: 394702
%try 2: 394702
