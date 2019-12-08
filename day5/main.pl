:- use_module(aoc).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dialect/hprolog)).

is_mode(position).
is_mode(immediate).

decode_mode(0, position).
decode_mode(1, immediate).

%! decode_opcode(++Code:int, -Opcode:atom, -InstructionLength:int).
decode_opcode(1, add_op, 4).
decode_opcode(2, mul_op, 4).
decode_opcode(3, store_op, 2).
decode_opcode(4, write_op, 2).
decode_opcode(99, done_op, 0).

opcode(_Op,_Mode).

range(A,B,Out) :-
	findall(X, between(A,B,X), Out).

% !decode_arg_modes(++ArgModeList:list<int>, ++NumArgs:int, -ArgModes:list<is_mode>).
decode_arg_modes(ArgModeList, NumArgs, ArgModes) :-
	NumArgsOneLess is NumArgs - 1,
	range(0, NumArgsOneLess, Range),
	maplist(
		{ArgModeList, NumArgs}/[Index,Mode]>>(
			(
				nth0(Index, ArgModeList, ModeNum);
				ModeNum = 0
			),
			decode_mode(ModeNum, Mode)
		),
		Range,
		ArgModes
	).

%! decode_intcode(+IntCode:int, -Opcode:opcode, -Arguments:list).
decode_intcode(IntCode, Opcode, ArgumentsModes) :-
	number_digits(IntCode, Digits),
	reverse(Digits,RevDigits),
	[Oc, OpcodeMode |Arguments] = RevDigits,
	decode_mode(OpcodeMode, Mode),
	decode_opcode(Oc, _, InstructionLen),
	NumArgs is InstructionLen - 1,
	length(ArgumentsModes, NumArgs),
	Opcode = opcode(Oc, Mode).

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
