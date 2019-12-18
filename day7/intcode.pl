:- module(intcode, [run_program/2]).
:- use_module(aoc).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(mavis)).
:- use_module(library(dialect/hprolog)).

%! decode_opcode(++Code:int, -InstructionLength:int).
decode_opcode(1, 4). % add
decode_opcode(2, 4). % mul
decode_opcode(3, 2). % input
decode_opcode(4, 2). % output
decode_opcode(99, 1). %done

%Opcode 5 is jump-if-true: if the first parameter is non-zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
decode_opcode(5, 3).
%Opcode 6 is jump-if-false: if the first parameter is zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
decode_opcode(6, 3).
%Opcode 7 is less than: if the first parameter is less than the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
decode_opcode(7, 4).
%Opcode 8 is equals: if the first parameter is equal to the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
decode_opcode(8, 4).

opcode(_Op,_Mode).

range(A,B,Out) :-
	findall(X, between(A,B,X), Out).

% Gets the mode at the specified index of the arg mode list
get_mode(ArgModeList, Index, Out) :-
	length(ArgModeList, Len),
	Index < Len,
	nth0(Index, ArgModeList, Out),!.
get_mode(ArgModeList, Index, 0).

:- begin_tests(get_mode).
test('get mode base case 1') :-
	get_mode([1,1], 0, 1).
test('get mode base case 2') :-
	get_mode([1,0], 1, 0).
test('get mode outside len 1') :-
	get_mode([1,0], 2, 0).
test('get mode outside len 2') :-
	get_mode([1,0], 3, 0).
:- end_tests(get_mode).

% decode_arg_modes(++ArgModeList:list, ++NumArgs:integer, -ArgModes:list)
decode_arg_modes(ArgModeList, NumArgs, ArgModes) :-
	NumArgsOneLess is NumArgs - 1,
	range(0, NumArgsOneLess, Range),
	maplist({ArgModeList}/[Index,Mode]>>get_mode(ArgModeList, Index, Mode), Range, ArgModes).

%! arg_modes(+IntCode:integer, -Opcode:integer, -Arguments:list)
arg_modes(IntCode, Opcode, ArgModes) :-
	number_digits(IntCode, Digits),
	reverse(Digits,RevDigits),
	(
		([9,9|Arguments] = RevDigits, Opcode = 99);
		([Opcode, _ |Arguments] = RevDigits);
		([Opcode] = RevDigits, Arguments = [])
	),
	decode_opcode(Opcode, InstructionLen),
	NumArgs is InstructionLen - 1,
	length(ArgModes, NumArgs),
	decode_arg_modes(Arguments, NumArgs, ArgModes),!.

:- begin_tests(arg_modes).
test(intcode_decode_1) :-
	arg_modes(1101, 1, [1,1,0]).
test(intcode_decode_2) :-
	arg_modes(1, 1, [0,0,0]).
test(intcode_decode_3) :-
	arg_modes(3, 3, [0]).
test(intcode_decode_4) :-
	arg_modes(111, 1, [1,0,0]).
test(intcode_decode_4) :-
	arg_modes(99, 99, []).
:- end_tests(arg_modes).

% get_arg(++State:list, Arg:integer, Mode:integer, -Out) is det.
get_arg(State, Arg, 0, Out) :-
	nth0(Arg, State, Out),!.
get_arg(_, Arg, 1, Arg).
set_arg(State, Pos, Arg, NewState) :-
	replace_item_at_pos(State, Pos, Arg, NewState), !.

:- begin_tests(get_arg).
test('get arg 1') :-
	get_arg([1,2,3,4], 3, 0, 4).
:- end_tests(get_arg).

:- begin_tests(set_arg).
test('set arg 1') :-
	set_arg([1,2,3,4], 3, 123, [1,2,3,123]).
:- end_tests(set_arg).

% apply_op(++Op:integer, ++ArgModes:list, ++State:list, ++IP:integer, -NextState:list)
apply_op(Op, ArgModes, State, IP, NextState, NextIP) :-
	[AMode,BMode|_] = ArgModes,
	split_at(IP, State, _, [_,A,B,Res|_]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	((
		Op = 1,
		ResVal is AVal + BVal
	);
	(
		Op = 2,
		ResVal is AVal * BVal
	)),
	set_arg(State, Res, ResVal, NextState),
	NextIP is IP + 4.

% Read input and put at address of first arg
apply_op(3, ArgModes, State, IP, NextState, NextIP) :-
	split_at(IP, State, _, [H,Res|_]),
	read(UserInput),
	set_arg(State, Res, UserInput, NextState),
	NextIP is IP + 2.

% Write arg to output
apply_op(4, ArgModes, State, IP, State, NextIP) :-
	[InputMode] = ArgModes,
	split_at(IP, State, _, [H,Output|_]),
	get_arg(State, Output, InputMode, OutputVal),
	NextIP is IP + 2,
	format('Output: ~w~n', [OutputVal]).

%Opcode 5 is jump-if-true: if the first parameter is non-zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
apply_op(5, ArgModes, State, IP, State, NextIP) :-
	[PredMode, NewIPMode|_] = ArgModes,
	% TODO: Make a pred for this so its more clear what it does (getting current instruction args)
	split_at(IP, State, _, [H,Pred,NewInstructionPointer|_]),
	get_arg(State, Pred, PredMode, PredVal),
	format('5: Pred: ~w, PredVal ~w~n', [Pred, PredVal]),
	(
		PredVal \= 0 ->
		(format('    Jump if true was true'),nl,
		 get_arg(State, NewInstructionPointer, NewIPMode, NextIP));
		(NextIP is IP + 3)
	).

%Opcode 6 is jump-if-false: if the first parameter is zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
apply_op(6, ArgModes, State, IP, State, NextIP) :-
	[PredMode, NewIPMode|_] = ArgModes,
	% TODO: Make a pred for this so its more clear what it does (getting current instruction args)
	split_at(IP, State, _, [H,Pred,NewInstructionPointer|_]),
	get_arg(State, Pred, PredMode, PredVal),
	format('6: Pred: ~w, PredVal ~w~n', [Pred, PredVal]),
	(
		PredVal = 0 -> 
		(format('    Jump if false was true'),nl,
		 get_arg(State, NewInstructionPointer, NewIPMode, NextIP));
		(NextIP is IP + 3)
	).

%Opcode 7 is less than: if the first parameter is less than the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
apply_op(7, ArgModes, State, IP, NextState, NextIP) :-
	[AMode,BMode|_] = ArgModes,
	format('AMode: ~w, BMode: ~w~n', [AMode, BMode]),
	split_at(IP, State, _, [_, A,B,Res|_]),
	format('A ~w, B ~w, Res ~w~n', [A,B,Res]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	format('7: AVal: ~w, BVal: ~w~n', [AVal, BVal]),
	(
		AVal < BVal -> 
		(format('    Less than was true'),nl,
		 set_arg(State, Res, 1, NextState),
		 (NextIP is IP + 4));
		(format('    Less than was false'),nl,
		 set_arg(State, Res, 0, NextState),
		 (NextIP is IP + 4))
	).

%Opcode 8 is equals: if the first parameter is equal to the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
apply_op(8, ArgModes, State, IP, NextState, NextIP) :-
	[AMode,BMode|_] = ArgModes,
	split_at(IP, State, _, [_, A,B,Res|_]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	format('8: AVal: ~w, BVal: ~w~n', [AVal, BVal]),
	(
		AVal = BVal ->
		(format('    Equals than was true'),nl,
		 set_arg(State, Res, 1, NextState),
		 (NextIP is IP + 4));
		(format('    Equals than was true'),nl,
		 set_arg(State, Res, 0, NextState),
		 (NextIP is IP + 4))
	).

perform_operation_using_opcode(State, IP, OpVal, ArgModes, NextState, NextIP) :-
	decode_opcode(OpVal, OpSize),
	NumArgs is OpSize - 1,
	apply_op(OpVal, ArgModes, State, IP, NextState, NextIP).

perform_operation(State, InstructionPointer, NextState, NextInstructionPointer) :-
	nth0(InstructionPointer, State, Intcode),
	length(State,LL),
	ground(Intcode),
	arg_modes(Intcode, Opcode, ArgModes),
	decode_opcode(Opcode, InstructionLength),
	!,
	%format('Performing: ~w, ~w, ~w, ~w~n', [State, InstructionPointer, Opcode, ArgModes]),
	perform_operation_using_opcode(State, InstructionPointer, Opcode, ArgModes, NextState, NextInstructionPointer),!.


run_program_impl(program{state:State, input:_ProgInput, output:_ProgOutput}, IP, Output) :-
	nth0(IP, State, Inst),
	((Inst \= 99) ->
		(
			perform_operation(State, IP, NextState, NextInstructionPointer),
			run_program_impl(
				program{state:NextState, input: _ProgInput, output:_ProgOutput},
				NextInstructionPointer,
				Output
			)
		);(
			Output = State
		)).

error:has_type(number_list, X) :-
	the(list, X),
	maplist([I]>>(the(number, I)), X).

error:has_type(intcode_program, program{state:State, input:Input, output:Output}) :-
	the(number_list, State),
	the(number_list, Input),
	the(number_list, Output).

% run_program(++Program:intcode_program, Read, Write, --OutputState:number_list) is det.
run_program(Program, OutputState) :-
	run_program_impl(Program, 0, OutputState).


:- begin_tests(opcode_tests).
test(test_op_add_immediate) :-
	perform_operation([1101, 5, 6, 3], 0, [1101, 5, 6, 11], 4).

test(test_op_add_with_negative_immediate) :-
	perform_operation([1101, 5, -6, 3], 0, [1101, 5, -6, -1], 4).

test(test_op_add_position) :-
	perform_operation([0001, 3, 3, 3], 0, [1,3,3,6], 4).

test(test_op_mul_immediate) :-
	perform_operation([1102, 5, 5, 3], 0, [1102,5,5,25], 4).

test(test_op_mul_position_1) :-
	perform_operation([1102, 3, 3, 3], 0, [1102,3,3,9], 4).

test(test_more_1) :-
	run_program(program{state:[1102, 3, 3, 3, 1101, 20, 20, 0,99],input:[],output:X}, [40,3,3,9,1101,20,20,0,99]).

%test(user_input_1) :-
%	perform_operation([1103, 2, 0], 0, [1103, 2, 250], 2).

%test(test_op_3_immediate) :-
%	perform_operation([113, 3, 20, 3], 0, [113,3,20,20], 3).

:- end_tests(opcode_tests).
