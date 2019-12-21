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
get_mode(_ArgModeList, _Index, 0).

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

% operation_arguments(++ProgramState:int_program_state, --Args:list) is det.
operation_arguments(ProgramState, Arg1) :-
	split_at(ProgramState.instructionPointer, ProgramState.state, _, [_,Arg1|_]).
operation_arguments(ProgramState, Arg1, Arg2) :-
	split_at(ProgramState.instructionPointer, ProgramState.state, _, [_,Arg1,Arg2|_]).
operation_arguments(ProgramState, Arg1, Arg2, Arg3) :-
	split_at(ProgramState.instructionPointer, ProgramState.state, _, [_,Arg1,Arg2,Arg3|_]).
operation_arguments(ProgramState, Arg1, Arg2, Arg3, Arg4) :-
	split_at(ProgramState.instructionPointer, ProgramState.state, _, [_,Arg1,Arg2,Arg3,Arg4|_]).

% apply_op(++Op:integer, ++ArgModes:list, ++State:list, ++IP:integer, -NextState:list)
apply_op(Op, ArgModes, ProgramState, NextProgramState) :-
	[AMode,BMode|_] = ArgModes,
	operation_arguments(ProgramState, A,B,Res),
	get_arg(ProgramState.state, A, AMode, AVal),
	get_arg(ProgramState.state, B, BMode, BVal),
	((
		Op = 1,
		ResVal is AVal + BVal
	);
	(
		Op = 2,
		ResVal is AVal * BVal
	)),
	set_arg(ProgramState.state, Res, ResVal, NextState),
	NextIP is ProgramState.instructionPointer + 4,
	NextProgramState = ProgramState.put(instructionPointer, NextIP).put(state, NextState).

% Read input and put at address of first arg
apply_op(3, _ArgModes, ProgramState, NextProgramState) :-
	operation_arguments(ProgramState, Res),
	[UserInput|InputLeft] = ProgramState.input,
	set_arg(ProgramState.state, Res, UserInput, NextState),
	NextIP is ProgramState.instructionPointer + 2,
	NextProgramState = ProgramState.put(state, NextState).put(input, InputLeft).put(instructionPointer, NextIP).

% Write arg to output
apply_op(4, ArgModes, ProgramState, NextProgramState) :-
	[InputMode] = ArgModes,
	operation_arguments(ProgramState, Output),
	get_arg(ProgramState.state, Output, InputMode, OutputVal),
	NextIP is ProgramState.instructionPointer + 2,
	append(State.output, [OutputVal], NextOutput),
	NextProgramState = State.put(output, NextOutput).put(instructionPointer, NextIP).

%Opcode 5 is jump-if-true: if the first parameter is non-zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
apply_op(5, ArgModes, ProgramState, NextProgramState) :-
	[PredMode, NewIPMode|_] = ArgModes,
	% TODO: Make a pred for this so its more clear what it does (getting current instruction args)
	operation_arguments(ProgramState, Pred, NewInstructionPointer),
	get_arg(ProgramState.state, Pred, PredMode, PredVal),
	(
		(PredVal \= 0 ->
		get_arg(ProgramState.state, NewInstructionPointer, NewIPMode, NextIP);
		NextIP is ProgramState.instructionPointer + 3),
		NextProgramState = ProgramState.put(instructionPointer, NextIP)
	).

%Opcode 6 is jump-if-false: if the first parameter is zero,
%it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
apply_op(6, ArgModes, ProgramState, NextProgramState) :-
	[PredMode, NewIPMode|_] = ArgModes,
	% TODO: Make a pred for this so its more clear what it does (getting current instruction args)
	operation_arguments(ProgramState, Pred, NewInstructionPointer),
	get_arg(ProgramState, Pred, PredMode, PredVal),
	(
		PredVal = 0 ->
		get_arg(ProgramState, NewInstructionPointer, NewIPMode, NextIP);
		NextIP is ProgramState.instructionPointer + 3
	),
	NextProgramState = ProgramState.put(instructionPointer, NextIP).

%Opcode 7 is less than: if the first parameter is less than the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
apply_op(7, ArgModes, ProgramState, NextProgramState) :-
	[AMode,BMode|_] = ArgModes,
	operation_arguments(ProgramState, A, B, Res),
	get_arg(ProgramState, A, AMode, AVal),
	get_arg(ProgramState, B, BMode, BVal),
	(
		AVal < BVal ->
		 (set_arg(ProgramState.state, Res, 1, NextState),
		  (NextIP is ProgramState.instructionPointer + 4));
		 (set_arg(ProgramState.state, Res, 0, NextState),
		 (NextIP is ProgramState.instructionPointer + 4))
	),
	NextProgramState = ProgramState.put(instructionPointer, NextIP).put(state, NextState).

%Opcode 8 is equals: if the first parameter is equal to the second parameter,
%it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
apply_op(8, ArgModes, ProgramState, NextProgramState) :-
	[AMode,BMode|_] = ArgModes,
	operation_arguments(ProgramState, A,B,Res),
	get_arg(ProgramState, A, AMode, AVal),
	get_arg(ProgramState, B, BMode, BVal),
	(
		AVal = BVal ->
		 (set_arg(ProgramState.state, Res, 1, NextState),
		 (NextIP is ProgramState.instructionPointer + 4));
		 (set_arg(ProgramState.state, Res, 0, NextState),
		 (NextIP is ProgramState.instructionPointer + 4))
	),
	NextProgramState = ProgramState.put(instructionPointer, NextIP).put(state, NextState).

perform_operation_using_opcode(ProgramState, OpVal, ArgModes, NextProgramState) :-
	apply_op(OpVal, ArgModes, ProgramState, NextProgramState).

perform_operation(ProgramState, NextProgramState) :-
	program{state:State,
			input:_,
			output:_,
			instructionPointer:InstructionPointer} = ProgramState,
	format('Performing operation on: ~w~n', [State]),
	nth0(InstructionPointer, State, Intcode),
	ground(Intcode),
	arg_modes(Intcode, Opcode, ArgModes),
	!,
	perform_operation_using_opcode(ProgramState, Opcode, ArgModes, NextProgramState),!.


run_program_impl(ProgramState, Output) :-
	program{state:State,
			input:_,
			output:_,
			instructionPointer:IP} = ProgramState,
	nth0(IP, State, Inst),
	((Inst \= 99) ->
		(
			perform_operation(ProgramState, NextProgramState),
			run_program_impl(
				NextProgramState,
				Output
			)
		);(
			Output = ProgramState
		)).

error:has_type(number_list, X) :-
	the(list, X),
	maplist([I]>>(the(number, I)), X).

error:has_type(intcode_program_state, program{state:State, input:Input, output:Output, instructionPointer:IP}) :-
	the(number_list, State),
	the(number_list, Input),
	the(number_list, Output),
	the(number, IP).

% run_program(++ProgramState:intcode_program_state, Read, Write, --OutputState:number_list) is det.
run_program(ProgramState, OutputState) :-
	run_program_impl(ProgramState, OutputState).

init_program_state(InitialState, Out) :-
	Out = program{state:InitialState,input:[],output:[],instructionPointer:0}.

:- begin_tests(opcode_tests).
test(test_op_add_immediate) :-
	init_program_state([1101, 5, 6, 3], Init),
	format('Init: ~w~n', [Init]),
	perform_operation(Init, program{state:[1101, 5, 6, 11],input:_,output:_,instructionPointer:4}).

test(test_op_add_with_negative_immediate) :-
	init_program_state([1101, 5, -6, 3], Init),
	perform_operation(Init, program{state:[1101, 5, -6, -1],input:_,output:_,instructionPointer:4}).

test(test_op_add_position) :-
	init_program_state([0001, 3, 3, 3], Init),
	perform_operation(Init, program{state:[1,3,3,6],input:_,output:_,instructionPointer:4}).

test(test_op_mul_immediate) :-
	init_program_state([1102, 5, 5, 3], Init),
	perform_operation(Init, program{state:[1102,5,5,25],input:_,output:_,instructionPointer:4}).

test(test_op_mul_position_1) :-
	init_program_state([1102, 3, 3, 3], Init),
	perform_operation(Init, program{state:[1102,3,3,9],input:_,output:_,instructionPointer:4}).

test(test_more_1) :-
	run_program(program{state:[1102, 3, 3, 3, 1101, 20, 20, 0,99],input:[],output:[],instructionPointer:0}, program{state:[40,3,3,9,1101,20,20,0,99],input:[],output:[],instructionPointer:8}).
%
test(user_input_1) :-
	perform_operation(program{state:[1103, 2, 0], input:[250],output:[],instructionPointer:0}, program{state:[1103, 2, 250],input:[],output:[],instructionPointer:2}).

%test(test_op_3_immediate) :-
%	perform_operation([113, 3, 20, 3], 0, [113,3,20,20], 3).

:- end_tests(opcode_tests).
