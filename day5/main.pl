:- use_module(aoc5).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dialect/hprolog)).

%! decode_opcode(++Code:int, -InstructionLength:int).
decode_opcode(1, 4). % add
decode_opcode(2, 4). % mul
decode_opcode(3, 2). % input
decode_opcode(4, 2). % output
decode_opcode(99, 0). %done

opcode(_Op,_Mode).

range(A,B,Out) :-
	findall(X, between(A,B,X), Out).

decide_mode(ArgModeList, Index, Out) :-
	length(ArgModeList, Len),
	Index < Len,
	nth0(Index, ArgModeList, Out).
decide_mode(ArgModeList, Index, 0).

% !decode_arg_modes(++ArgModeList:list<int>, ++NumArgs:int, -ArgModes:list<is_mode>).
decode_arg_modes(ArgModeList, NumArgs, ArgModes) :-
	NumArgsOneLess is NumArgs - 1,
	format('Argmodelist ~w~n', [ArgModeList]),
	range(0, NumArgsOneLess, Range),
	maplist({ArgModeList}/[Index,Mode]>>decide_mode(ArgModeList, Index, Mode), Range, ArgModes).

%! arg_modes(+IntCode:int, -Opcode:opcode, -Arguments:list).
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
	decode_arg_modes(Arguments, NumArgs, ArgModes).

get_arg(State, Arg, 0, Out) :-
	nth0(Arg, State, Out).
get_arg(_, Arg, 1, Arg).
set_arg(State, Pos, Arg, NewState) :-
	replace_item_at_pos(State, Pos, Arg, NewState),
	format('       Set arg (position): ~w, (pos: ~w, arg: ~w), NS: ~w ~n', [State, Pos, Arg, NewState]).

apply_op(Op, ArgModes, State, IP, NextState) :-
	must_be(list, ArgModes),
	[AMode,BMode|_] = ArgModes,
	split_at(IP, State, _, [_,A,B,Res|_]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	format('foo -- a: ~w, b: ~w~n', [AVal, BVal]),
	((
		Op = 1,
		ResVal is A + B
	);
	(
		Op = 2,
		ResVal is A * B
	)),
	format('res val: ~w~n', [ResVal]),
	format('   applying opcode: ~w with ~w and ~w =>(res:~w) res: ~w~n', [Op, A, B, Res, ResVal]),
	set_arg(State, Res, ResVal, NextState),
	format('        done~n').

apply_op(3, ArgModes, State, IP, NextState) :-
	must_be(list, ArgModes),
	[InputMode|_] = ArgModes,
	split_at(IP, State, _, [H,Res|_]),
	read(UserInput),
	%format('Res ~w, Input ~w, mode: ~w~n', [Res, Input, InputMode]),
	set_arg(State, Res, UserInput, NextState).

apply_op(4, ArgModes, State, IP, NextState) :-
	must_be(list, ArgModes),
	[InputMode] = ArgModes,
	split_at(IP, State, _, [H,Res,Input|_]),
	get_arg(State, Input, InputMode, InputVal),
	format('OUTPUT: ~w~n', [InputVal]),nl,
	set_arg(State, Res, InputVal, NextState).

perform_operation_using_opcode(State, IP, OpVal, ArgModes, NextState) :-
	format('Opcode: ~w, ArgModes: ~w~n', [OpVal, ArgModes]),
	decode_opcode(OpVal, OpSize),
	NumArgs is OpSize - 1,
	format('  Applying opcode (op val: ~w, op size: ~w, argModes: ~w)~n', [OpVal, OpSize, ArgModes]),
	apply_op(OpVal, ArgModes, State, IP, NextState),
	format("123NS ~w~n", [NextState]).
	%format('Next state: ~w~n', [NextState]).

perform_operation(State, InstructionPointer, NextState, NextInstructionPointer) :-
	format('Instruction pointer at: ~w~n', [InstructionPointer]),
	nth0(InstructionPointer, State, Intcode),
	format('Int code: ~w~n', [Intcode]),
	arg_modes(Intcode, Opcode, ArgModes),
	format('Decoded opcode: ~w for intcode: ~w~n', [ArgModes, Opcode]),
	decode_opcode(Opcode, InstructionLength),
	format('    opcode len: ~w~n', [InstructionLength]),
	perform_operation_using_opcode(State, InstructionPointer, Opcode, ArgModes, NextState),
	NextInstructionPointer is InstructionPointer + InstructionLength.
	%format('   next IP: ~w~n', [NextInstructionPointer]).

run_program_impl(State, IP, Res, Res) :-
	nth0(IP, State, 99).
run_program_impl(State, IP, Prev, Output) :-
	nth0(IP, State, Inst),
	%format('Current state: ~w~n', [State]),
	%format('Next instruction pointer: ~w~n', [IP]),
	%format('Next instruction: ~w~n', [Inst]),
	perform_operation(State, IP, NextState, NextInstructionPointer),
	run_program_impl(NextState, NextInstructionPointer, Prev, Output).

run_program(State, OutputState) :-
	run_program_impl(State, 0, State, OutputState).

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, [InitialState]),
	run_program(InitialState, OutputState),
	format('Output state ~w~n', [OutputState]).

test_perform_operation(State, IP, NIP, NS) :-
	perform_operation(State, IP, NIP, NS),
	format('NewState: ~w, NewIP: ~w~n', [NIP, NS]).

:- begin_tests(opcode_tests).

test(intcode_decode_1) :-
	arg_modes(1101, 1, [1,1,0]).

test(test_op_add_immediate) :-
	test_perform_operation([1101, 5, 6, 3], 0, [1101, 5, 6, 11], 4).

test(test_op_add_with_negative_immediate) :-
	test_perform_operation([1101, 5, -6, 3], 0, [1101, 5, -6, -1], 4).

test(test_op_add_position) :-
	test_perform_operation([0001, 3, 3, 3], 0, [1,3,3,6], 4).

test(test_op_mul_immediate) :-
	test_perform_operation([1102, 5, 5, 3], 0, [1102,5,5,25], 4).

test(test_op_mul_position) :-
	test_perform_operation([1102, 3, 3, 3], 0, [1102,3,3,9], 4).

%test(test_op_3_immediate) :-
%	test_perform_operation([113, 3, 20, 3], 0, [113,3,20,20], 3).

:- end_tests(opcode_tests).
