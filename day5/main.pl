:- use_module(aoc).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dialect/hprolog)).

is_mode(position).
is_mode(immediate).

decode_mode(0, position).
decode_mode(1, immediate).

%! decode_opcode(++Code:int, -InstructionLength:int).
decode_opcode(1, 4). % add
decode_opcode(2, 4). % mul
decode_opcode(3, 2). % input
decode_opcode(4, 2). % output
decode_opcode(99, 0). %done

opcode(_Op,_Mode).

range(A,B,Out) :-
	findall(X, between(A,B,X), Out).

% !decode_arg_modes(++ArgModeList:list<int>, ++NumArgs:int, -ArgModes:list<is_mode>).
decode_arg_modes(ArgModeList, NumArgs, ArgModes) :-
	%format('ArgModeList: ~w, NumArgs: ~w~n', [ArgModeList, NumArgs]),
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
decode_intcode(IntCode, Opcode, ArgModes) :-
	number_digits(IntCode, Digits),
	reverse(Digits,RevDigits),
	(
		[Oc, OpcodeMode |Arguments] = RevDigits;
		([Oc] = RevDigits, Arguments = [])
	),
	decode_mode(OpcodeMode, Mode),
	decode_opcode(Oc, InstructionLen),
	NumArgs is InstructionLen - 1,
	length(ArgModes, NumArgs),
	decode_arg_modes(Arguments, NumArgs, ArgModes),
	Opcode = opcode(Oc, Mode).

get_arg(State, Arg, position, Out) :-
	nth0(Arg, State, Out).
get_arg(State, Arg, immediate, Arg).

set_arg(State, Pos, Arg, NewState) :-
	%format('Set arg (position): ~w, (pos: ~w, arg: ~w), NS: ~w ~n', [State, Pos, Arg, NewState]),
	replace_item_at_pos(State, Pos, Arg, NewState).

apply_op(opcode(Op, OpMode), ArgModes, State, IP, NextState) :-
	(
		Op = 1,
		OpImpl = [A,B,Out]>>(Out is A + B)
	);
	(
		Op = 2,
		OpImpl = [A,B,Out]>>(Out is A * B)
	),
	[AMode,BMode|_] = ArgModes,
	split_at(IP, State, _, [H,A,B,Res|_]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	ResVal is AVal + BVal,
	set_arg(State, Res, ResVal, NextState).

apply_op(opcode(3, OpMode), ArgModes, State, IP, NextState) :-
	[InputMode|_] = ArgModes,
	split_at(IP, State, _, [H,Res|_]),
	read(UserInput),
	%format('Res ~w, Input ~w, mode: ~w~n', [Res, Input, InputMode]),
	set_arg(State, Res, UserInput, NextState).

apply_op(opcode(4, OpMode), ArgModes, State, IP, NextState) :-
	[InputMode] = ArgModes,
	split_at(IP, State, _, [H,Res,Input|_]),
	get_arg(State, Input, InputMode, InputVal),
	format('OUTPUT: ~w~n', [InputVal]),nl,
	set_arg(State, Res, InputVal, NextState).

perform_operation_using_opcode(State, IP, opcode(OpVal,OpMode), ArgModes, NextState) :-
	format('Opcode: (~w,~w), ArgModes: ~w~n', [OpVal, OpMode, ArgModes]),
	decode_opcode(OpVal, OpSize),
	NumArgs is OpSize - 1,
	%format('Applying opcode (op val: ~w, op mode: ~w)~n', [OpVal, OpMode]),
	apply_op(opcode(OpVal, OpMode), ArgModes, State, IP, NextState).
	%format('Next state: ~w~n', [NextState]).

perform_operation(State, InstructionPointer, NextState, NextInstructionPointer) :-
	format('Instruction pointer at: ~w~n', [InstructionPointer]),
	nth0(InstructionPointer, State, Intcode),
	%format('Int code: ~w~n', [Intcode]),
	decode_intcode(Intcode, Opcode, ArgModes),
	%format('Decoded opcode: ~w~n', [Opcode]),
	opcode(OpcodeNum, _) = Opcode,
	decode_opcode(OpcodeNum, InstructionLength),
	perform_operation_using_opcode(State, InstructionPointer, Opcode, ArgModes, NextState),
	NextInstructionPointer is InstructionPointer + InstructionLength,
	format('   next IP: ~w~n', [NextInstructionPointer]).

run_program_impl(State, IP, OutputState, Res) :-
	nth0(IP, State, Inst),
	format('Current state: ~w~n', [State]),
	format('Next instruction pointer: ~w~n', [IP]),
	format('Next instruction: ~w~n', [Inst]),
	(
		(
			Inst = 99,
			OutputState = Res
		);
		(
			perform_operation(State, IP, NextState, NextInstructionPointer),
			run_program_impl(NextState, NextInstructionPointer, NextState, Res)
		)
	).

run_program(State, OutputState) :-
	run_program_impl(State, 0, OutputState).

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, [InitialState]),
	run_program(InitialState, OutputState),
	format('Output state ~w~n', [OutputState]).

:- begin_tests(opcode_tests).

test(test_op_add_immediate) :-
	perform_operation([1101, 5, 6, 3], 0, [1101, 5, 6, 11], 4).

test(test_op_add_with_negative_immediate) :-
	perform_operation([1101, 5, -6, 3], 0, [1101, 5, 6, -2], 4).

test(test_op_add_position) :-
	perform_operation([0001, 3, 3, 3], 0, [1,3,3,6], 4).

test(test_op_mul_immediate) :-
	perform_operation([1101, 5, 5, 3], 0, [1,3,3,25], 4).

test(test_op_mul_position) :-
	perform_operation([1101, 3, 3, 3], 0, [1,3,3,9], 4).

test(test_op_3_immediate) :-
	perform_operation([113, 3, 20, 3], 0, [113,3,20,20], 3).

:- end_tests(opcode_tests).
