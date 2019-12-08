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
decode_opcode(3, 2). % write
decode_opcode(4, 2). % output
decode_opcode(99, 0). %done

opcode(_Op,_Mode).

range(A,B,Out) :-
	findall(X, between(A,B,X), Out).

% !decode_arg_modes(++ArgModeList:list<int>, ++NumArgs:int, -ArgModes:list<is_mode>).
decode_arg_modes(ArgModeList, NumArgs, ArgModes) :-
	format('ArgModeList: ~w, NumArgs: ~w~n', [ArgModeList, NumArgs]),
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
	format('Oc: ~w~n', Digits),
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

set_arg(State, Pos, Arg, position, NewState) :-
	format('Set arg (position): ~w, (a: ~w, p: ~w) ~n', [State, Arg, Pos]),
	replace_item_at_pos(State, Pos, Arg, NewState).

apply_op(opcode(1, OpMode), ArgModes, State, IP, NextState) :-
	%format('ARG MODES: ~w~n', [ArgModes]),
	[AMode,BMode,ResMode|_] = ArgModes,
	split_at(IP, State, _, [H|[A,B,Res]]),
	get_arg(State, A, AMode, AVal),
	get_arg(State, B, BMode, BVal),
	ResVal is AVal + BVal,
	format('ResVal ~w~n', [ResVal]),
	set_arg(State, Res, ResVal, ResMode, NextState),
	format('Done applying op\n').

perform_operation_using_opcode(State, IP, opcode(OpVal,OpMode), ArgModes, NextState) :-
	format('Opcode: (~w,~w), ArgModes: ~w~n', [OpVal, OpMode, ArgModes]),
	decode_opcode(OpVal, OpSize),
	NumArgs is OpSize - 1,
	format('Applying opcode (op val: ~w, op mode: ~w)~n', [OpVal, OpMode]),
	apply_op(opcode(OpVal, OpMode), ArgModes, State, IP, NextState),
	format('Next state: ~w~n', [NextState]).

perform_operation(State, InstructionPointer, NextState, NextInstructionPointer) :-
	nth0(InstructionPointer, State, Intcode),
	format('Int code: ~w~n', [Intcode]),
	decode_intcode(Intcode, Opcode, ArgModes),
	format('Decoded opcode: ~w~n', [Opcode]),
	opcode(OpcodeNum, _) = Opcode,
	decode_opcode(OpcodeNum, InstructionLength),
	perform_operation_using_opcode(State, InstructionPointer, Opcode, ArgModes, NextState),
	format('foo ~w, ~w~n', [InstructionPointer, InstructionLength]), nl,
	NextInstructionPointer is InstructionPointer + InstructionLength.

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, [InitialState]),
	perform_operation(InitialState, 0, NextState, NextInstructionPointer).

:- begin_tests(opcode_tests).
test(test_op_add_immediate) :-
	perform_operation([1101, 5, 6, 3], 0, [1101, 5, 6, 11], 4).
test(test_op_add_position) :-
	perform_operation([0001, 3, 3, 3], 0, [1,3,3,6], 4).
:- end_tests(opcode_tests).
