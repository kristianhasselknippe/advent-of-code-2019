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
	format('ArgModeList: ~w, NumArgs: ~w, ArgModes: ~w~n', [ArgModeList, NumArgs, ArgModes]),
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
	decode_opcode(Oc, _, InstructionLen),
	NumArgs is InstructionLen - 1,
	length(ArgModes, NumArgs),
	decode_arg_modes(Arguments, NumArgs, ArgModes),
	Opcode = opcode(Oc, Mode).

perform_operation(State, opcode(OpVal,OpMode), ArgModes, NextState, NextInstructionPointer) :-
	format('Opcode: (~w,~w), ArgModes: ~w~n', [OpVal, OpMode, ArgModes]).

perform_operation(State, InstructionPointer, NextState, NextInstructionPointer) :-
	nth0(InstructionPointer, State, Intcode),
	format('Int code: ~w~n', [Intcode]),
	decode_intcode(Intcode, Opcode, ArgModes),
	perform_operation(State, Opcode, ArgModes, NextState, NextInstructionPointer).

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(to_numbers_list, Lines, [InitialState]),
	perform_operation(InitialState, 0, NextState, NextInstructionPointer).
