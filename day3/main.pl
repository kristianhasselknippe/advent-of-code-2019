:- use_module(aoc).

string_to_op_list(String, OpList) :-
	split_string(String, ",", "", OpList).

write_with_comma(Input) :-
	write(Input), write(","), nl.

evaluate(Wire) :-
	maplist(write_with_comma, Wire).

main(_) :-
	read_lines_from_file("./input.txt", Lines),
	maplist(string_to_op_list, Lines, Wires),
	maplist(evaluate, Wires),
	write(OpLists).
