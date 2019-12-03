:- module(aoc, [read_lines_from_file/2]).

read_lines(Stream, []) :-
	at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Line),
	read_lines(Stream, Rest).

read_lines_from_file(File, Lines) :-
	open(File, read, Stream),
	read_lines(Stream, Chars),
	maplist(string_chars, Lines, Chars).

chars_to_number(CharsList, Number) :-
	number_codes(CharsList, Number).
