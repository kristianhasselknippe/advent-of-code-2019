:- module(aoc, [read_lines_from_file/2,chars_to_number/2,if_/3,number_digits/2]).

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

number_digits(Number, Digits) :-
	number_chars(Number, Chars),
	maplist([Char,String]>>string_chars(Char, String), Chars, Strings),
	maplist([String, Number]>>number_string(Number, String), Strings, Digits).

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
