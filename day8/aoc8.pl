:- module(aoc8, [read_lines_from_file/2,chars_to_number/2,if_/3, number_digits/2, replace_item_at_pos/4, to_numbers_list/2]).

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
	maplist([Char, Digit]>>number_chars(Digit, [Char]), Chars, Digits).

:- begin_tests(aoc).

test(number_digits_test_1) :-
	number_digits(1110, [1,1,1,0]).


test(number_digits_test_2) :-
	number_digits(1110, [1,1,1,0]).

:- end_tests(aoc).

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

replace_item_at_pos([_|T],0,E,[E|T]).
replace_item_at_pos([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, replace_item_at_pos(T,NP,E,R).

to_numbers_list(String, NumbersList) :-
	split_string(String, ",", "", ListOfStrings),
	maplist(number_string, NumbersList, ListOfStrings).
