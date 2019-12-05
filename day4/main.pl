%
%You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it o%ut.

%However, they do remember a few key facts about the passwo%rd:

%It is a six-digit number.
%The value is within the range given in your puzzle input.
%Two adjacent digits are the same (like 22 in 122345).
%Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
%Other than the range rule, the following are tr%ue:

%111111 meets these criteria (double 11, never decreases).
%223450 does not meet these criteria (decreasing pair of digits 50).
%123789 does not meet these criteria (no double).
%How many different passwords within the range given in your puzzle input meet these criteria?
%
%Your puzzle input is 137683-596253.
%
:- use_module(library(clpfd)).

is_six_digit(X) :-
	number(X),
	number_chars(X, Chars),
	length(Chars, Len),
	Len = 6.

within_range(Num) :-
	Num #>= 137683,
	Num #=< 596253.

string_number(A,B) :-
	number_string(B,A).

char_string(A,B) :-
	string_chars(B, [A]).


list_has_adjacent([A,A|_]).
list_has_adjacent([A|Rest]) :-
	write(A),
	list_has_adjacent(Rest).

number_digits(Num, Digits) :-
	number_chars(Num, Chars),
	maplist(char_string, Chars, Strings),
	maplist(string_number, Strings, Digits).

adjacent_digits(X) :-
	number(X),
	number_digits(X, Digits),
	list_has_adjacent(Digits).


never_decreasing([]).
never_decreasing([_]).
never_decreasing([A,B|Rest]) :-
	write(A), write(B), nl,
	A #=< B,
	write('foo'), nl,
	never_decreasing([B|Rest]).

test_never_decreasing(X) :-
	number_digits(X, Digits),
	never_decreasing(Digits).

satisfies(X) :-
	within_range(X),
	write('within range: '), write(X),
	adjacent_digits(X),
	write('adjacent'),
	never_decreasing(X).

