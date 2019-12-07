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
:- use_module(library(solution_sequences)).

is_six_digit(X) :-
	number(X),
	number_chars(X, Chars),
	length(Chars, Len),
	Len = 6.

string_number(A,B) :-
	number_string(B,A).

char_string(A,B) :-
	string_chars(B, [A]).


list_has_adjacent([A,A|_]).
list_has_adjacent([A|Rest]) :-
	list_has_adjacent(Rest).

number_digits(Num, Digits) :-
	number_chars(Num, Chars),
	maplist(char_string, Chars, Strings),
	maplist(string_number, Strings, Digits).

adjacent_digits(X) :-
	number(X),
	number_digits(X, Digits),
	list_has_adjacent(Digits).

write_type(X) :-
	number(X),
	format('~w is a number~n', X).
write_type(X) :-
	string(X),
	format('~w is a string~n', X).
write_type(X) :-
	atom(X),
	format('~w is an atom~n', X).
write_type(X) :-
	is_list(X),
	format('~w is an array~n', X).
write_type(X) :-
	format('~w is unknown type~n', X).


never_decreasing([_]).
never_decreasing([A,B|Rest]) :-
	%format('bar [~w, ~w|~w]\n', [A,B,Rest]),
	A #=< B,
	never_decreasing([B|Rest]).
test_never_decreasing(X) :-
	number(X),
	%format('X is now: ~w~n', [X]),
	number_digits(X, Digits),
	never_decreasing(Digits).
	%format('    did satisfy: ~w~n', [X]).

satisfies(X) :-
	adjacent_digits(X),
	%format('Satisfies adjacent: ~w~n', X),
	never_decreasing(X),
	write(X).


never_decreasing_test([], Out, Out).
never_decreasing_test([H|Rest], Num, Out) :-
	never_decreasing(H),
	Num is Num + 1.
never_decreasing_test(Numbers, Out) :-
	never_decreasing_test(Numbers, 0, Out).


test_number(X,range(From, To)) :-
	between(From,To,X),
	adjacent_digits(X),
	never_decreasing(X).

main(_) :-
	findall(X, (between(137683, 596253, X), adjacent_digits(X)), AllWithAdjacentWithSomeDuplicates),
	sort(AllWithAdjacentWithSomeDuplicates, AllWithAdjacent),
	length(AllWithAdjacent, AWL),
	format('All adjacent length ~w~n', [AWL]),
	findall(Y, (member(Y, AllWithAdjacent), test_never_decreasing(Y)), Output),
	length(Output, Len),
	format('Num members: ~w~n', Len).

test(X) :-
	adjacent_digits(X),
	never_decreasing(X).

test1 :-
	test(123789).

test2 :-
	test(111111).

test3 :-
	test(223450).
