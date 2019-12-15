:- use_module(main).
% define helper predicates here

:- use_module(library(tap)).
% define test predicates here

'two plus two is four' :-
    4 is 2+2.

'zero not equal to one'(fail) :-
    0 =:= 1.
