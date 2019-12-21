:- use_module(aoc).
:- use_module(intcode).

test1 :-
	Input = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
	
.

main(_) :-
	read_lines_from_file('./input.txt', Lines),
	write(Lines),
	!.
