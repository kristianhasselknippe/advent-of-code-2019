:- use_module(library(clpfd)).
:- use_module(library(dialect/hprolog)).
:- use_module(library(aggregate)).
:- use_module(aoc8).

num_numbers_per_layer(Out) :-
	Out is 25 * 6.

extract_items_matching(Elem, List, Out) :-
	findall(X, (member(X, List), X = Elem), Out).

num_items_matching(Elem, List, Out) :-
	extract_items_matching(Elem, List, ItemsMatching),
	length(ItemsMatching, Out).

layer(_Zeros, _Ones, _Twos).

analyze_layers([], Out, Out).
analyze_layers(Input, Acc, Out) :-
	num_numbers_per_layer(N),
	split_at(N, Input, Layer, Rest),
	num_items_matching(0, Layer, Num0),
	num_items_matching(1, Layer, Num1),
	num_items_matching(2, Layer, Num2),
	analyze_layers(Rest, [layer(Num0, Num1, Num2)|Acc], Out).

stack_layers([], Out, Out).
stack_layers([Layer|Rest], Acc, Out) :-
	%format('Layer: ~w, Acc: ~w, Out: ~w~n', [Layer, Acc, Out]),
	length(Rest, RestLen),
	length(Acc, AccLen),
	format('Length of rest: ~w, length of Acc ~w~n', [RestLen, AccLen]),
	format('Layer: ~w, Acc ~w~n', [Layer, Acc]),
	maplist([X,Y,O]>>(
				(X = 2 -> O = Y ; O = X)
				%format('X: ~w, Y: ~w, Out: ~w~n', [X,Y,O])
			), Acc, Layer, NewAcc),
	write('Done mapping'), nl,
	stack_layers(Rest, NewAcc, Out).

create_image([H|Rest], Out) :-
	stack_layers(Rest, H, Out).

init_array(0, _, Out, Out).
init_array(N, E, Acc, Out) :-
	N1 is N - 1,
	init_array(N1, E, [E|Acc], Out).
init_array(N, E, Out) :-
	init_array(N, E, [], Out).


layers_list([], Out, Out).
layers_list(Numbers, Acc, Out) :-
	split_at(150, Numbers, Layer, Rest),
	length(Rest, LenRest),
	%format('Layer ~w, rest ~w~n', [Layer, LenRest]),
	layers_list(Rest, [Layer|Acc], Out).
layers_list(Numbers, Out) :-
	layers_list(Numbers, [], Rev),
	reverse(Rev, Out).

main(_) :-
	read_lines_from_file('./input.txt', [Input]),
	number_string(Number, Input),
	number_digits(Number, NumbersList),
	layers_list(NumbersList, Layers),
	%write(Layers),
	create_image(Layers, FinalImage),
	write(FinalImage).

main0(_) :-
	read_lines_from_file('./input.txt', [Input]),
	number_string(Number, Input),
	number_digits(Number, NumbersList),
	analyze_layers(NumbersList, [], AnalyzedLayers),
	write(AnalyzedLayers), nl,
	foldl([A,B,R]>>(
			  format('A ~w~n', [A]),
			  format('B ~w~n', [B]),
			  layer(Z1,_,_) = A,
			  layer(Z2,_,_) = B,
			  M is min(Z1, Z2),
			  (
				  (
					  M #= Z1,
					  R = A
				  );
				  (
					  M #= Z2,
					  R = B
				  )
			  )
		  ), AnalyzedLayers, layer(9999,9999,9999), Out),
	write(Out), nl.
