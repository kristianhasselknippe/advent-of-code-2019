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
	maplist([X,Y,O]>>(
				(X #= 0 -> O = Y ; O = X),
				format('X: ~w, Y: ~w, Out: ~w~n', [X,Y,O])
			), Layer, Acc, NewAcc),
	write('Done mapping'), nl,
	stack_layers(Rest, NewAcc, Out).

create_image(Layers, Out) :-
	zeroes_array(150, InitialImage),
	length(Layers, LayersLen),
	format('Layers length: ~w~n', [LayersLen]),
	stack_layers(Layers, InitialImage, Out).

zeroes_array(0, Out, Out).
zeroes_array(N, Acc, Out) :-
	N1 is N - 1,
	zeroes_array(N1, [0|Acc], Out).
zeroes_array(N, Out) :-
	zeroes_array(N, [], Out).


layers_list([], Out, Out).
layers_list(Numbers, Acc, Out) :-
	split_at(150, Numbers, Layer, Rest),
	length(Rest, LenRest),
	format('Layer ~w, rest ~w~n', [Layer, LenRest]),
	layers_list(Rest, [Acc|Layer], Out).
layers_list(Numbers, Out) :-
	layers_list(Numbers, [], Out).

main(_) :-
	read_lines_from_file('./input.txt', [Input]),
	number_string(Number, Input),
	number_digits(Number, NumbersList),
	layers_list(NumbersList, Layers),
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
