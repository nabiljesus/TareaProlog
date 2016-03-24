% consult('javier.pro').
% 1
%% ?- my_last(X,[a,b,c,d]).
%% X = d

my_last(X,[X]).
my_last(X,[_|T]) :- my_last(X,T), ! .

% 2
last_but_one(X,[X,_]).
last_but_one(X,[_|T]) :- last_but_one(X,T),!.

% 3 nth element
%% ?- element_at(X,[a,b,c,d,e],3).
%% X = c

element_at( H ,[H|_],1).
element_at( X ,[_|T],N) :- N1 is N-1, element_at(X,T,N1),!.

% 4 my length
my_length([],0).
my_length([_|L],N1) :- my_length(L,N), N1 is N+1.

% 5 reverse a list
%% my_reverse([],[]).
my_reverse(L2,L) :- my_reverse_aux(L2,[],L).

my_reverse_aux([],L,L).
my_reverse_aux([H|T],L,L2) :- my_reverse_aux(T,[H|L],L2).