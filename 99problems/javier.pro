% consult('javier.pro').
% 1
%% ?- my_last(X,[a,b,c,d]).
%% X = d

my_last(X,[X]).
my_last(X,[_|T]) :- my_last(X,T), ! .

% 2
%% last_but_one(X,[X,_]).
%% last_but_one(X,[_|T]) :- last_but_one(X,T),!.

% 3 nth element
%% ?- element_at(X,[a,b,c,d,e],3).
%% X = c

%% element_at( H ,[H|_],1).
%% element_at( X ,[_|T],N) :- N1 is N-1, element_at(X,T,N1),!.

% 4 my length
%% my_length([],0).
%% my_length([_|L],N1) :- my_length(L,N), N1 is N+1.

% 5 reverse a list
%% my_reverse([],[]).
%% my_reverse(L2,L) :- my_reverse_aux(L2,[],L).

%% my_reverse_aux([],L,L).
%% my_reverse_aux([H|T],L,L2) :- my_reverse_aux(T,[H|L],L2).

%% divisores(N,L) :- calc_divisores(N,1,L).
%% calc_divisores(N,N,[N]).
%% calc_divisores(N,C,[H|T]):- N > C,
%%                             C1 is C +1,
%%                             ( 0 =:= N mod C -> 
%%                                 H = C,
%%                                 calc_divisores(N,C1,T)
%%                               ;
%%                               calc_divisores(N,C1,[H|T])),!.

%% natural(1).
%% natural(N):- natural(N1), N is N1 +1.

%% perfecto(N):-
%%     nonvar(N),
%%     divisores(N,LDiv),
%%     sum_list(LDiv,Sum),
%%     N =:= Sum - N,!.

%% perfecto(N):-
%%     natural(N1),
%%     perfecto(N1),
%%     N = N1.

%% universal(F,[Name|T]):-
%%     nonvar(F),
%%     functor(F,Name,NArgs),
%%     length(T,NArgs),
%%     fill(F,T,1,NArgs),!.
%% universal(F,[Name|Args]):-
%%     var(F),
%%     length(Args,NArgs),
%%     functor(F,Name,NArgs),
%%     fill(F,Args,1,NArgs),!.

%% fill(_,[],N,Max):- N =:= Max+1.
%% fill(F,[H|Tl],N,Max):- 
%%     arg(N,F,H),
%%     N1 is N+1,
%%     fill(F,Tl,N1,Max).



%% get_argumento(1,[Res|_],Res):- ! .
%% get_argumento(N,[_|T],Res):-
%%     N1 is N-1,
%%     get_argumento(N1,T,Res).

%% longitud([],0),!.
%% longitud([_|T],N) :- longitud(T,N1), N is N1+1,!.

%% underScoreList(0,[]).
%% underScoreList(N,[_|T]):-
%%     N1 is N-1,
%%     underScoreList(N1,T).

%% argumento(N,F,Val) :-
%%     F =.. [Name|Args],
%%     get_argumento(N,Args,Val),!.

%% estructura(Functor,Name,Nargs):- 
%%     nonvar(Functor),
%%     Functor =.. [Name|Tail],
%%     longitud(Tail,Nargs),!.
%% estructura(Functor,Name,NArgs):-
%%     var(Functor),
%%     underScoreList(NArgs,L),
%%     Functor =.. [Name|L],!.

esrever(Lista,Atsil):-
    esrever(Lista,[],Atsil).
esrever([],Atsil,Atsil):- !.
esrever([X|Xs],L,Atsil):-
    esrever(Xs,[X|L],Atsil).

split(L,P1,P2):-
    split_helper(L,left,P1,P2),!.

split_helper([],_,[],[]).
split_helper([X|Xs],left,[X|P1],P2):-
    split_helper(Xs,right,P1,P2).
split_helper([X|Xs],right,P1,[X|P2]):-
    split_helper(Xs,left,P1,P2).

msort([],[]):- !.
msort([X],[X]):- !.
msort(L,Ordenada):-
    split(L,L1,L2),
    msort(L1,L1Ordenada),
    msort(L2,L2Ordenada),
    merge(L1Ordenada,L2Ordenada,Ordenada),!.

merge([],[],[]).
merge([],L2,L2).
merge(L1,[],L1).
merge([X|L1],[X|L2],[X,X|L3]):-
    merge(L1,L2,L3).
merge([X|L1],[Y|L2],[Y|L3]):-
    X > Y,
    merge([X|L1],L2,L3).
merge([X|L1],[Y|L2],[X|L3]):-
    Y > X,
    merge(L1,[Y|L2],L3).

slice([X|_],1,[X]).
slice([X|Xs],F,[X|Slice]) :-
    F1 is F-1,
    slice(Xs,F1,Slice).

slice(Source,1,F,Slice):-
    length(Slice,F),
    slice(Source,F,Slice),!.
    %% append(Slice,_,Source),!.
slice([_|Xs],I,F,L):-
    I2 is I-1,
    F2 is F-1,
    slice(Xs,I2,F2,L).

