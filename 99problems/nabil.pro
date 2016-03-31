universal(F,[Name|Args]) :-
	var(F),
	length(Args,NumArgs),
	functor(F,Name,NumArgs),
	setargs(F,Args,1), !.
universal(F,[Name|Rest]) :-
	functor(F,Name,NumArgs),
	length(Rest,NumArgs),
	setargs(F,Rest,1), !.
setargs(_,[],_) :- !.
setargs(F,[A|As],Pos) :- arg(Pos,F,A),
	Pos1 is Pos + 1,
	setargs(F,As,Pos1).






divisores(N,Divs) :- integer(N), N>0,
					 getDivs(N,N,[],Divs).	

getDivs(1,_,L,[1|L]) :- !.
getDivs(M,N,L,D) :- R is mod(N,M), R = 0, !, M1 is M - 1, getDivs(M1,N,[M|L],D).
getDivs(M,N,L,D) :- M1 is M - 1, getDivs(M1,N,L,D).
