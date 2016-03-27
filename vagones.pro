% Solución para Reorganización de Trenes
%
%
% @author López Javier
% @author Márquez

:- dynamic(visited/1).
%% visited(1).
%estado es de la forma [ [base], [above], [below] ]
%% push(_,0,,E,E):- !.

% Para encontrar las solucion es necesario un predicado dinamico para almacenar
% Estados ya visitados
% consult('vagones.pro'),vagones([a,c],[c,a],X).



vagones(InitialState,FinalState,Movements):-
    nonvar(Movements),
    move_yaldra([InitialState,[],[]],
                [FinalState,[],[]],
                Movements,yes),!.
vagones(InitialState,FinalState,Movements):-
    findall(Moves,
            find_vagones([InitialState,[],[]],
                        [FinalState,[],[]],
                        Moves),
            [First|AllMoves]),
    %% print([First|AllMoves]),
    length(First,Lf),
    smallest_list(First,Lf,AllMoves,Movements),
    vagones(InitialState,FinalState,Movements),
    retractall(visited(X)),!.

smallest_list(Smallest,_,[],Smallest).
smallest_list(Sm,Size,[Next|Tl],Ns):-
    length(Next,Nl),
    (
        Nl >= Size ->
            smallest_list(Sm,Size,Tl,Ns)
        ;
        smallest_list(Next,Nl,Tl,Ns)
    ),!.

steps(X,[_|T]) :-
    steps(1,X,T).
steps(N,N,_).
steps(N,M,[_|T]):-
    L is N+1,
    steps(L,M,T).

find_vagones(I,I,[]).
find_vagones(I,F,[M|Mv]):-
    \+ visited(I),
    assertz(visited(I)),
    create_case(I,M),
    move_yaldra(I,Actual,[M],no),
    find_vagones(Actual,F,Mv).

move_yaldra(Final,Final,[],yes):- print_yaldra(Final,3), !.
move_yaldra(Final,Final,[],_)  :- !.
move_yaldra(Actual,Final,[ M |Mvs],Print) :-
    M      =.. [Move,Dir,Size],
    Action =.. [Move,Dir,Size,Actual,NewActual],
    once(Action),
    ( Print = yes -> print_yaldra(Actual,3);true),
    move_yaldra(NewActual,Final,Mvs,Print),!.


% print_yaldra([[a,b],[c],[]],3).
print_yaldra([Base,Above,Below],MaxWag) :-
    Size is MaxWag * 2-1+2+2+2+3+1,number_atom(Size,N),
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print(' __ '), ( Above \= [] -> print(Above); true -> format("~7c",[95])),nl,
    atom_concat('~',N,X), atom_concat(X,'c',Z),format(Z,[32]),
    print(/), nl,
    format("-- ~p ---+",[Base]),nl,
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print('\\__ '), ( Below \= [] -> print(Below); true -> format("~7c",[95])),nl.

push(Dir,N,[Base,Above,Below],F) :-
    length(Movement,N),
    append(NewBase,Movement,Base),
    (   Dir = above,
        append(Movement,Above,NewAbove),
        F = [NewBase,NewAbove,Below]
        ;
        Dir = below,
        append(Movement,Below,NewBelow),
        F = [NewBase,Above,NewBelow]
    ),!. % Step tal vez sea inútil, no se

pop(above,N,[Base,Above,Below],F):-
    length(Movement,N),
    append(Movement,NewAbove,Above),
    append(Base,Movement,NewBase),
    F    = [NewBase,NewAbove,Below].

pop(below,N,[Base,Above,Below],F):-
    length(Movement,N),
    append(Movement,NewBelow,Below),
    append(Base,Movement,NewBase),
    F    = [NewBase,Above,NewBelow].
    

create_case([Base,Above,Below],Movement) :-
    member(Pos,[base,above,below]),
    (
        Pos = above -> 
            steps(N,Above),
            Movement = pop(above,N)
        ;
        Pos = below -> 
            steps(N,Below),
            Movement = pop(below,N)
        ;
        Pos = base ->
            member(Dir,[above,below]),
            steps(N,Base),
            Movement = push(Dir,N)
    ).