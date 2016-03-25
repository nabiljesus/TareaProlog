% Solución para Reorganización de Trenes
%
%
% @author López Javier
% @author Márquez
% Empezaré por aqui. (javier)


%%                 ___________:
%%                /
%% -- [a,b,c] ---+
%%                \___________:

%estado es de la forma [ [base], [above], [below] ]
%% push(_,0,,E,E):- !.

% Para encontrar las solucion es necesario un predicado dinamico para almacenar
% Estados ya visitados

vagones(InitialState,FinalState,Movements):-
    move_yaldra([InitialState,[],[]],
                [FinalState,[],[]],
                Movements).

move_yaldra(Final,Final,[]):- !.
move_yaldra(Actual,Final,[ M |Mvs]) :-
    M      =.. [Move,Dir,Size],
    Action =.. [Move,Dir,Size,Actual,NewActual],
    once(Action),
    print(NewActual),nl,
    move_yaldra(NewActual,Final,Mvs).

print_yaldra([Base,Above,Below],MaxWag) :-
    Size is MaxWag * 2-1+2+2+2+3+1,number_atom(Size,N),
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print(' __'), ( Above \= [] -> print(Above); true -> format("~7c",[95])),nl,
    atom_concat('~',N,X), atom_concat(X,'c',Z),format(Z,[32]),
    print(/), nl,
    format("-- ~p ---+",[Base]),nl,
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print('\__'), ( Below \= [] -> print(Below); true -> format("~7c",[95])),nl.

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
    
%% push(below,N,[[H|T],A,B],F) :-
%%     N1 is N-1, 
%%     push(above,N1,[T,A,[H|B]],F),!.

