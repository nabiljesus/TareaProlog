% Solución para Reorganización de Trenes
%
%
% @author López Javier
% @author Márquez

% Predicado para almacenar estados ya visitados
% Cada estado almacenado es de la forma [ [base], [above], [below] ]
:- dynamic(visited/1).


%% vagones(+InitialState:list,+FinalState:list,?Movements:list)
%  
% Predicado que demuestra como ir de InitialState a FinalState con la lista de
% movimientos Movements, realizando movimientos de los vagones,
% 
% @param InitialState  modela el estado inicial del vagon que se encuentra
%                      en la estación
% @param FinalState    modela el estafo final del tren en la estación
% @param Movements     secuencia de movimientos más corta para ir del estado
%                      inicial al final

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
    length(First,Lf),
    smallest_list(First,Lf,AllMoves,Movements),
    vagones(InitialState,FinalState,Movements),
    retractall(visited(X)),!.

%% smallest_list(+Small:list,+SmallSize:int,+Iterable:list,?Smallest:list)
%  
% Predicado que dada una lista de listas, su primer elemento y tamaño, entrega
% la más pequeña de todas las listas
% 
% @param Small        elemento más pequeño encontrado hasta el momento
% @param SmallSize    tamaño de small para evitar se recalculado en cada paso
% @param Iterable     lista de listas a recorrer
% @param Smallest     lista más pequeña de Iterable

smallest_list(Smallest,_,[],Smallest).
smallest_list(Sm,Size,[Next|Tl],Ns):-
    length(Next,Nl),
    (
        Nl >= Size 
    ->
        smallest_list(Sm,Size,Tl,Ns)
    ;
        smallest_list(Next,Nl,Tl,Ns)
    ),!.

%% steps(?Number:int,+List:list)
%  
% Predicado que dada una lista, provee por backtracking los números entre 1 y 
% el tamaño de la lista.
% 
% @param Number     número entre 1 y longitud(lista)
% @param List       lista cuyo tamaño servirá como cota superior

steps(X,[_|T]) :-
    steps(1,X,T).
steps(N,N,_).
steps(N,M,[_|T]):-
    L is N+1,
    steps(L,M,T).

%% find_vagones(+I:list,+F:list,?Mvs:list)
%  
% Predicado que visita el estado inicial I del problema de los vagones, 
% revisa los posibles movimientos del tren y pasa al estado F agregando el 
% movimiento realizado en Mvs. El predicado obtiene una demostración exitosa
% cuando los estados Inicial y Final son iguales.
% 
% Inicial y final son de la forma [ [Base], [Above], [Below] ]
%
% @param I       Estado Inicial
% @param F       Estado Final después del movimiento [Mv|Mvs]
% @param Mvs     Lista de movimientos realizados, el primer movimiento a la 
%                cabeza es el necesario para ir del estado I al F

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
    Size is MaxWag * 2 + 9 ,number_atom(Size,N), % -1+2+2+2+3+1
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
    (   
        Dir = above
    ->
        append(Movement,Above,NewAbove),
        F = [NewBase,NewAbove,Below]
    ;
        Dir = below
    ->
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
        Pos = above 
    -> 
        steps(N,Above),
        Movement = pop(above,N)
    ;
        Pos = below 
    -> 
        steps(N,Below),
        Movement = pop(below,N)
    ;
        Pos = base 
    ->
        member(Dir,[above,below]),
        steps(N,Base),
        Movement = push(Dir,N)
    ).