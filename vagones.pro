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
    find_vagones2([InitialState,[],[]],[FinalState,[],[]],
                  [[InitialState,[],[]]],
                  []),
    %length(First,Lf),
    %smallest_list(First,Lf,AllMoves,Movements),
    print('DADADA'),nl,
    print(AllMoves),nl,
    print('DADADADA'),nl.
    %retractall(visited(X)),!.

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

find_vagones2(Old,F,[F|Queue],_V).
find_vagones2(Old,F,[A|Queue],V):-
    member(A,V),
    find_vagones2(Old,F,Queue,V).
find_vagones2(Old,F,[A|Queue],V):-
    %% \+ visited(I),
    %% (
    %%     assertz(visited(I))
    %% ;
    %%     retract(visited(I))
    \+ member(A,V),
    %Busca las acciones posibles
    findall(Move,create_case(A,Move),Moves),
    %Busca los hijos posible
    move_yaldra2(A,YaldraMoves,Moves,yes),
    print('Si Paso'),nl,
    %Busca todas las posibles relaciones A (Padre) con Hijo para tener un arbol por el cual hallar el camino
    % de I a F luego.
    findall(father(A,CFather),member(CFather,YaldraMoves),FSList),
    add_parents(FSList),
    %Concatenaciones
    append(Queue,YaldraMoves,NQueue),
    find_vagones2(A,F,NQueue,[A|V]),
    print(A),nl,
    print(Moves),nl,
    print(YaldraMoves),nl,
    nl,print('Di end').

unshift([],E):- false.
unshift([L|LL],E):- L = E.

add_parents([]).
add_parents([Tup|Tups]):-
    assertz(Tup),
    add_parents(Tups).

%% move_yaldra(+Actual:list,+Final:list,+Mvs:list,?Print:atom)
%  
% Predicado que realiza los movimientos de la lista Mvs, sobre el 
% estado actual hasta llegar al estado Final. Los estados intermedios
% se imprimen si Print unifica con yes
%
% @param A       Estado Actual
% @param F       Estado Final después de los movimientos [Mv|Mvs]
% @param Print   Indica si el recorrido debe ser impreso

move_yaldra(Final,Final,[],yes):- print_yaldra(Final,3), !.
move_yaldra(Final,Final,[],_)  :- !.
move_yaldra(Actual,Final,[ M |Mvs],Print) :-
    M      =.. [Move,Dir,Size],
    Action =.. [Move,Dir,Size,Actual,NewActual],
    once(Action),
    ( Print = yes -> print_yaldra(Actual,3) ; true),
    move_yaldra(NewActual,Final,Mvs,Print),!.

move_yaldra2(Final,[],[],yes):- print_yaldra(Final,3), !.
move_yaldra2(Final,[],[],_)  :- !.
move_yaldra2(Actual,[NewActual|Final],[ M |Mvs],Print) :-
   print('TOy en yalda'),
    M      =.. [Move,Dir,Size],
    Action =.. [Move,Dir,Size,Actual,NewActual],
    once(Action),
    ( Print = yes -> print_yaldra(Actual,3) ; true),
    move_yaldra2(Actual,Final,Mvs,Print),!.

%% move_yaldra(+Estado:list,+MaxWag:int)
%  
% Predicado que imprime Estado en un formato espec`ifico, en donde
% MaxWag es el entero indicando la cantidad m`axima de elementos
% que puede haber en un vag`on cualquiera de Estado.
%
% @param Estado    Estado a imprimir
% @param MaxWag    Cantidad ma`xima de vagones

print_yaldra([Base,Above,Below],MaxWag) :-
    Size is MaxWag * 2 + 9 ,number_atom(Size,N), % -1+2+2+2+3+1
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print(' __ '), ( Above \= [] -> print(Above); true -> format("~7c",[95])),nl,
    atom_concat('~',N,X), atom_concat(X,'c',Z),format(Z,[32]),
    print(/), nl,
    format("-- ~p ---+",[Base]),nl,
    atom_concat('~',N,X),atom_concat(X,'c',Z),format(Z,[32]),
    print('\\__ '), ( Below \= [] -> print(Below); true -> format("~7c",[95])),nl.

% version ineficiente y fallida
print_yaldra([Base,Above,Below]) :-
    length(Base,BaseLen),
    length(Above,AboveLen),
    length(Below,BelowLen),
    MaxY     is max(AboveLen,BelowLen),
    AboveDif is (MaxY - AboveLen),
    BelowLen is (MaxY - BelowLen),
    Size     is BaseLen * 2 + 9 ,
    print_n(Size,32),
    print(' __'),
    ( 
        Above \= [] 
    ->
        print(1,32),
        print(Above), %*2 -1+2+2
        print(1,32)
    ;
        true
    ),print(AboveDif,95),print(':'),nl,
    print_n(Size,32),
    print(/), nl,
    format("-- ~p ---+",[Base]),nl,
    print_n(Size,32),
    print('\\__'), 
    ( 
        Below \= [] 
    -> 
        print(1,32),
        print(Below),
        print(1,32)
    ;   
        true
    ),print(AboveDif,95),print(':'),nl.

%print_n(0,_).
print_n(Int,Char) :-
    number_atom(Int,N), % -1+2+2+2+3+1
    atom_concat('~',N,X),
    atom_concat(X,'c',Z),
    format(Z,[Char]).

%% push(+Dir:atom,+N:int,+Init:list,?F:list)
%  
% Predicado que realiza el movimiento push. Se mueven N vagones 
% desde la base hasta Dir (above o below), pasando del estado 
% Init al F
%
% @param Dir     Dir hacia donde realizar el movimiento
% @param N       Cantidad de vagones a mover
% @param Init    Estado inicial
% @param F       Estado Final

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
    ),!. 

%% pop(+Dir:atom,+N:int,+Init:list,?F:list)
%  
% Predicado que realiza el movimiento pop. Se mueven N vagones 
% desde Dir (above o below) hasta Base, pasando del estado 
% Init al F
%
% @param Dir     Dir desde el cual se mover`an vagones
% @param N       Cantidad de vagones a mover
% @param Init    Estado inicial
% @param F       Estado Final

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
    

%% create_case(+Estado:list,?Movement:list)
%  
% Predicado que indica todos los posibles movimientos en Movement,
% que se pueden realizar desde Estado. Los resultados son entregados
% uno a uno por backtracking (generate).
%
% @param Estado     Estado de los vagones
% @param Movement   Posibles movimientos a realizar

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