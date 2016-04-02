% Solución para Reorganización de Trenes
%
%
% @author López Javier 11-10552
% @author Márquez      11-10683

% Predicado para almacenar las relaciones padres e hijos y la accion asociada.
% De la forma [[ [base], [above], [below] ],acción]

:- dynamic(father/2).
father(inicial,inicial).

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
    retractall(padre(A,B)), %Limpio la memoria
    move_yaldra([InitialState,[],[]],
                [FinalState,[],[]],
                Movements,yes),!.
vagones(InitialState,FinalState,Movements):-
    bfs_vagones([FinalState,[],[]],
                  [[[inicial,none],[[InitialState,[],[]],exist]]],
                  []),
    % Hallando acción asociada al padre.
    father(UFather,[[FinalState,[],[]],Fact]),
    find_elder([[FinalState,[],[]],Fact],Path),
    % Acciones
    reverse(Path,PathR),
    % Impresión de las acciones
    print(PathR),nl,
    vagones(InitialState,FinalState,PathR),!.

%% find_elder(+Final:[State,Move],+Moves:list)
%  
% Predicado que dada una tupla de estado con su acción, 
% encuentra los antecesores de estos para imprimir el camino
% recorrido.
% 
% @param Final     Nodo buscado.
% @param Moves     Path recorrido desde el nodo inicial al nodo buscado.

find_elder([State,Move],[]):-
    father([inicial,none],[State,Move]).    
find_elder([State,Move],[Move|NextMove]):-
    father(Parent,[State,Move]),
    Parent \= inicial,
    find_elder(Parent,NextMove).

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

%% bfs_vagones(+F:State,+Queue:[[State,Action]],+V:[State])
%  
% Predicado que busca el estado Final F desencolando desde la pila,
% utilizando el reccorrido en arbol de BFS.
% 
%
% @param F        Estado Buscado (Final)
% @param Queue    Pila del BFS
% @param V        Lista de estados visitados

bfs_vagones(F,_Queue,[F|_V]):- !. %Al llegar al estado final, termina.
bfs_vagones(F,[[[Adad,AdAct],[A,Aact]]|Queue],V):-
    %Si está en visitados, backtracking.
    member(A,V),
    bfs_vagones(F,Queue,V).
bfs_vagones(F,[[[Adad,Adact],[A,Aact]]|Queue],V):-
    %Si no está en visitados
    \+ member(A,V),
    %Agrega el hecho padre-hijo a la base de datos
    assertz(father([Adad,Adact],[A,Aact])),
    %Busca las acciones posibles
    findall(Move,create_case(A,Move),Moves),
    %Busca los estados hijos posibles
    move_yaldra2(A,YaldraMoves,Moves,no),
    %Empaqueta en una lista con el formato [estado,acción]
    zip(YaldraMoves,Moves,FMoves),
    %Arma la lista de todas las tuplas padre-hijo
    findall([[A,Aact],CFather],member(CFather,FMoves),FSList),
    %Los Agrega a la pila y repite la busqueda
    append(Queue,FSList,NQueue),
    bfs_vagones(F,NQueue,[A|V]),!.

zip([],[],[]).
zip([X|Xs],[Y|Ys],[[X,Y]|Rest]):-
    zip(Xs,Ys,Rest).


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

%% move_yaldra2(+Actual:list,+Final:list,+Mvs:list,?Print:atom)
%  
% Analogamente a la anterior, solo aplica 1 movimiento de la lista
% Mv al estado pasado y devuelve los distintos estados resultantes.
%
% @param A       Estado Actual
% @param F       Estados Finales después de cada movimiento [Mv|Mvs]
% @param Mv      Lista de movimientos.
% @param Print   Indica si el recorrido debe ser impreso

move_yaldra2(Final,[],[],yes):- print_yaldra(Final,3), !.
move_yaldra2(Final,[],[],_)  :- !.
move_yaldra2(Actual,[NewActual|Final],[ M |Mvs],Print) :-
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