% Solución para Calendario NFL
%
%
% @author López Javier
% @author Márquez

% American Football Conference standings
standings(afc,east,1,patriots).
standings(afc,east,2,jets).
standings(afc,east,3,bills).
standings(afc,east,4,dolphins).

standings(afc,north,1,bengals).
standings(afc,north,2,steelers).
standings(afc,north,3,ravens).
standings(afc,north,4,browns).

standings(afc,south,1,texans).
standings(afc,south,2,colts).
standings(afc,south,3,jaguars).
standings(afc,south,4,titans).

standings(afc,west,1,broncos).
standings(afc,west,2,chiefs).
standings(afc,west,3,raiders).
standings(afc,west,4,chargers).


% National Football Conference standings
standings(nfc,east,1,redskins).
standings(nfc,east,2,eagles).
standings(nfc,east,3,giants).
standings(nfc,east,4,cowboys).

standings(nfc,north,1,vikings).
standings(nfc,north,2,packers).
standings(nfc,north,3,lions).
standings(nfc,north,4,bears).

standings(nfc,west,1,cardinals).
standings(nfc,west,2,seahawks).
standings(nfc,west,3,rams).
standings(nfc,west,4,'49ers').

standings(nfc,south,1,panthers).
standings(nfc,south,2,falcons).
standings(nfc,south,3,saints).
standings(nfc,south,4,buccaneers).

% Intra-conference
intra(north,east).
intra(south,west).

% Inter-conference
inter(east,west).
inter(north,east).
inter(south,north).
inter(west,south).

%% divisional(+Home:atom,?Games:list)
%  
% Predicado que, dado un equipo muestra todos los partidos de division que 
% deben ser jugados por backtracking
% 
% @param Home        atomo que indica cual es equipo a consultar
% @param Games       lista con Lista de juegos de la forma [[H,V],[V,H]]

divisional(Home,[[Home,Visitor],[Visitor,Home]]) :-
    standings(Conf,Area,_,Home),
    standings(Conf,Area,_,Visitor),
    Home \= Visitor.


%% intragames(+T:atom,?Games:list)
%  
% Predicado que, dado un equipo muestra todos los juegos intraconferencia
% que deben ser jugados. El backtraking muestra las diferentes formas de 
% seleccionar todos esos juegos
% 
% @param T        atomo que indica cual es equipo a consultar
% @param Games    lista de juegos de la forma [Tm1,Tm2]

intragames(T,L):-
    standings(Conf,Card,_,T),
    (
        intra(Card,OpCard)
    ; 
        intra(OpCard,Card)
    ),

    findall(OpTeam ,standings(Conf,OpCard,_,OpTeam) ,Oponents),!,

    select(Home1,Oponents,Oponents1), 
    select(Home2,Oponents1,Oponents2),
    select(Visitor1,Oponents2,[Visitor2]),
    % Evitamos permutaciones entre partidos 1,2 y 3,4
    Home1    @> Home2,
    Visitor1 @> Visitor2,

    L = [[T,Visitor1],[T,Visitor2],[Home1,T],[Home2,T]].

%% intergames(+T:atom,?Games:list)
%  
% Predicado que, dado un equipo muestra todos los juegos interconferencia
% que deben ser jugados. El backtraking muestra las diferentes formas de 
% seleccionar todos esos juegos
% 
% @param T        atomo que indica cual es equipo a consultar
% @param Games    lista de juegos de la forma [Tm1,Tm2]

intergames(T,L):-
    standings(Conf,TArea,_,T),
    (
        Conf = afc
    ->
        OConf = nfc,
        inter(TArea,OArea)
    ;
        Conf = nfc
    ->
        OConf = afc,
        inter(TArea,OArea)
    ),
    findall(OpTeam ,standings(OConf,OArea,_,OpTeam) ,Oponents),!,

    select(Home1,Oponents,Oponents1), 
    select(Home2,Oponents1,Oponents2),
    select(Visitor1,Oponents2,[Visitor2]),
    
    Home1    @> Home2,
    Visitor1 @> Visitor2,

    L = [[T,Visitor1],[T,Visitor2],[Home1,T],[Home2,T]].

%% intragames(+T:atom,?Games:list)
%  
% Predicado que, dado un equipo muestra todas las formas de completar los dos 
% juegos faltantes, los cuales deben ser 1 como visitante, y como local, debe
% ser de  la misma  conferencia, con  equipos que se encuentren  en la  misma
% posicion y que no sean parte de juegos intraconferencia o divisionales
% 
% @param T           atomo que indica cual es equipo a consultar
% @param Games       lista de juegos de la forma [Tm1,Tm2]

finalgames(T,L):-
    standings(Conf,Cord,Position,T),
    (
        intra(Cord,OpCord)
    ; 
        intra(OpCord,Cord)
    ),

    findall(OpTeam ,standings(Conf,_,Position,OpTeam) ,Oponents),!,

    select(Home,Oponents,Oponents1),
    standings(_,Cord1,_,Home), 
    Cord   \= Cord1,
    OpCord \= Cord1,
    select(Visitor,Oponents1,_),
    standings(_,Cord2,_,Visitor),
    Cord   \= Cord2,
    OpCord \= Cord2,

    L = [[T,Visitor],[Home,T]].

%% intragames(+Teams:atom,?Acc:list,?Result:list)
%  
% Predicado que recibe una lista de equipos, los acumula en grupos de 4 en 
% Acc y triunfa cuando en Result se encuentran los equipos dividos en listas
% del tramaño mencionado. Cada lista se almacena de forma ordenada y sin 
% repeticiones.
% 
% @param Teams        lista con todos los equipos a los cuales se les aplicará
%                     la partición
% @param Acc          argumento auxiliar acumulador
% @param Result       particiones de tamaño 4 de los equipos

select_n([],[],[]).
select_n(Teams,[A,B,C,D],[[A,B,C,D]|All]):-
    select_n(Teams,[],All).  
select_n(Teams,L,All):- 
    select(Team,Teams,RestOfTeams),
    (
        L = [Last|_]
    ->
        Team @> Last
    ;
        true
    ),
    select_n(RestOfTeams,[Team|L],All).

%% intragames(?Result:list,+Teams:list)
%  
% Indica todas las disposiciones de equipos  que descansaran en las primeras 8 
% semanas. Generando cambios por backtraking desde el inicio de la lista hasta
% el final 
% 
% @param Result      lista con permutaciones de descanso para los 32 equipos.
%                    8 listas de tamaño 4
% @param Teams       lista de todos los equipos

byes(Result,Teams) :- 
    findall(Team,standings(_,_,_,Team),Teams), !,
    select_n(Teams,[],Rests),
    reverse(Rests,Result).

%% byes_print(+Teams:list)
%  
% Impresión de todos los equipos que descansarán en una semana usando el formato
% indicado en el enunciado
% 
% @param Teams   lista con los equipos que descansan

byes_print([Team])          :- print(Team),nl.
byes_print([Team|Teams])    :- format('~p, ',[Team]), byes_print(Teams).

%% week_print(+Matches:list)
%  
% Impresión de todos los juegos de la lista Matches, uno por línea
% 
% @param Matches   lista de juegos de la forma [T1,T2]

week_print([]).
week_print([Match|Matches]) :- match_print(Match),nl, week_print(Matches).

%% match_print(+Match:list)
%  
% Impresión de un partido usando el formato Visitor at Home
% 
% @param Match  lista de dos átomos de la forma [Home,Visitor]

match_print([Home,Visitor]) :- format('~p at ~p',[Visitor,Home]).

%% make_structure(?Struct:list)
%  
% Predicado que triunfa si Struct tiene la estructura de un calendario de
% juego (lista de 17 )
% 
% @param Struct  lista heterogénea de tamaño 17  donde los primeros 8 elementos 
%                son listas de tamaño 14 y las restantes de longitud 16

make_structure(Struct):-
    length(Struct,17),
    make_structure(Struct,1).
make_structure([],18):- !.  
make_structure([WithByes|Rest],N):-
    N < 9,
    length(WithByes,14),
    N1 is N+1,
    make_structure(Rest,N1),!.
make_structure([WithoutByes|Rest],N):-
    8 < N,   
    N < 18,
    length(WithoutByes,16),
    N1 is N+1,
    make_structure(Rest,N1),!.

%% make_structure/0
%  
% Predicado que triunfa cuando encuentra un calendario de juego válido y 
% lo imprime en el formato indicado en el enunciado.

schedule :- 
    findall(Team,standings(_,_,_,Team),Teams),!,
    byes(B,Teams),
    make_structure(Matches),
    assign_random_matches(Matches,Teams,[]),
    check_matches(Matches),
    check_byes(Matches,B),
    schedule(1,Matches,B).

%% assign_random_matches(+Calendar:list,+Teams:list,+Visited:list)
%  
% Predicado que triunfa cuando todos los juegos de Calendar unifican con 
% algun partidos, estos partidos no se repiten y deben estar conpuestos de
% dos equipos diferentes. 
%
% @param   Calendar     estructura de un calendario
% @param   Teams        lista con todos los equipos
% @param   Visted       lista con los partido ya agregados


assign_random_matches([],Teams,Visited).
assign_random_matches([[]|Weeks],Teams,Visited) :-
    assign_random_matches(Weeks,Teams,Visited).
assign_random_matches([[Match1|RestOfWeek]|Weeks],Teams,Visited) :-
    member(Home,Teams),
    member(Visitor,Teams),
    Home \= Visitor,
    \+ member([Home,Visitor],Visited),
    Match1 = [Home,Visitor],
    assign_random_matches([RestOfWeek|Weeks],Teams,[Match1|Visited]).



check_byes([],[]).
check_byes([M|Ms],[B|Bs]):-
    sleepers_will_sleep(M,B).

sleepers_will_sleep([],[Tm1,Tm2,Tm3,Tm4]).
sleepers_will_sleep([Match|Ms],[Tm1,Tm2,Tm3,Tm4]):-
    \+ member(Tm1,Match),
    \+ member(Tm2,Match),
    \+ member(Tm3,Match),
    \+ member(Tm4,Match),
    sleepers_will_sleep(Ms,[Tm1,Tm2,Tm3,Tm4]).

schedule(N,[],[]):- ! .
schedule(N,[Week|Weeks],[]):-
    format('WEEK ~p',[N]),nl,
    print( '------'),nl,
    week_print(Week),nl,
    N1 is N+1,
    schedule(N1,Weeks,NextByes),!.
schedule(N,[Week|Weeks],[Byes|NextByes]):-
    format('WEEK ~p',[N]),nl,
    print( '------'),nl,
    week_print(Week),nl,
    print('Byes: '),
    byes_print(Byes),nl,
    N1 is N+1,
    schedule(N1,Weeks,NextByes),!.



needed_matches([],[]).
needed_matches([Tm|Tms], [FinalGames |
                         [InterGames | 
                         [IntraGames | 
                         [DivionalGames | 
                         [Matches]]]]] ):-
    findall(X,divisional(vikings,X),DvGm),
    flatten(DvGm,DivionalGames),!, % Solo una forma de hacer los divisionales
    intragames(Tm,IntraGames),
    intergames(Tm,InterGames),
    finalgames(Tm,FinalGames),
    needed_matches(Tms,TmsGames).
    %append(TmGames,TmsGames,Matches).

% assignacion recursiva de partidos
check_matches(Calendar_structure):-
    findall(Team,standings(_,_,_,Team),Teams),!,
    needed_matches(Teams,Matches),%!, % Los equipos y los juegos necesarios no cambiaran
    check_matches(Matches,Calendar_structure).

    % Se podria revisar aqui mismo los byes?


check_matches([],Calendar).
check_matches([Match|Ms],Calendar):-
    big_member(Matches,Calendar_structure),
    check_matches(Ms,Calendar).

% Es miembro de una semana, o de las siguientes
big_member(Game,[Week|_]):-
    member(Game,Week).
big_member(Game,[_|Weeks]):-
    big_member(Game,Weeks).

flatten([],[]).
flatten([[[A,B],[C,D]]|Elements],[[A,B],[C,D]|Flatted]):-
    flatten(Elements,Flatted).