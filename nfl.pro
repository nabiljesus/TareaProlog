% Solución para Calendario NFL
%
%
% @author López Javier
% @author Márquez

:- dynamic(played/2).

played(caracasfc,deportivotachira).

retract2(X)       :- X, reallyRetract2(X).
reallyRetract2(X) :- retract(X).
reallyRetract2(X) :- asserta(X), fail.

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

% Todos los juegos divisionales
divisional(Home,[[Home,Visitor],[Visitor,Home]]) :-
    standings(Conf,Area,_,Home),
    standings(Conf,Area,_,Visitor),
    Home \= Visitor.

% Juegos intra conferencia
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

% Juegos Inter-conferencia
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

% Juegos de conferencia con equipos de la misma posicion
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

% Selecciona 8 listas con grupos de 4 equipos no repetidos
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

% Indica todas las disposiciones de equipos que descansaran en las primeras 8 semanas
byes(Result) :- 
    findall(Team,standings(_,_,_,Team),Teams), !,
    select_n(Teams,[],Rests),
    reverse(Rests,Result).

% Impresion de byes en formato dado
byes_print([Team])          :- print(Team),nl.
byes_print([Team|Teams])    :- format('~p, ',[Team]), byes_print(Teams).

week_print([]).
week_print([Match|Matches]) :- match_print(Match),nl, week_print(Matches).

match_print(Teams)          :- format('~p at ~p',Teams).

% Creador de la estructura de un calendario
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

% Predicado principal
schedule :- 
    byes(B),
    make_structure(Matches),
    %% assign(Matches),
    findall(Team,standings(_,_,_,Team),Teams),!,
    dynamic_assign(Matches,Teams,Teams),
    check_schedule(Matches,B),
    %% retractall(played(X,Y)),
    (schedule(1,Matches,B),!;true).

dynamic_assign([],Teams,Teams).
dynamic_assign([[]|Weeks],Teams,Teams) :-
    dynamic_assign(Weeks,Teams,Teams).
dynamic_assign([[Match1|RestOfWeek]|Weeks],Teams,Teams) :-
    member(Home,Teams),
    member(Visitor,Teams),
    Home \= Visitor,
    \+ played(Home,Visitor),
    reallyRetract2(played(Home,Visitor)),
    Match1 = [Home,Visitor],
    dynamic_assign([RestOfWeek|Weeks],Teams,Teams).


check_schedule([],[]).
check_schedule([M|Ms],[B|Bs]):-
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
assign(Calendar_structure):-
    findall(Team,standings(_,_,_,Team),Teams),!,
    needed_matches(Teams,Matches),%!, % Los equipos y los juegos necesarios no cambiaran
    assign(Matches,Calendar_structure).

    % Se podria revisar aqui mismo los byes?


assign([],Calendar).
assign([Match|Ms],Calendar):-
    big_member(Matches,Calendar_structure),
    assign(Ms,Calendar).

% Es miembro de una semana, o de las siguientes
big_member(Game,[Week|_]):-
    member(Game,Week).
big_member(Game,[_|Weeks]):-
    big_member(Game,Weeks).

flatten([],[]).
flatten([[[A,B],[C,D]]|Elements],[[A,B],[C,D]|Flatted]):-
    flatten(Elements,Flatted).