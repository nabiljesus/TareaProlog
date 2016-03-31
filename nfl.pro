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

%% standings(afc,north,1,bengals).
%% standings(afc,north,2,steelers).
%% standings(afc,north,3,ravens).
%% standings(afc,north,4,browns).

%% standings(afc,south,1,texans).
%% standings(afc,south,2,colts).
%% standings(afc,south,3,jaguars).
%% standings(afc,south,4,titans).

%% standings(afc,west,1,broncos).
%% standings(afc,west,2,chiefs).
%% standings(afc,west,3,raiders).
%% standings(afc,west,4,chargers).



% National Football Conference standings
%% standings(nfc,east,1,redskins).
%% standings(nfc,east,2,eagles).
%% standings(nfc,east,3,giants).
%% standings(nfc,east,4,cowboys).

%% standings(nfc,north,1,vikings).
%% standings(nfc,north,2,packers).
%% standings(nfc,north,3,lions).
%% standings(nfc,north,4,bears).

%% standings(nfc,west,1,cardinals).
%% standings(nfc,west,2,seahawks).
%% standings(nfc,west,3,rams).
%% standings(nfc,west,4,'49ers').

%% standings(nfc,south,1,panthers).
%% standings(nfc,south,2,falcons).
%% standings(nfc,south,3,saints).
%% standings(nfc,south,4,buccaneers).

% Intra-conference
intra(north,east).
intra(south,west).

% Todos los juegos divisionales
divisional(Home,Visitor) :-
    standings(Conf,Area,_,Home),
    standings(Conf,Area,_,Visitor),
    Home1 \= Visitor.

% Juegos intra division misma conferencia
intragames(L):- % De aquí ya salen 3808 combinaciones, pero un partido puede salir de pos 1 o 2
    intra(Card1,Card2),
    member(Conf,[afc,nfc]),

    findall(FirstConfTeam,standings(Conf,Card1,_,FirstConfTeam),FstConfTms),
    findall(SecondConfTm ,standings(Conf,Card2,_,SecondConfTm) ,SndConfTms),

    select(Home1,FstConfTms,RestOfFirst), % Pensando en que no se pueden repetir
    select(Home2,RestOfFirst,_),
    
    select(Visitor1,SndConfTms,RestOfSecond),
    select(Visitor2,RestOfSecond,_),

    L = [ [Home1,Visitor1],[Home2,Visitor2] ].

positional(L) :-

    select(Area1,[north,east,west,south],RestOfFirst), % Pensando en que no se pueden repetir
    select(Area2,RestOfFirst,_),
    % Revisar que no esta ya en intragames

    findall(FirstConfTeam,standings(Conf,Area1,Pos,FirstConfTeam),FstConfTms),
    findall(SecondConfTm ,standings(Conf,Area2,Pos,SecondConfTm) ,SndConfTms),

    select(Home1,FstConfTms,RestOfFirst), % Pensando en que no se pueden repetir
    select(Home2,RestOfFirst,_),
    
    select(Visitor1,SndConfTms,RestOfSecond),
    select(Visitor2,RestOfSecond,_),

    L = [ [Home1,Visitor1],[Home2,Visitor2] ].    

byes(Rests) :- 
    findall(Team,standings(_,_,_,Team),Teams),
    select_n(Teams,[],Rests).
byes.


select_n([],[],[]).
select_n([Teams],[A,B,C,D],[[A,B,C,D]|All]):-
    select_n(Teams,[],All).  
select_n(Teams,L,All):- 
    select(Team,Teams,RestOfTeams),
    format('Estoy seleccionando ~p',[Team]),nl,
    (
        L = [Last|_]
    ->
        Team @< Last
    ;
        print('Voy aca menol'),nl
    ),
    format('~p paso!!',[Team]),nl,
    select_n(RestOfTeams,[Team|L],All).

