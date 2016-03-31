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

% Intra-conference
intra(north,east).
intra(south,west).

% Todos los juegos divisionales
divisional(Home,Visitor) :-
    standings(Conf,Area,_,Home),
    standings(Conf,Area,_,Visitor),
    Home \= Visitor.

% Juegos intra división
intragames(L):- % De aquí ya salen 3808 combinaciones, pero un partido puede salir de pos 1 o 2
    intra(Card1,Card2),
    %% select(Conference1,[nfc,afc],[Conference2]),

    findall(FirstConfTeam,standings(Conf,Card1,_,FirstConfTeam),FstConfTms),
    findall(SecondConfTm ,standings(Conf,Card2,_,SecondConfTm) ,SndConfTms),
    
    select(Home1,FstConfTms,RestOfFirst), % Pensando en que no se pueden repetir
    select(Home2,RestOfFirst,_),
    
    select(Visitor1,SndConfTms,RestOfSecond),
    select(Visitor2,RestOfSecond,_),

    L = [ [Home1,Visitor1],[Home2,Visitor2] ].
