% Solución para Pares e Impares
%
%
% @author López Javier
% @author NMárquez Nabil


%% even(+N:atom)
%  
% Predicado que, dado un entero N, triunfa si N es par.
% 
% @param N        atomo que indica cual es numero a consultar.
even(N):- 0 is N mod 2.

%% odd(+N:atom)
%  
% Predicado que, dado un entero N, triunfa si N es impar.
% 
% @param N        atomo que indica cual es numero a consultar.
odd(N):- \+ even(N).

%% pei()
%  
% Predicado que resuelve el problema de la multiplicación propuesta.
% 
pei :-
        % Generar una posible solución
        XI = [I0],
        XP = [P0,P1,P2,P3],
        DigitsI = [1,3,5,7,9], % Dígitos Impares.
        DigitsP = [0,2,4,6,8], % Dígitos Pares.
        assign_digits(XI, DigitsI), %Asignación de los impares.
        assign_digits(XP, DigitsP), %Asignación de los pares.

        % Verificar si la solución es apropiada.

        % Calculando IPP y PP
        I0 > 0, 
        P3 > 0,
        IPP  is              100*I0 + 10*P0 + P1,
        even(IPP),
        PP   is                       10*P2 + P3,
        even(PP),

        % Calculando PIPP y PIP
        PIPP  is    IPP*P3,
        PIPP  >     999,
        even(PIPP),
        PIP   is    IPP*P2,
        PIP   <     1000,
        even(PIP),

        % Calculando I1 y P4,P5,P6
        P6 is PIPP rem 10,
        even(P6),
        P5 is PIPP rem 100 // 10,
        even(P6),
        I1 is PIPP rem 1000 // 100,
        odd(I1),
        P4 is PIPP rem 10000 // 1000,
        even(P4),
        P4>0,

        % Calculando I2 y P7,P8
        P8 is PIP rem 10,
        even(P8),
        I2 is PIP rem 100 // 10,
        odd(I2),
        P7 is PIP rem 1000 // 100,
        even(P7),
        P7>0,

        % Obteniendo IIPP, I3, I4 y P9, P10.
        IIPP is PIPP + PIP*10,
        P10 = P6,
        P9 is IIPP rem 100 // 10,
        even(P9),
        I4 is IIPP rem 1000 // 100,
        odd(I4),
        I3 is IIPP rem 10000 // 1000,
        odd(I3),
        I3 > 0,
        even(IIPP),
        PIPPP is IPP  * PP,
        IIPP  is PIPP + PIP*10, !, 

        % Encontramos la solución única, así que colocamos
        % un Cut.
        % Tenemos una solución, así que la mostramos.
        write('\n Respuesta: \n'), nl,
        format('  ~p *',[IPP]), nl,
        format('   ~p',[PP]), nl,
        write(' ------'), nl,
        format(' ~p +',[PIPP]), nl,
        format(' ~p',[PIP]), nl,
        write(' ------'), nl,
        format(' ~p',[IIPP]), nl.


%% assign_digits(+D:list,+List:list)
%  
% Predicado que tiene éxito si no quedan elementos restantes por asignar.
% 
% @param D        lista de variables a signar
% @param List     lista de posbiles valores
assign_digits([], _List).
assign_digits([D|Ds], List):-
        select(D, List, Restantes), %Obvia restantes para permitir duplicados. 
        assign_digits(Ds, List). 