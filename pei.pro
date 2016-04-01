% Solución para Pares e Impares
%
%
% @author López Javier
% @author Márquez

even(N):- 0 is N mod 2.
odd(N):- \+ even(N).

/*
     SEND +
     MORE
   ------- 
    MONEY
 
   Estrategia general de exploración dirigida por Prolog.

   Se escriben reglas tales que:
     1.- Generan posibles soluciones (una a la vez).
     2.- Verifican si la solución es plausible.
     3.- La muestran.
   Se aprovecha el backtracking de Prolog para que en caso que
   (2) falle, se regresa a (1) para generar otra solución. Si (1)
   falla, simplemente no hay soluciones.
   
 */

pei(X) :-
        % Generar una posible solución
        XI = [I0,I1,I2,I3,I4],
        XP = [P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10],
        DigitsI = [1,3,5,7,9],
        DigitsP = [2,4,6,8],
        %print('Antes de saignar menol'),nl,
        assign_digits(XI, DigitsI),
        %format('Ya pase el Impares y es ~p',[XI]),nl,
        assign_digits(XP, DigitsP),
        %format('Ya pase el Pares y es ~p',[XP]),nl,
        % Verificar si la solución es plausible, incluyendo
        % heurísticas para simplificar la búsqueda.
        I0 > 0, 
        P2 >= 0,
        P4 >= 0,
        P7 >= 0,
        I3 > 0,
         IPP  is              100*I0 + 10*P0 + P1,
          PP  is                       10*P2 + P3,
        %--- *
        PIPP  is    1000*P4 + 100*I1 + 10*P5 + P6,
         PIP  is              100*P7 + 10*I2 + P8,
        %--- +
        IIPP  is    1000*I3 + 100*I4 + 10*P9 + P10,
        
        even(IPP),
        even(PP),
        even(PIPP),
        even(PIP),
        even(IIPP ),
        R1 is P6 + P8,
        R1 > 9,
        R2 is I1 + P7,
        R2 > 9,
        P4 < 9,
        PIPPP is IPP  * PP,
        IIPP  is PIPP * PIP,
        % Tenemos una solución, así que la mostramos.
        % Si el problema tuviera múltiples soluciones, al
        % forzar el backtrack, se continuaría la búsqueda.
        % Si solamente interesa la primera solución o se sabe
        % que es única, aquí sería el lugar apropiado para
        % un cut.
        write('  IPP * =  '), write(IPP),write(' *'), nl,
        write('   PP   =   '), write(PP), nl,
        write(' ------'),nl,
        write(' PIPP   = '), write(PIPP),write(' +'), nl,
        write('  PIP   =   '), write(PP), nl,
        write(' ------'),nl,
        write(' IIPP   = '), write(IIPP), nl.

assign_digits([], _List).
assign_digits([D|Ds], List):-
        select(D, List, NewList),
        assign_digits(Ds, List).