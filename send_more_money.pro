%%   S E N D
%% + 1 0 8 E
%% ------------
%% M O N E 2

works(Vars):-
    Vars = [S,E,N,D,M,O,R,Y],
    Numbers = [0,1,2,3,4,5,6,7,8,9],
    assign(Vars,Numbers),
    M \= 0,
    S \= 0,
    Send  is            S *1000 + E *100 + N *10 + D,
    More  is            M *1000 + O *100 + R *10 + E,
    Money is M *10000 + O *1000 + N *100 + E *10 + Y,
    Send_More is Send+More,
    Money == Send_More.

assign([],L).
assign([X|Xs],L):-
    select(X,L,NL),
    assign(Xs,NL).
