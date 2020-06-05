:- use_module(library(clpfd)).

at(Mat, [Row, Col], Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).


length_(N,L) :- length(L,N).

add(X,L,Ls) :- Ls = [X|[L]].
indices1(L,X,Ls)   :- maplist(add(X),L,Ls).
indices2(L1,L2,Li) :- maplist(indices1(L2),L1,Ls), append(Ls,Li).
indices(M,N,Is) :- numlist(1,M,LMs), numlist(1,N,LNs), indices2(LMs,LNs,Is).


vecinoS(L,[X,Y]) :- at(L, [X, Y], 1), A #= Y+1, at(L, [X,A], 2)|
                   at(L, [X, Y], 2), B #= Y-1, at(L, [X,B], 1)|
                   at(L, [X, Y], 3), C #= X+1, at(L, [C,Y], 4)|
                   at(L, [X, Y], 4), D #= X-1, at(L, [D,Y], 3).


numero(V1, V2, R) :- R #= V1*10 + V2.

comparacion(R, [X,Y]) :- X =< Y, numero(X, Y, R)|
                         Y < X, numero(Y, X, R).

are_identical(X, Y) :-
    X == Y.

filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).

obt_piezas(MS, MP, [XS, YS], [XP, YP]) :- at(MS, [XS, YS], 1), A #= YS+1, at(MS, [XS,A], 2), at(MP, [XS, YS], XP), at(MP, [XS,A], YP) |
                                          at(MS, [XS, YS], 2), C #= YS-1, at(MS, [XS,C], 1), XP #= 9, YP #=9| 
                                          at(MS, [XS, YS], 3), B #= XS+1, at(MS, [B,YS], 4), at(MP, [XS, YS], XP), at(MP, [B,YS], YP) |
                                          at(MS, [XS, YS], 4), D #= XS-1, at(MS, [D,YS], 3), XP #= 9, YP #=9 .


matrizS(M,N,MP,Mat) :- length(Mat, M), 
                maplist(length_(N), Mat), 
                append(Mat, Vs), Vs ins 1..4,
                indices(M,N,Is),                                                               
                maplist(vecinoS(Mat),Is),
                maplist(obt_piezas(Mat, MP), Is, L),
                filterList([9,9], L, Out),
                maplist(comparacion, LF, Out),
                all_distinct(LF).
               
matrizP([[1,3,0,1,2],
         [3,2,0,1,3],
         [3,3,0,0,1],
         [2,2,1,2,0]]).

%matrizP(A), matrizS(4, 5, A, M), maplist(writeln, M).