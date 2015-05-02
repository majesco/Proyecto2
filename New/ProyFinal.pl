:- use_module(library(clpfd)). 

figura(Fig,Fichas,Sol):-
  genSol(Fig,Fichas,[],Sol),
  plasmSol(Fig,Fichas,Sol).


tst1(X):-
  rotacion(X),
  tst2(X).


tst2(Y):- Y=270.

rotacion(0).
rotacion(90).
rotacion(180).
rotacion(270).

genSol( _,[],Sol,R):-reverse(Sol,R).
genSol(Fig, [Head|Tail]  , Sol, R):- /*crea todas las posibles soluciones al acertijos*/
	
  [H|T]  = Head, 
  [H2|T2]=Fig,
  rotacion(X), 
  proper_length(Fig, LengthY),
  between( 1, LengthY, J ),
  proper_length( H2, LengthX ),
  between(   1, LengthX, I ),
  append(  [[H,X,I,J]], Sol , SolT), 
  genSol(Fig,Tail, SolT, R).  



plasmSol(Fig,Fichas,R):-   /*evalua que una solcion sea valida coparandola con la Fig*/
  genMtrx0(Fig,[],Mtrx0), !,         /* genera una matriz de o de iguales dimensiones a Fig*/
  giraFichs(Fichas,R,[],Fichas2),  /* las fichas cuando sea necesario*/
  testFich(Fichas2,Fig,R),          /* prueba que las fichas puedan ser ubicadas en la matriz de Fig*/
  acomodarFicha(Mtrx0,R,Fichas,Fig).     /* toma R y Mtrx0 para hacer una matriz que luego se comparara con la de Fig*/



genMtrx0([],MR,MR). /*genera una matriz de o con las dimensiones de una figura,se ejecuta con genMtrx0(Fig,[],R)*/
genMtrx0([Hfig|Tfig],M,MR):-
  proper_length(Hfig,LengtH),
  makeList0(LengtH,[],R), !,
  genMtrx0(Tfig,[R|M],MR).

makeList0(0, R,R).  /*crea una lista de o de manaño Tam, se ejecuta con makeList0(#,[],R).*/
makeList0(Tam,L,R):-
  Tam2 is Tam - 1,
  makeList0(Tam2,[o|L],R).




testFich([],_,[]).
testFich([HF|TF],Fig,[HSol|TSol]):- /*prueba que las fichas quepan en las dimensiones de la matriz de la Figura*/
  HF = [H2F|T2F],/*toma en H2F el nombre de la ficha y el T2F la lista de representacion de una ficha*/
  T2F=[H3F|T3F],
  proper_length(H3F,LenXF),/*toma el tamaño de la ficha*/
  proper_length(T2F,LenYF),
  Fig= [HFig|TFig],
  proper_length(HFig,LenXFig),/*toma el tamaño de la figura*/
  proper_length(Fig,LenYFig),
  nth1(3,HSol,PosX),   /*toma la posicion de la ficha*/
  nth1(4,HSol,PosY),
  X is PosX + LenXF - 1,
  Y is PosY + LenYF - 1,
  X =< LenXFig, /*establece si la ficha segun su posicion y sus dimensiones se puede hubicar en la solucion*/
  Y =< LenYFig,
  testFich(TF,Fig,TSol).


giraFichs([],[],RFichs,RFichs2):-reverse(RFichs,RFichs2). /*toma la lista de fichas y la solucion, gira las fichas que se indican en la solucion, da el resultado en RFichs2*/
giraFichs([HF|TF],[HSol|TSol],RFichs,RFichs2):-
  HF=[H2F|T2F],/*toma la primera ficha*/
  nth1(2,HSol,Rot),/*toma el numero del giro*/
  giraAux(Rot,T2F,RF),/*gira la ficha*/
  R2=[H2F|RF],/*pone el nombre a la pieza girada*/
  (Rot\=0->T2F\=RF;(Rot=0->true)),/*prueba de repeticion de piezas*/
  giraFichs(TF,TSol,[R2|RFichs],RFichs2).
  



giraAux(0,Fich,Fich2):- Fich=Fich2.
giraAux(90,Fich,Fich2):- flip90(Fich,Fich2).
giraAux(180,Fich,Fich2):- flip180(Fich,Fich2).
giraAux(270,Fich,Fich2):- flip270(Fich,Fich2).

reverseMat( [], RMat, RMat  ).
reverseMat( [H|T], RMat , R):-
  reverse(H, X),
  append( [X], RMat , RX ),
  reverseMat( T, RX , R  ).

flip90( OrigMat, Result ):-
  transpose(OrigMat, X ),
  reverseMat( X, R, Almost ),
  !,
  reverse(Almost, Result ),
  !.

flip180(OrigMat, Result):-
  flip90( OrigMat, X ),
  flip90( X, Result).

flip270( OrigMat, Result):-
  flip180(OrigMat, X),
  flip90(X, Result).



acomodarFicha( Mat1, [], _, Mat2):- Mat1 =Mat2. /* Acomoda una ficha y compara la matriz resultante con la de la  */
                              /* ficha a ver si la solucion es correcta */    
acomodarFicha( Mtrx0, [HSol|TSol], [HFicha|TFicha], MtrxFig):-
  
  HFicha = [H|T],
  nth1( 3, HSol, PosX  ), /* Posicion X de la ficha      */
  nth1( 4, HSol, PosY), /*   Posicion Y de la ficha      */

  llenarMtrx( Mtrx0, T, PosX , PosY, L ),
  acomodarFicha( L, TSol, TFicha, MtrxFig).

llenarMtrx( A, [],_,_,B) :- A=B. /* write(B),write(' '). */

llenarMtrx( Mtrx0, [HFilaFicha|TFilaFicha], PosX , PosY , L):-

  PosXR is PosX -1,
  PosYR is PosY -1,

  llenarElemento(Mtrx0, HFilaFicha, PosXR, PosYR , K),

  PosYNew is PosY +1,
  llenarMtrx( K, TFilaFicha, PosX, PosYNew , L).


llenarElemento(A,[], _,_,B):- A=B.  
llenarElemento(Mtrx0, [Hcaracter | Tcaracter ], PosX, PosY,  X):-
  
  nth0( PosY ,Mtrx0, ListaModi  ),
  replaceX( ListaModi, PosX, Hcaracter  ,ListaModiR),
  replaceX( Mtrx0, PosY, ListaModiR , Mtrx1 ),
  PosXNew is PosX +1,  
  llenarElemento(Mtrx1, Tcaracter, PosXNew, PosY, X).



replaceX([_|T], 0, X, [X|T]).

replaceX([H|T], I, X, [ H|R ] ):-  /*Funcion que reemplaza en una lista  */
  
  I > 0, I1 is I-1, replaceX(T, I1, X, R). 
                          /* un elemento en cierto indice ! :) */



fichs(a,[ [a, [x,o],[x,x]], [b, [x,o],[o,x]] ]).

fig(a, [ [x,o,x,o],[x,x,o,x] ]).

sols(a, [ [a,90,1,1],[b,0,4,1] ]).


/*([[x,o,x,x],[o,x,o,o]],[[a,[x,o],[o,x]],[b,[x,x],[o,o]]],Sol)*/



