/**
 * \file colonie.pl
 * \brief Implantation d un joueur intelligent pour le jeu colonie.
 * \author Equipe 22
 * \date Octobre 2023
 * \version 1.0
 */
% predicat
interval([X,Y]) :-
  X > 0,
  Y > 0,
  X < 8,
  Y < 8.
intervalCopy([X,Y],[X2,Y2]) :-
  X2 >= (X-1),
  Y2 >= (Y-1),
  X2 =< (X+1),
  Y2 =< (Y+1),
  [X,Y] \= [X2,Y2].
intervalJump([X,Y],[X2,Y2]) :-
  X2 >= (X-2),
  Y2 >= (Y-2),
  X2 =< (X+2),
  Y2 =< (Y+2).
remover(_, [], []).
remover(R, [H|T], T2) :- \+ H \= R, remover(R, T, T2).
remover(R, [H|T], [H|T2]) :- H \= R, remover(R, T, T2).

gameEnded(AllPieces, black) :-
  not( member([case(_,_,humain,white)], AllPieces) ).

gameEnded(AllPieces, white) :-
  not( member([case(_,_,computer,black)], AllPieces) ).

% Interface du plateau de jeu formatte.
imprimerPlateau(AllPieces) :- nl,
  write('       1       2       3       4       5       6       7'),nl,
  write('   ________________________________________________________'), nl,
  imprimerPlateauLignes(AllPieces, 1),nl,nl.
  
imprimerPlateauLignes(_, 8) :- !.
imprimerPlateauLignes(AllPieces, YCoord) :-
  write('   |       |       |       |       |       |       |       |'), nl,
  write(' '), write(YCoord), write(' |'), imprimerPlateauCases(AllPieces, 1, YCoord), nl,
  write('   |_______|_______|_______|_______|_______|_______|_______|'), nl,
  NewYCoord is YCoord + 1,
  imprimerPlateauLignes(AllPieces, NewYCoord).
imprimerPlateauCases(_, 8, _) :- !.
imprimerPlateauCases(AllPieces, XCoord, YCoord) :-
  rechercherPieceSurCase(AllPieces, XCoord, YCoord, Piece, Color),
  imprimerPiece(Piece, Color),
  NewXCoord is XCoord + 1,
  imprimerPlateauCases(AllPieces, NewXCoord, YCoord).

% Imprimer les pions avec les couleurs blanc ou noir dans les cases.
rechercherPieceSurCase([], _, _, no, piece) :- !.
rechercherPieceSurCase([case(XCoord, YCoord, Piece, Color) | _], XCoord, YCoord, Piece, Color) :- !.
rechercherPieceSurCase([_ | RestPieces], XCoord, YCoord, Piece, Color) :-
  rechercherPieceSurCase(RestPieces, XCoord, YCoord, Piece, Color), !.
imprimerPiece('no','piece') :- write('       |'), !.
imprimerPiece(Piece, Color) :-
  couleurJoueur(Color),
  joueur(Piece),
  write('|').
imprimerPiece(cumputer, black).
couleurJoueur(white) :- write(' white '), !.
couleurJoueur(black) :- write(' black '), !.
joueur(_).

% Definir la position initiale des pions sur le board.
initialiserPlateau(AllPieces) :- AllPieces = [case(4,1,computer,black),case(4,7,human,white)].

getName(Name) :-
  write('Please enter your "name". to begin game?'), nl,
  read(Name),
  ( (nonvar(Name)) ->
    joueur(human),write('Welcome '), write(Name), write(' begin the game!'), nl;
    write('Not a valid name, try again!'),nl,getName(Name)
  ).

choisirOperation(Operation,AllPieces,SelectPiece,white) :-
  write('Please enter your operation.: n - Initialize a new game;'),nl,
  write('                              s(X,Y) - Select the piece on the X,Y case;'),nl,
  write('                              c(X,Y) - Copy the selected piece to the X,Y case;'), nl,
  write('                              j(X,Y) - Make the selected piece jump on the X,Y case;'), nl,
  write('                              q - Exit the program?'), nl,
  read(Operation2),
  ( (member(Operation2, [s(X,Y), c(X,Y), j(X,Y)]), not(interval([X,Y])) ) ->
    write('Not a valid operation interval X,Y, try again!'), nl, choisirOperation(Operation,AllPieces,SelectPiece,white);
    ( (member(Operation2, [n, s(X,Y), c(X,Y), j(X,Y), q]) ) ->
      effectuerOperation(Operation2,AllPieces,SelectPiece,white);
      write('Not a valid operation, try again!'), nl, choisirOperation(Operation,AllPieces,SelectPiece,white)
    )
  ).

effectuerOperation(Operation,AllPieces,SelectPiece,white) :-
  ( (Operation == n) ->
    getName(_),initialiserPlateau(NewAllPieces),imprimerPlateau(NewAllPieces),choisirOperation(_,NewAllPieces,_,white);
    ( (Operation == q) ->
      halt(0);
      ( (member(Operation, [s(_,_)])),! ->
        arg(1, Operation, X),arg(2, Operation, Y),pieceSelected(X,Y,Operation,AllPieces,white);
        ( (member(Operation, [c(_,_),j(_,_)]),var(SelectPiece) ),! ->
          write("Not a valid operation you will select piece before, try again!"), nl, choisirOperation(Operation,AllPieces,SelectPiece,white);
          ( (member(Operation, [c(_,_)]) ),! ->
            arg(1, Operation, X2),arg(2, Operation, Y2),pieceCopy(X2,Y2,AllPieces,SelectPiece,white);
            ( (member(Operation, [j(_,_)]) ),! ->
              arg(1, Operation, X2),arg(2, Operation, Y2),pieceJump(X2,Y2,AllPieces,SelectPiece,white)
            )
          )
        )
      )
    )
  ).

pieceSelected(X,Y,Operation,AllPieces,white) :-
  ( (member(case(X,Y,human,white),AllPieces) ) ->
    choisirOperation(Operation,AllPieces,case(X,Y,human,white),white);
    write("Not a valid position X,Y aren't exist white piece on the board, try again!"), nl, choisirOperation(Operation,AllPieces,_,white)
  ).

pieceCopy(X2,Y2,AllPieces,SelectPiece,white) :-
  arg(1, SelectPiece, X),
  arg(2, SelectPiece, Y),
  ( (member(case(X2,Y2,computer,black),AllPieces) ) ->
    write("Not a valid position X,Y he have a piece black on this case, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
    ( (member(case(X2,Y2,humain,white),AllPieces) ) ->
      write("Not a valid position X,Y he have a piece white on this case, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
      ( not(intervalCopy([X,Y],[X2,Y2]) ) ->
        write("Not a valid position X,Y to copy piece, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
        append([case(X2,Y2,human,white)],AllPieces,NewPieces), imprimerPlateau(NewPieces), choisirOperation(_,NewPieces,_,white)
      )
    )
  ).
pieceJump(X2,Y2,AllPieces,SelectPiece,white) :-
  arg(1, SelectPiece, X),
  arg(2, SelectPiece, Y),
  ( (member(case(X2,Y2,computer,black),AllPieces) ) ->
    write("Not a valid position X,Y he have a piece black on this case, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
    ( (member(case(X2,Y2,humain,white),AllPieces) ) ->
      write("Not a valid position X,Y he have a piece white on this case, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
      ( (not(intervalCopy([X,Y],[X2,Y2])), not(intervalJump([X,Y],[X2,Y2])) ) ->
        write("Not a valid position X,Y to jump piece, try again!"), nl, choisirOperation(_,AllPieces,SelectPiece,white);
        remover(case(X,Y,human,white),AllPieces,N), append([case(X2,Y2,human,white)],N,NewPieces), imprimerPlateau(NewPieces), choisirOperation(_,NewPieces,_,white)
      )
    )
  ).

% Pour commencer la partie "start".
start :- nl,
  write('Welcome to Colonie! You are playing white (and for the moment black too) and start.'),nl,
  getName(_),
  initialiserPlateau(AllPieces),
  imprimerPlateau(AllPieces),
  choisirOperation(_,AllPieces,_,white).

