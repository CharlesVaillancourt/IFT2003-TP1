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
initialiserPlateau(AllPieces) :- AllPieces = [case(4,1,computer,black),case(4,7,human,white)],
  asserta(list_piece(AllPieces)).

getName(Name) :-
  write('Please enter your "name". to begin game?'), nl,
  read(Name),
  ( (nonvar(Name)) ->
    joueur(human),write('Welcome '), write(Name), write(' begin the game!'), nl;
    write('Not a valid name, try again!'),nl,getName(Name)
  ).

choisirOperation(Operation,AllPieces) :-
  write('Please enter your operation.: n - Initialize a new game;'),nl,
  write('                              s(X,Y) - Select the piece on the X,Y case;'),nl,
  write('                              c(X,Y) - Copy the selected piece to the X,Y case;'), nl,
  write('                              j(X,Y) - Make the selected piece jump on the X,Y case;'), nl,
  write('                              q - Exit the program?'), nl,
  read(Operation2),
  ( (member(Operation2, [s(X,Y), c(X,Y), j(X,Y)]), not(interval([X,Y])) ) ->
         write('Not a valid operation interval X,Y, try again!'), nl, choisirOperation(Operation,AllPieces);
    ( (member(Operation2, [n, s(X,Y), c(X,Y), j(X,Y), q]) ) ->
      effectuerOperation(Operation2,AllPieces);
      write('Not a valid operation, try again!'), nl, choisirOperation(Operation,AllPieces)
    )
  ).

effectuerOperation(Operation,AllPieces) :-
  ( (Operation == n) ->
    initialiserPlateau(AllPieces),imprimerPlateau(AllPieces),choisirOperation(Operation,AllPieces);
    ( (Operation == q) ->
      halt(0);
      ( (member(Operation, [s(1,1),s(1,2),s(1,3),s(1,4),s(1,5),s(1,6),s(1,7),s(2,1),s(2,2),s(2,3),s(2,4),s(2,5),s(2,6),s(2,7),
                            s(3,1),s(3,2),s(3,3),s(3,4),s(3,5),s(3,6),s(3,7),s(4,1),s(4,2),s(4,3),s(4,4),s(4,5),s(4,6),s(4,7),
                            s(5,1),s(5,2),s(5,3),s(5,4),s(5,5),s(5,6),s(5,7),s(6,1),s(6,2),s(6,3),s(6,4),s(6,5),s(6,6),s(6,7),
                            s(7,1),s(7,2),s(7,3),s(7,4),s(7,5),s(7,6),s(7,7)])) ->
        arg(1, Operation, X),arg(2, Operation, Y),pieceSelected(X,Y,Operation,AllPieces)
      )
    )
  ).

pieceSelected(X,Y,Operation,AllPieces) :-
  list_piece(L),
  ( (member(case(X,Y,_,_),L) ) ->
    choisirOperation(Operation,AllPieces);
    write("Not a valid position X,Y aren't exist on the board, try again!"), nl, choisirOperation(Operation,AllPieces)
  ).

% Pour commencer la partie "start".
start :- nl,
  write('Welcome to Colonie! You are playing white (and for the moment black too) and start.'),nl,
  getName(_),
  initialiserPlateau(AllPieces),
  imprimerPlateau(AllPieces),
  choisirOperation(_,AllPieces).
