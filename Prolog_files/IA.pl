players([cyclist(position(X1, Y1), Played1, Fallen1), cyclist(position(X2, Y2), Played2, Fallen2), cyclist(position(X3, Y3), Played3, Fallen3)]). %représente les 3 cyclistes joués par un joueur
cyclist(position(X, Y), Played, Fallen). %représente un cycliste
position(X, Y). %représente la position d'un cycliste, X est le numéro entre 1 et 95 du plateau, Y est la position sur la largeur
etats_joueurs(etat_joueur(EtatJoueur1, CartesJoueur1), etat_joueur(EtatJoueur2, CartesJoueur2), etat_joueur(EtatJoueur3, CartesJoueur3), etat_joueur(EtatJoueur4, CartesJoueur4)). %Etat des joueurs à un instant t

etat_joueur(Joueur, [Carte1|Cartes]) :- %représente l'état d'un joueur à un instant t
    %Cartes est une liste représentant les cartes que le joueur a en main
    %Joueur représente les 3 cyclistes joués par le joueur ce qui donne l'info des positions des cyclistes
    Joueur = players([
        cyclist(position(X1, Y1), Played1, Fallen1), 
        cyclist(position(X2, Y2), Played2, Fallen2), 
        cyclist(position(X3, Y3), Played3, Fallen3)
    ]).

action_joueur(EtatActuel, Cartes, IndiceCarte, IndiceCycliste, NouveauX) :- %Prédicat pour prendre une décision. Dans mon idée, ce prédicat peut etre utilisé dans l'arbre de décision pour représenter un noeud
    nth0(IndiceCarte, Cartes, Carte), %Récupérer la carte à jouer à partir de l'indice
    nth0(IndiceCycliste, EtatActuel, Cycliste), %Récupérer le cycliste à déplacer à partir de l'indice
    Cycliste = cyclist(position(X, _), _, _), %Récupérer la position actuelle du cycliste
    test_rule(X + Carte), % Vérifier si la nouvelle valeur de X est valide, dans le sens que le move est valide et qu'il ne traverse pas d'autres cyclistes etc
    (test_rule(X + Carte) -> NouveauX is X + Carte ; NouveauX = -1 ). %Si la règle est respectée, renvoyer la nouvelle valeur de X, sinon -1. 
    %Dans l'idée, l'arbre de décision que j'imagine serait un algorithme Max^4 et donc le but de chaque joueur serait de maximiser les X de ses cyclistes

%L'idée pour l'arbre minmax à 4 joueurs pourrait d'une part de partir sur le fait de maximiser un jour 
%et en contrepartie faire en sorte que les 3 autres veulent minimiser le joueur 1, mais on perd le côté optimal de l'algo
%On peut alors partir sur un algo où chaque joueur tente simplement de maximiser ses points
%Un joueur a alors 15 possibilités d'actions (5 cartes x 3 cyclistes) et pour chaque action, on a ensuite les 15 possibites du joueur 2 et ainsi de suite

%Exemple de situation: imaginons l'état du joueur 1 comme ceci:
%Cycliste 1: x = 6
%Cycliste 2: x = 8
%Cycliste 3: x = 9

%Cartes: 8, 4, 5, 9, 5

%On prend en premier cas la première carte valant 8 et le premier cycliste.
%On ajoute le 8 à la valeur X donc ici on a 14. Si le move est legal, alors on garde la valeur, sinon on renvoie -1 et la branche ne continue pas plus loin, on remonte ou on passe à la suivante.
%Une fois qu'on remonte et qu'on veut comparer 2 branches, on essaye de maximiser la valeur. On compare donc les deux valeurs et on prend la plus proche de 95 (dernière case avant l'arrivée). 
%On peut utiliser le même principe que l'élagage Alpha bêta pour réduire des parties. Par exemple si la valeur de X 9+8 est possible sans poser de soucis par la suite, il y a pas moyen que la carte 4 vale une plus grande valeur. On peut donc directement supprimer cette branche.
%Cependant je vois un petit soucis avec cela, c'est que les actions des autres joueurs sont peu pris en compte dans la maximisation, et je ne sais pas trop comment changer ça.

arbre([ %Idée de structure de l'arbre de décision (l'idée est donc la même que celle que Youlan a proposé, elle me parait plutot adéquate 
%même si faut modifier pour avoir plus que deux branche comme c'est le cas dans cet exemple)
    [data1, data2], % Branche gauche
    [data3, data4] % Branche droite
]).

%Prédicat pour parcourir l'arbre (pour le moment je fais juste des write pour montrer un exemple)
parcourir_arbre([]). %Cas de base: liste vide
parcourir_arbre([BrancheGauche, BrancheDroite]) :-
    write('Branche gauche: '), writeln(BrancheGauche),
    write('Branche droite: '), writeln(BrancheDroite),
    %Parcourir récursivement les branches gauche et droite
    parcourir_arbre(BrancheGauche),
    parcourir_arbre(BrancheDroite).

