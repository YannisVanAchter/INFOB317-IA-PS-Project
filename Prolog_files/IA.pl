
% players([cyclist(position(X1, Y1), Played1, Fallen1), cyclist(position(X2, Y2), Played2, Fallen2), cyclist(position(X3, Y3), Played3, Fallen3)]). %représente les 3 cyclistes joués par un joueur
% cyclist(position(X, Y), Played, Fallen). %représente un cycliste
% position(X, Y). %représente la position d'un cycliste, X est le numéro entre 1 et 95 du plateau, Y est la position sur la largeur
% etats_joueurs(etat_joueur(EtatJoueur1, CartesJoueur1), etat_joueur(EtatJoueur2, CartesJoueur2), etat_joueur(EtatJoueur3, CartesJoueur3), etat_joueur(EtatJoueur4, CartesJoueur4)). %Etat des joueurs à un instant t

% etat_joueur(Joueur, [Carte1|Cartes]) :- %représente l'état d'un joueur à un instant t
%     %Cartes est une liste représentant les cartes que le joueur a en main
%     %Joueur représente les 3 cyclistes joués par le joueur ce qui donne l'info des positions des cyclistes
%     Joueur = players([
%         cyclist(position(X1, Y1), Played1, Fallen1), 
%         cyclist(position(X2, Y2), Played2, Fallen2), 
%         cyclist(position(X3, Y3), Played3, Fallen3)
%     ]).

% action_joueur(EtatActuel, Cartes, IndiceCarte, IndiceCycliste, NouveauX) :- %Prédicat pour prendre une décision. Dans mon idée, ce prédicat peut etre utilisé dans l'arbre de décision pour représenter un noeud
%     nth0(IndiceCarte, Cartes, Carte), %Récupérer la carte à jouer à partir de l'indice
%     nth0(IndiceCycliste, EtatActuel, Cycliste), %Récupérer le cycliste à déplacer à partir de l'indice
%     Cycliste = cyclist(position(X, _), _, _), %Récupérer la position actuelle du cycliste
%     test_rule(X + Carte), % Vérifier si la nouvelle valeur de X est valide, dans le sens que le move est valide et qu'il ne traverse pas d'autres cyclistes etc
%     (test_rule(X + Carte) -> NouveauX is X + Carte ; NouveauX = -1 ). %Si la règle est respectée, renvoyer la nouvelle valeur de X, sinon -1. 
%     %Dans l'idée, l'arbre de décision que j'imagine serait un algorithme Max^4 et donc le but de chaque joueur serait de maximiser les X de ses cyclistes

% %L'idée pour l'arbre minmax à 4 joueurs pourrait d'une part de partir sur le fait de maximiser un jour 
% %et en contrepartie faire en sorte que les 3 autres veulent minimiser le joueur 1, mais on perd le côté optimal de l'algo
% %On peut alors partir sur un algo où chaque joueur tente simplement de maximiser ses points
% %Un joueur a alors 15 possibilités d'actions (5 cartes x 3 cyclistes) et pour chaque action, on a ensuite les 15 possibites du joueur 2 et ainsi de suite

% %Exemple de situation: imaginons l'état du joueur 1 comme ceci:
% %Cycliste 1: x = 6
% %Cycliste 2: x = 8
% %Cycliste 3: x = 9

% %Cartes: 8, 4, 5, 9, 5

% %On prend en premier cas la première carte valant 8 et le premier cycliste.
% %On ajoute le 8 à la va'1-A-left'leur X donc ici on a 14. Si le move est legal, alors on garde la valeur, sinon on renvoie -1 et la branche ne continue pas plus loin, on remonte ou on passe à la suivante.
% %Une fois qu'on remonte et qu'on veut comparer 2 branches, on essaye de maximiser la valeur. On compare donc les deux valeurs et on prend la plus proche de 95 (dernière case avant l'arrivée). 
% %On peut utiliser le même principe que l'élagage Alpha bêta pour réduire des parties. Par exemple si la valeur de X 9+8 est possible sans poser de soucis par la suite, il y a pas moyen que la carte 4 vale une plus grande valeur. On peut donc directement supprimer cette branche.
% %Cependant je vois un petit soucis avec cela, c'est que les actions des autres joueurs sont peu pris en compte dans la maximisation, et je ne sais pas trop comment changer ça.

% arbre([ %Idée de structure de l'arbre de décision (l'idée est donc la même que celle que Youlan a proposé, elle me parait plutot adéquate 
% %même si faut modifier pour avoir plus que deux branche comme c'est le cas dans cet exemple)
%     [data1, data2], % Branche gauche
%     [data3, data4] % Branche droite
% ]).

% %Prédicat pour parcourir l'arbre (pour le moment je fais juste des write pour montrer un exemple)
% parcourir_arbre([]). %Cas de base: liste vide
% parcourir_arbre([BrancheGauche, BrancheDroite]) :-
%     write('Branche gauche: '), writeln(BrancheGauche),
%     write('Branche droite: '), writeln(BrancheDroite),
%     %Parcourir récursivement les branches gauche et droite
%     parcourir_arbre(BrancheGauche),
%     parcourir_arbre(BrancheDroite).



% on a besoin de pour chaque joueur
% liste avec en premier elem sa main, puis ses coureurs représentés par un tuple de coord [[num_player, [main], [(x1,y1), (x2,y2)]], num_player]
%  ex: [[1,[1,1,1,1,1],['1','2','4']],[4,[4,4,4,4,4],['3','4','6']],[2,[2,2,2,2,2],['2','3','10a']],[3,[3,3,3,3,3],['4','5','22']]]
% minmax à 2, l'heuristique ça serait d'être le premier
get_move_IA(Infos,Move):-
    nth0(0,Infos,Data),
    get_infos_player(Data,1,Player1),
    get_infos_player(Data,2,Player2),
    get_infos_player(Data,3,Player3),
    get_infos_player(Data,4,Player4).


% there is not base case since every player should be present one time only, and each shoul be present
get_infos_player([H_infos|_],Num_player,Infos):-
    nth0(0,H_infos,Num),
    nth0(1,H_infos,Hand),
    nth0(2,H_infos,Bikes),
    Num = Num_player,
    Infos=[Hand,Bikes], !.

get_infos_player([H_infos|T_infos],Num_player,Infos):-
    get_infos_player(T_infos,Num_player,Infos).

    

% player 0=min
% player 1= max


% minmax([1,2],[[[1,2],['1-A-left','2-A-left']],[[1,1],['1-A-left','1-A-left']]],1,1,0,A)
% State,Depth,Player,BestScore
% base case: depth reached or terminal state reached
minmax(State,0,_,Best_score,Best_move):-
    print('b'),
    score(State,Best_score).

minmax(State,_,_,Best_score,Best_move):-
    print('c'),
    terminal_state(State),
    score(State,Best_score).

% recursive case
% faudrait un minmax qui iète sur les cartes e,
% du coup on iètre plus sur les cartes aèrs
minmax(State,Depth,Player,Best_score,Best_move):-
    print('a'),
    Player=1,
    nth0(0,State,Player_infos),
    nth0(1,Player_infos,Bikes),
    nth0(0,Player_infos,Cards),
    nth0(0,Cards,Card),
    possible_moves(State,Card,Bikes,Moves),
    evaluate_moves(Moves,State,Depth,Player,Best_score,Best_move).

minmax(State,Depth,Player,Best_score,Best_move):-
    Player=0,
    nth0(1,State,Player_infos),
    nth0(1,Player_infos,Bikes),
    nth0(0,Player_infos,Cards),
    nth0(0,Cards,Card),
    possible_moves(State,Card,Bikes,Moves),
    evaluate_moves(Moves,State,Depth,Player,Best_score,Best_move).

evaluate_moves([],_,_,_,Best_score,Best_move).

evaluate_moves([Move|Other_moves],State,Depth,Player,Best_score,Best_move):-
    Player=1,
    make_move(State,Move,Player,New_state),
    Opponent= 0,
    New_depth is Depth-1,
    nth0(0,State,Player),
    nth0(0,Player,Cards),
    minmax(Cards,New_state, New_depth, Opponent, Temp_score,Best_move),
    New_best_score= max(Best_score,Temp_score),
    New_best_score>Best_score,
    Best_move=Move,
    evaluate_moves(Other_moves,State,Depth,Player,Best_score,Best_move).

evaluate_moves([Move|Other_moves],State,Depth,Player,Best_score,Best_move):-
    Player=1,
    make_move(State,Move,Player,New_state),
    Opponent= 0,
    New_depth is Depth-1,
    nth0(0,State,Player),
    nth0(0,Player,Cards),
    minmax(Cards,New_state, New_depth, Opponent, Temp_score),
    New_best_score= max(Best_score,Temp_score),
    New_best_score=<Best_score,
    evaluate_moves(Other_moves,State,Depth,Player,Best_score,Best_move).

evaluate_moves([Move|Other_moves],State,Depth,Player,Best_score):-
    Player=0,
    make_move(State,Move,Player,New_state),
    Opponent= 1,
    New_depth is Depth-1,
    nth0(1,State,Player),
    nth0(0,Player,Cards),
    minmax(Cards,New_state, New_depth, Opponent, Temp_score),
    New_best_score= min(Best_score, Temp_score),
    New_best_score>Best_score,
    Best_move=Move,
    evaluate_moves(Other_moves,State,Depth,Player,Best_score,Best_move).

evaluate_moves([Move|Other_moves],State,Depth,Player,Best_score):-
    Player=0,
    make_move(State,Move,Player,New_state),
    Opponent= 1,
    New_depth is Depth-1,
    nth0(1,State,Player),
    nth0(0,Player,Cards),
    minmax(Cards,New_state, New_depth, Opponent, Temp_score),
    New_best_score= min(Best_score, Temp_score),
    New_best_score=<Best_score,
    evaluate_moves(Other_moves,State,Depth,Player,Best_score,Best_move).
    
% test if one of the position ends the game
terminal_state(State):-
    nth0(0,State,Player1),
    nth0(1,State,Player2),
    nth0(1,Player1,Pos1),
    nth0(1,Player2,Pos2),
    append(Pos1,Pos2,Positions),
    terminal_pos(Positions).
    
terminal_pos([]):-false,!.
terminal_pos([Pos|Other_pos]):-
    end(Pos),!.

terminal_pos([Position|Other_pos]):-
    terminal_pos(Other_pos).

% get game state structure for the IA
game_state(Player1, Player2, Player3, Player4, Num_player_IA, [IA, Others]) :-
    Num_player_IA= 1,
    IA= Player1,
    Others= [get_other_cards(Player2, Player3, Player4), get_other_bikes(Player2, Player3, Player4)].
    
game_state(Player1, Player2, Player3, Player4, Num_player_IA, [IA, Others]) :-
    Num_player_IA= 2,
    IA= Player2,
    Others= [get_other_cards(Player1, Player3, Player4), get_other_bikes(Player2, Player3, Player4)].
    
game_state(Player1, Player2, Player3, Player4, Num_player_IA, [IA, Others]) :-
    Num_player_IA= 3,
    IA= Player3,
    Others= [get_other_cards(Player1, Player2, Player4), get_other_bikes(Player2, Player3, Player4)].
    
game_state(Player1, Player2, Player3, Player4, Num_player_IA, [IA, Others]) :-
    Num_player_IA= 4,
    IA= Player4,
    Others= [get_other_cards(Player1, Player2, Player3), get_other_bikes(Player2, Player3, Player4)].
    
get_other_cards(Player2, Player3, Player4) :-
    nth0(0, Player2, Cards2),
    nth0(0, Player3, Cards3),
    nth0(0, Player4, Cards4),
    append(Cards2, Cards3, Temp_card),
    append(Cards4, Temp_card, Cards_other).
          
get_other_bikes(Player2, Player3, Player4) :-
    nth0(1, Player2, Bike2),
    nth0(1, Player3, Bike3),
    nth0(1, Player4, Bike4),
    append(Bike2, Bike3, Temp_bike),
    append(Bike4, Temp_bike, Bike_other).

make_move(State,Move,Player,New_state):-
    Player=1,
    nth0(1,Move,Index),
    nth0(3,Move,New_pos),
    nth0(0,Move,Card),
    nth0(0,State,Player1),
    nth0(1,Player1,Old_bikes),
    nth0(0,Player1,Cards),
    nth0(1,State,Player2),
    select(Card,Cards,New_Cards),
    replace_position(Index,New_pos,0,Old_bikes,[],New_bikes),
    New_state=[[New_Cards,New_bikes],Player2].
    
make_move(State,Move,Player,New_state):-
    Player=0,
    nth0(1,Move,Index),
    nth0(3,Move,New_pos),
    nth0(0,Move,Card),
    nth0(0,State,Player1),
    nth0(1,State,Player2),
    nth0(0,Player2,Cards),
    nth0(1,Player2,Old_bikes),
    select(Card,Cards,New_Cards),
    replace_position(Index,New_pos,0,Old_bikes,[],New_bikes),
    New_state=[Player1,[New_Cards,New_bikes]].

replace_position(Index,New_position,Iter_index,[Old_bike|Other_old_bikes],Acc,New_state_bikes):-
    Iter_index=Index,
    append(Acc,[New_position|Other_old_bikes],New_state_bikes),!.

replace_position(Index,New_position,Iter_index,[Old_bike|Other_old_bikes],Acc,New_state_bikes):-
    append(Acc,[Old_bike],New_acc),
    New_iter_index is Iter_index+1,
    replace_position(Index,New_position,New_iter_index,Other_old_bikes,New_acc,New_state_bikes).

    


% la main est déjà modif car fait au dessus, ici je veux repartir sur chaque vélo
% et faudrait faire ça mais qui donne une val diff de new_pos à chaque fois
% faut aussi créer new_bikes, pour ça je pense que dégager l'elem à l'index et le remplacer ok
new_bikes(Used_card,Hand,New_pos,[Pos|Other_pos],Turn,Hand2,Bikes2,Tree):-
    New_turn,
    New_bikes,
    New_tree=[(Used_card,Position)],
    arbre_d(Hand,New_bikes,Hand2,Bikes2,Turn,New_tree),
    new_bikes().


% if moves without causing a fall possible, return those
possible_moves(State,Card,Bikes,Possible_moves):-
    select_moves(Card,Bikes,All_moves,Candidate_moves),
    length(Candidate_moves,Number_moves),
    Number_moves>0,
    Possible_moves=Candidate_moves.

% if not, return all
possible_moves(State,Card,Bikes,Possible_moves):-
    select_moves(State,Card,Bikes,Possible_moves).

% select all moves for current hand and bikes position
select_moves(Card,Bikes,Possible_moves,Moves):-
    create_moves(Card,Bikes,0,[],Possible_moves),
    test_moves(State,Possible_moves,Moves).


% create all the possible next moves for each bike with each card in hand
% create_moves([1,2,3],['1-A-left','2-A-left','2-A-left'],A).
% A = [(1, ('2-A-left', '2-A-left', '2-A-left'), ('1-A-left', '3-A-left', '2-A-left'), '1-A-left', '2-A-left', '3-A-left'), (2, ('3-A-left', '2-A-left', '2-A-left'), ('1-A-left', '4-A-left', '2-A-left'), '1-A-left', '2-A-left', '4-A-left'), (3, ('4-A-left', '2-A-left', '2-A-left'), ('1-A-left', '5-A-left', '2-A-left'), '1-A-left', '2-A-left', '5-A-left')]
% je vais pas pouvoir faire ça comme ça, parce que pour les 3 autres on en a 9 et pas 3 donc tendu

% faudrait rendre un truc du genre
% [ [carte, position, nouvelle_pos, [nouvel_ensemnble]], [carte, position, nouvelle pos, [nouvel ensemble]] ]
% iterate_cards([],_,[]).
% iterate_cards([Card|Other_cards],Bikes,Possible_moves):-
%     get_bikes_positions(Bikes,Card,Moves_bikes,0),
%     iterate_bikes(Card,Moves_bikes,Bikes,[],Moves),
%     Possible_moves=[Moves|Other_moves],
%     iterate_cards(Other_cards,[Bike1,Bike2,Bike3],Other_moves),!.

% iterate_bikes(_,[],Acc,Acc).
% iterate_bikes(Card,[Current_bike_move|Other_bikes_move],Bikes_pos,Acc,Moves):-
%     nth0(1,Current_bike_move,Old_pos),
%     nth0(2,Current_bike_move,New_pos),
%     nth0(0,Current_bike_move,Index),
%     % là ya des soucis
%     Index_before is Index-1,
%     Index_after is Index+1,
%     findall(Elem,(between(0, Index_before, Index1), nth0(Index1,Bikes_pos,Elem)),Before),
%     findall(Elem,(between(Index_after,Length,Index1),nth0(Index1,Bikes_pos,Elem)),After),
%     length(Bikes_pos,Length),
%     append(Before,[New_pos|After],New_bike_pos),
%     append([Card,Old_pos,New_pos,New_bike_pos],Acc,New_acc),
%     iterate_bikes(Card,Other_bikes_move,Bikes_pos,New_acc,Moves).
    


% % (index,pos_base,new_pos)
% get_bikes_positions([],_,[]).
% get_bikes_positions([Bike|Other_bikes],Card,Moves,Index_bike):-
%     get_next_position(Bike,Card,New_pos),
%     Moves=[(Index_bike,Bike,New_pos)|Other_moves],
%     New_index is Index_bike+1,
%     get_bikes_positions(Other_bikes,Card,Other_moves,New_index).


% create_moves(1,['1-A-left','2-A-left'],0,[],A).
% A = [[1, 0, '1-A-left', '2-A-left'], [1, 1, '2-A-left', '3-A-left']].
create_moves(_,[],_,Acc,Acc).
create_moves(Card,[Bike|Other_bikes],Index,Acc,Moves):-
    get_next_position(Bike,Card,New_pos),
    Move=[[Card,Index,Bike,New_pos]],
    New_index is Index+1,
    append(Acc,Move,New_acc),
    create_moves(Card,Other_bikes,New_index,New_acc,Moves),!.
% gets the potential next positions of the player depending on the value of his cards
get_next_position(Position,0,Position).
get_next_position(Position,Card_value,Position):-
    end(Position).
get_next_position(Position,Card_value,Res):-
    next(Position,List_Next),
    member(Next,List_Next),
    New_value is Card_value-1,
    get_next_position(Next, New_value,Res).

% number of places available on the board
available_places('1-A-left',3).
available_places('2-A-left',3).
available_places('3-A-left',3).
available_places('4-A-left',3).
available_places('5-A-left',3).
available_places('6-A-left',3).
available_places('7-A-left',3).
available_places('8-A-left',3).
available_places('9-A-left',1).
available_places('9-B-left',1).
available_places('9-C-left',1).
available_places('10-A-left',1).
available_places('10-B-left',1).
available_places('10-C-left',1).
available_places('11-A-left',2).
available_places('12-A-left',2).
available_places('13-A-left',2).
available_places('14-A-left',2).
available_places('15-A-left',2).
available_places('16-A-left',2).
available_places('17-A-left',2).
available_places('18-A-left',2).
available_places('19-A-left',3).
available_places('20-A-left',3).
available_places('21-A-left',3).
available_places('22-A-left',3).
available_places('23-A-left',2).
available_places('24-A-left',2).
available_places('25-A-left',2).
available_places('26-A-left',1).
available_places('26-B-left',1).
available_places('27-A-left',1).
available_places('27-A-left',1).
available_places('28-A-left',2).
available_places('29-A-left',2).
available_places('30-A-left',2).
available_places('31-A-left',2).
available_places('32-A-left',2).
available_places('33-A-left',2).
available_places('34-A-left',2).
available_places('35-A-left',2).
available_places('23-A-right',1).
available_places('24-A-right',1).
available_places('25-A-right',1).
available_places('26-D-right',1).
available_places('26-C-right',1).
available_places('27-D-right',1).
available_places('27-C-right',1).
available_places('28-A-right',1).
available_places('29-A-right',1).
available_places('30-A-right',1).
available_places('31-A-right',1).
available_places('32-A-right',1).
available_places('33-A-right',1).
available_places('34-A-right',1).
available_places('35-A-right',1).
available_places('36-A-left',2).
available_places('37-A-left',2).
available_places('38-A-left',2).
available_places('39-A-left',2).
available_places('40-A-left',2).
available_places('41-A-left',2).
available_places('42-A-left',2).
available_places('43-A-left',2).
available_places('44-A-left',2).
available_places('45-A-left',2).
available_places('46-A-left',2).
available_places('47-A-left',2).
available_places('48-A-left',2).
available_places('49-A-left',2).
available_places('50-A-left',2).
available_places('51-A-left',2).
available_places('52-A-left',2).
available_places('53-A-left',2).
available_places('54-A-left',2).
available_places('55-A-left',2).
available_places('56-A-left',2).
available_places('57-A-left',2).
available_places('58-A-left',2).
available_places('59-A-left',2).
available_places('60-A-left',2).
available_places('61-A-left',2).
available_places('62-A-left',2).
available_places('63-A-left',1).
available_places('63-B-left',1).
available_places('63-C-left',1).
available_places('64-A-left',1).
available_places('64-B-left',1).
available_places('64-C-left',1).
available_places('65-A-left',2).
available_places('66-A-left',2).
available_places('67-A-left',2).
available_places('68-A-left',2).
available_places('69-A-left',2).
available_places('70-A-left',2).
available_places('71-A-left',2).
available_places('72-A-left',2).
available_places('73-A-left',1).
available_places('74-A-left',1).
available_places('75-A-left',1).
available_places('76-A-left',2).
available_places('77-A-left',2).
available_places('78-A-left',2).
available_places('79-A-left',2).
available_places('80-A-left',2).
available_places('81-A-left',2).
available_places('82-A-left',2).
available_places('83-A-left',2).
available_places('84-A-left',1).
available_places('85-A-left',1).
available_places('86-A-left',1).
available_places('87-A-left',1).
available_places('88-A-left',1).
available_places('89-A-left',1).
available_places('90-A-left',1).
available_places('91-A-left',1).
available_places('92-A-left',1).
available_places('93-A-left',1).
available_places('94-A-left',1).
available_places('84-A-right',1).
available_places('85-A-right',1).
available_places('86-A-right',1).
available_places('87-A-right',1).
available_places('88-A-right',1).
available_places('89-B-right',1).
available_places('89-C-right',1).
available_places('90-B-right',1).
available_places('90-C-right',1).
available_places('91-A-right',1).
available_places('92-A-right',1).
available_places('93-A-right',1).
available_places('94-A-right',1).
available_places('95-A-left',3).
available_places('0-A-left',3).
available_places('-1-A-left',3).
available_places('-2-A-left',3).
available_places('-3-A-left',3).
available_places('-4-A-left',3).
available_places('-5-A-left',3).
available_places('-6-A-left',3).
available_places('-7-A-left',3).
available_places('-8-A-left',3).
available_places('-9-A-left',3).

% linked cases on the board
next('1-A-left',['2-A-left']).
next('2-A-left',['3-A-left']).
next('3-A-left',['4-A-left']).
next('4-A-left',['5-A-left']).
next('5-A-left',['6-A-left']).
next('6-A-left',['7-A-left']).
next('7-A-left',['8-A-left']).
next('8-A-left',['9-A-left','9-C-left']).
next('9-A-left',['10-A-left','10-C-left']).
next('9-B-left',['10-A-left','10-C-left']).
next('9-C-left',['9-B-left']).
next('10-A-left',['11-A-left']).
next('10-B-left',['11-A-left']).
next('10-C-left',['10-B-left']).
next('11-A-left',['12-A-left']).
next('12-A-left',['13-A-left']).
next('13-A-left',['14-A-left']).
next('14-A-left',['15-A-left']).
next('15-A-left',['16-A-left']).
next('16-A-left',['17-A-left']).
next('17-A-left', ['18-A-left']).
next('18-A-left',['19-A-left']).
next('19-A-left',['20-A-left']).
next('20-A-left',['21-A-left']).
next('21-A-left',['22-A-left']).
next('22-A-left',['23-A-left','23-A-right']).
next('23-A-left',['24-A-left']).
next('24-A-left',['25-A-left']).
next('25-A-left',['26-A-left','26-B-left']).
next('26-A-left',['27-A-left','27-B-left']).
next('26-B-left',['27-A-left','27-B-left']).
next('27-A-left',['28-A-left']).
next('27-B-left',['28-A-left']).
next('28-A-left',['29-A-left']).
next('29-A-left',['30-A-left']).
next('30-A-left',['31-A-left']).
next('31-A-left',['32-A-left']).
next('32-A-left',['33-A-left']).
next('33-A-left',['34-A-left']).
next('34-A-left',['35-A-left']).
next('35-A-left',['36-A-left']).
next('23-A-right',['24-A-right']).
next('24-A-right',['25-A-right']).
next('25-A-right',['26-D-right']).
next('26-D-right',['26-C-right']).
next('26-C-right',['27-D-right']).
next('27-D-right',['27-C-right']).
next('27-C-right', ['28-A-right']).
next('28-A-right',['29-A-right']).
next('29-A-right',['30-A-right']).
next('30-A-right',['31-A-right']).
next('31-A-right',['32-A-right']).
next('32-A-right',['33-A-right']).
next('33-A-right',['34-A-right']).
next('34-A-right',['35-A-right']).
next('35-A-right',['36-A-left']).
next('36-A-left',['37-A-left']).
next('37-A-left',['38-A-left']).
next('38-A-left',['39-A-left']).
next('39-A-left',['40-A-left']).
next('40-A-left',['41-A-left']).
next('41-A-left',['42-A-left']).
next('42-A-left',['43-A-left']).
next('43-A-left',['44-A-left']).
next('44-A-left',['45-A-left']).
next('45-A-left',['46-A-left']).
next('46-A-left',['47-A-left']).
next('47-A-left',['48-A-left']).
next('48-A-left',['49-A-left']).
next('49-A-left',['50-A-left']).
next('50-A-left',['51-A-left']).
next('51-A-left',['52-A-left']).
next('52-A-left',['53-A-left']).
next('53-A-left',['54-A-left']).
next('54-A-left',['55-A-left']).
next('55-A-left',['56-A-left']).
next('56-A-left',['57-A-left']).
next('57-A-left',['58-A-left']).
next('58-A-left',['59-A-left']).
next('59-A-left',['60-A-left']).
next('60-A-left',['61-A-left']).
next('61-A-left',['62-A-left']).
next('62-A-left',['63-A-left','63-C-left']).
next('63-A-left',['64-A-left','64-C-left']).
next('63-B-left',['64-A-left','64-C-left']).
next('63-C-left',['63-B-left']).
next('64-A-left',['65-A-left']).
next('64-B-left',['65-A-left']).
next('64-C-left',['64-B-left']).
next('65-A-left',['66-A-left']).
next('66-A-left',['67-A-left']).
next('67-A-left',['68-A-left']).
next('68-A-left',['69-A-left']).
next('69-A-left',['70-A-left']).
next('70-A-left',['71-A-left']).
next('71-A-left',['72-A-left']).
next('72-A-left',['73-A-left']).
next('73-A-left',['74-A-left']).
next('74-A-left',['75-A-left']).
next('75-A-left',['76-A-left']).
next('76-A-left',['77-A-left']).
next('77-A-left',['78-A-left']).
next('78-A-left',['79-A-left']).
next('79-A-left',['80-A-left']).
next('80-A-left',['81-A-left']).
next('81-A-left',['82-A-left']).
next('83-A-left',['84-A-left','84-A-right']).
next('84-A-left',['85-A-left']).
next('85-A-left',['86-A-left']).
next('86-A-left',['87-A-left']).
next('87-A-left',['88-A-left']).
next('88-A-left',['89-A-left']).
next('89-A-left',['90-A-left']).
next('90-A-left',['91-A-left']).
next('91-A-left',['92-A-left']).
next('92-A-left',['93-A-left']).
next('93-A-left',['94-A-left']).
next('94-A-left',['95-A-left']).
next('84-A-right',['85-A-right']).
next('85-A-right',['86-A-right']).
next('86-A-right',['87-A-right']).
next('87-A-right',['88-A-right']).
next('88-A-right',['89-C-right']).
next('89-B-right',['90-C-right']).
next('89-C-right',['89-B-right']).
next('90-B-right',['91-A-right']).
next('90-C-right',['90-B-right']).
next('91-A-right',['92-A-right']).
next('92-A-right',['93-A-right']).
next('93-A-right',['94-A-right']).
next('94-A-right',['95-A-left']).
next('95-A-left',['0-A-left']).
next('0-A-left',['-1-A-left']).
next('-1-A-left',['-2-A-left']).
next('-2-A-left',['-3-A-left']).
next('-3-A-left',['-4-A-left']).
next('-4-A-left',['-5-A-left']).
next('-5-A-left',['-6-A-left']).
next('-6-A-left',['-7-A-left']).
next('-7-A-left',['-8-A-left']).
next('-8-A-left',['-9-A-left']).
% voir comment on fait avec le fait que ça soit vide pour la dernière
% il boucle sur lui-même :)
next('-9-A-left',['9-A-left']).

% end cases of the board
end('0-A-left').
end('-1-A-left').
end('-2-A-left').
end('-3-A-left').
end('-4-A-left').
end('-5-A-left').
end('-6-A-left').
end('-7-A-left').
end('-8-A-left').
end('-9-A-left').








% TODO
% rendre le code plus compréhensible en terme de noms de variables et commenter plus 


% aspirations dans le make move, voir en fct des règles
% voir si fini dès qu'un joueur en dehors et si on le considère comme restant sur le plateau ou non, mais ça devrait être ok avce le make move


% test iterate bikes

%  s'il est au dessus, 1, égalité 0, sinon -1
% voir si score spécial si chute, en vrai mieux je pense
score(State,1).
% surement split sur les - (mais si negatif ça va bug) et test si c'est plus grand ou pas

% test if a move can be played without causing a fall
% test_moves([],[]).
% % il faudra tester si on ne fait pas tomber d'autres vélos, donc surement passer le state en paramètre ici aussi
% test_moves(State,[Current_move|Other_moves],Moves_without_fall).
test_moves(State,Moves,Moves).