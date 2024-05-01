:- module(bot_TDF, [produire_reponse/2]).
:- encoding(utf8).

%  mots-clés selon lesquels les réponses seront générées
mot_cle("gagne").
mot_cle("fin").
mot_cle("dépassement").
mot_cle("sprint").
mot_cle("chance").
mot_cle("aspiration").
mot_cle("priorité").
mot_cle("chute").
mot_cle("déplacer").
mot_cle("cartes").
mot_cle("vainqueur").
mot_cle("jaune").
mot_cle("but").
mot_cle("équipe").
mot_cle("commence").

% liste de réponses associées aux mots-clés
% pour la réponse par défaut, il n'y a pas de mot-clé
reponse("commence",Reponse):-
    Reponse="C'est au joueur ayant la plus haute carte secondes de commencer.".
reponse("équipe",Reponse):-
    Reponse="Chaque équipe est composée de 3 coureurs. Il y a au total 4 équipes.".
reponse("but",Reponse):-
    Reponse="Le but du jeu est d'obtenir le meilleur temps possible.".
reponse("jaune",Reponse):-
    Reponse="Le maillot jaune appartient au coureur le plus rapide.".
reponse("vainqueur",Reponse):-
    Reponse="Le vainqueur est le joueur qui possède le meilleur temps.".
reponse("cartes",Reponse):-
    Reponse="Chaque joueur reçoit 5 cartes en début de partie, puis 5 autres cartes quand elles sont toutes épuisées.".
reponse("déplacer",Reponse):-
    Reponse="Un coureur peut être déplacé tout droit ou en diagonale mais pas sur le côté ou en arrière. On déplace les coureurs par équipe, en commencant par l'équipe du coureur en tête.".
reponse("chute",Reponse):-
    Reponse="Une chute en série a lieu lorsqu'un coureur tente de passer sur la case d'un autre coureur ou de s'y arrêter. La chute a alors lieu sur toute la largeur de la route et implique le coureur ayant provoqué la chute, le coureur avec lequel il est entré en contact, tous les coureurs se trouvant sur une case adjacente portant le même numéro et tous les coureurs arrivant sur une case affectée lors de ce tour. Si les deux voies d'une route sont séparées par un terre-plein, l'autre côté de la route n'est alors pas affecté. Lorsqu'une chute en série à lieu, on place tous les coureurs concernés sur le bord de la route dans l'ordre de l'accident, du coureur ayant provoqué la chute au coureur arrivé en dernier. Les coureurs perdent alors 1 carte seconde, passent leur tour, et repartiront dans l'ordre dans lequel ils sont placés sur le bord de la route.".
reponse("priorité",Reponse):-
    Reponse="Un coureur situé sur une case numérotée a la priorité et passe avant les autres coureurs.".
reponse("aspiration",Reponse):-
    Reponse="Si un coureur est situé directement derrière un autre coureur ou un peloton, ce coureur peut gagner une seconde supplémentaire sur sa carte seconde si cela lui permet de se positionner derrière ou à côté de l'autre coureur. Le coureur de tête ne peut pas profiter de ce phénomène. Les aspirations ne sont pas obligatoires.".
reponse("chance",Reponse):-
    Reponse="Une carte chance doit être utilisée imméditamment après avoir été piochée, sauf mention contraire. Une carte chance ne provoque pas de chute en série si elle cause un déplacement, mais elle peut conduire vers une autre case où une chute a lieu, ou peut avoir pour but d'en causer une. Une case chance fait se déplacer un coureur de 1 à 3 cases vers l'avant ou vers l'arrière.".
reponse("sprint",Reponse):-
    Reponse="Lors d'un sprint, un ou plusieurs coureurs peuvent gagner des secondes qui seront décomptées du temps final du coureur.".
reponse("dépassement",Reponse):-
    Reponse="Un coureur qui choisi d'avancer lentement doit prendre en compte les coureurs qui le suivent afin d'éviter une chute en série. Il doit alors laisser la possibilité au coureur qui le suit d'utiliser une carte 4 secondes sauf si le coureur derrière lui peut doubler normalement. Les coureurs suivants n'ont alors le droit de provoquer une chute que si cette dernière est inévitable.".
reponse("fin",Reponse):-
    Reponse="Le coureur qui termine en premier le plus loin derrière la ligne d'arrivée obtient le meilleur score en terme de temps et gagne l'étape. Une aspiration ne peut plus avoir lieu lorsque la ligne d'arrivée est passée, tout comme une chute en série. Si plusieurs coureurs obtiennent le même score, celui situé sur la case numérotée l'emporte. Un coureur passant la ligne d'arrivée après d'autres coureurs et obtenant un meilleur score ne l'emporte pas car il est arrivé à un tour ultérieur. Pour ce coureur, on ajoute donc 10 secondes supplémentaires à son score après chaque tour.".
reponse("gagne",Reponse):-
    Reponse="Le coureur qui arrive en premier le plus loin derrière la ligne d'arrivée remporte l'étape.".
reponse_autre(Reponse):-
    Reponse="Désolé, je n'ai pas de réponse à votre question.".


% lis la question de l'utilisteur
% la question en input est split sur les espaces, tout en enlevant la ponctuation, pour récupérer une liste de mots
lire_question(Input_question,Liste_question):-
    split_string(Input_question," ", "?,.;:!", Liste_question).


% produit la réponse à la question
% on test la distance entre les mots de la question et les mots-clés afin de passer outre les fautes de frappe
produire_reponse("au revoir",Reponse):-
    Reponse="Merci d'avoir sollicité mon aide!",!.

produire_reponse(Question,Reponse):-
    % produit la liste des mots de la question
    lire_question(Question,Mots_question),
    % test si un mot de la question correspond, moyennant erreur de frappe, à un mot clé
    mot_cle(Mot1),
    member(Mot2,Mots_question),
    isub(Mot1,Mot2,Difference,[normalize(true)]),
    Difference>0.83,
    % produit la réponse si le mot clé a été identifié
    reponse(Mot1,Reponse),!.

produire_reponse(_,Reponse):-
    reponse_autre(Reponse).

% fonction principale
% tourdefrance():-
%     writeln("Bonjour!"),
%     writeln("comment puis-je vous aider?"),
%     repeat,
%     write("Vous :"),
%     ttyflush,
%     lire_question
