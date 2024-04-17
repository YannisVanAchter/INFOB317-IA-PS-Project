% en gros de ce que je comprends, c'est surtout la partie qui produit une réponse qui devra être refaite, parce que celle de Jcaquet est toute pétée et dégeu
% Question est une liste de mots
% Reponse est une liste de mots
% le cas de base est pour le moment de dire fin au bot
:- encoding(utf8).

reponse_autre(Reponse):-
    Reponse="Désolé, je n'ai pas de réponse à votre question.".
reponse("commence",Reponse):-
    Reponse="C'est au joueur ayant la plus haute carte secondes de commencer.".
reponse("équipe",Reponse):-
    Reponse="Chaque équipe est composée de 3 coureurs. Il y a au total 4 équipes.".
reponse("but",Reponse):-
    Reponse="Le but du jeu est d'obtenir le meilleur score possible.".
reponse("jaune",Reponse):-
    Reponse="Le maillot jaune appartient au coureur le plus rapide.".
reponse("vainqueur",Reponse):-
    Reponse="Le vainqueur est le joueur qui possède le meilleur temps et qui a le plus de points au classement.".
reponse("cartes",Reponse):-
    Reponse="Chauqe joueur reçoit 5 cartes par coureur en début de partie, puis 5 cartes par coureur quand elles sont toutes épuisées".
reponse("déplacer",Reponse):-
    Reponse="Un coureur peut être déplacé tout droit ou en diagonale mais pas sur le côté ou en arrière. Le coureur en tête est déplacé en premier, suivi du deuxième coureur, puis du troisème, etc.".
reponse("chute",Reponse):-
    Reponse="Une chure en série a lieu lorsqu'un coureur tente de passer sur la case d'un autre coureur ou de s'y arrêter. La chute a alors lieu sur toute la largeur de la routeet implique le coureur ayant provoqué la chute, le coureur avec lequel il ets entré en contact, tous les coureurs se trouvant sur une case adjacente portant le même numéro et tous les coureurs arrivant sur une case affectée lors de ce tour. Si les deux voies d'une route sont séparées par un terre-plein, l'autre côté de la route n'est alors pas affecté. Lorsqu'une chute en série à lieu, on place tous les coureurs concernés sur le bord de la route dans l'ordre de l'accident, du coureur ayant provoqué la chute au coureur arrivé en dernier. Les coureurs perdent alors 1 carte seconde, passent leur tour, et repartiront dans l'ordre dans lequel ils sont placés sur le bord de la route.".
reponse("priorité",Reponse):-
    Reponse="Un coureur situé sur une case numérotée a la priorité et passe avant les autres coureurs.".
reponse("aspiration",Reponse):-
    Reponse="Si un coureur est situé directement derrière un autre coureur ou un peloton, ce coureur peut gagner une seconde supplémentaite sur sa carte seconde si cela lui permet de se positionner derrière ou )à côté de l'autre coureur. Le coureur de tête ne peut pas profiter de ce phénomène. Les aspirations ne sont pas obligatoires.".
reponse("échange",Reponse):-
    Reponse="Une case échange permet de défausser 3 cartes et d'en repiocher 3 autres si le coureur s'arrête sur cette case. Si le joueur possède moins de 3 cartes dans sa main, celui-ci les défausse toutes et repioche le même nombre de cartes que celui qu'il possédait.".
reponse("chance",Reponse):-
    Reponse="Une carte chance doit être utilisée imméditamment après avoir été piochée, sauf mention contraire. Une carte chance ne provoque pas de chute en série si elle cause un déplacement, mais elle peut conduire vers une autre case où une chute a lieu, ou peut avoir pour but d'en causer une.".
reponse("sprint",Reponse):-
    Reponse="Lors d'un sprint, un ou plusieurs coureurs peuvent gagner des secondes qui seront décomptées du temps final du coureur.".
reponse("montées",Reponse):-
    Reponse="Les montées sont indiquées par des flèches rouges sur la carte. Elles ralentissent les coureurs de moitié. Un coureur en montée ne peut pas profiter d'une asipration.".
reponse("descente",Reponse):-
    Reponse="Les descentes sont indiquées par des flèches bleues sur la carte. Dans une descente, un coureur gagne 2 secondes et non une lors d'une aspiration et peut alors dépasser le coureur devant lui.".
reponse("dépassement",Reponse):-
    Reponse="Un coureur qui choisi d'avancer lentement doit prendre en compte les coureurs qui le suivent afin d'éviter une chute en série. Il doit alors laisser la possibilité au coureur qui le suit d'utiliser une carte 4 secondes, ou 6 en montée, sauf si le coureur derrière lui peut doubler normalement. Les coureurs suivants n'ont alors le droit de provoquer une chute que si cette derbière est inévitable.".
reponse("fin",Reponse):-
    Reponse="Le coureur qui termine en prmeier le plus loin derrière la ligne d'arrivée obtient le meilleur score en temr de temps et gagne l'étape. Une aspiration ne peut plus avoir lieu lorsque la ligne d'arrivée est passée, tout comme une chute en série. Si plusieurs coureurs obtiennent le même score, celui situé sur la case numérotée l'emporte. Un coureur passant la ligne d'arrivée après d'autres coureurs et obtenant un meilleur score ne l'emporte pas car il est arrivé à un tour ultérieur. Pour ce coureur, on ajoute donc 10 secondes supplémentaires à son score après chaque tour.".
reponse("gagne",Reponse):-
    Reponse="Le coureur qui arrive en premier le plus loin derrière la ligne d'arrivée remporte l'étape.".



mot_cle("gagne").
mot_cle("fin").
mot_cle("dépassement").
mot_cle("descente").
mot_cle("montées").
mot_cle("sprint").
mot_cle("chance").
mot_cle("échange").
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

lire_question(Input_question,Liste_question):-
    split_string(Input_question," ", "?,.;:!", Liste_question).


produire_reponse("au revoir",Reponse):-
    Reponse="Merci d'avoir sollicité mon aide!",!.

produire_reponse(Question,Reponse):-
    lire_question(Question,Mots_question),
    mot_cle(Mot1),
    member(Mot2,Mots_question),
    isub(Mot1,Mot2,Difference,[normalize(true)]),
    Difference>0.83,
    reponse(Mot1,Reponse),!.

produire_reponse(_,Reponse):-
    reponse_autre(Reponse).






    

    
    
    % % -----------------------------------------------------------------------
    % %     code de Jacquet pas trop pété qui devrait tenir le coup
    % % -----------------------------------------------------------------------
    
    % /* --------------------------------------------------------------------- */
    % /*                                                                       */
    % /*          CONVERSION D'UNE QUESTION DE L'UTILISATEUR EN                */
    % /*                        LISTE DE MOTS                                  */
    % /*                                                                       */
    % /* --------------------------------------------------------------------- */
    
    % % lire_question(L_Mots) 
    
    % lire_question(LMots) :- read_atomics(LMots).
    
    
    
    % /*****************************************************************************/
    % % my_char_type(+Char,?Type)
    % %    Char is an ASCII code.
    % %    Type is whitespace, punctuation, numeric, alphabetic, or special.
    
    % my_char_type(46,period) :- !.
    % my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
    % my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
    % my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
    % my_char_type(X,whitespace) :- X =< 32, !.
    % my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
    % my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
    % my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
    % my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
    % my_char_type(_,special).
    
    
    % /*****************************************************************************/
    % % lower_case(+C,?L)
    % %   If ASCII code C is an upper-case letter, then L is the
    % %   corresponding lower-case letter. Otherwise L=C.
    
    % lower_case(X,Y) :-
    %     X >= 65,
    %     X =< 90,
    %     Y is X + 32, !.
    
    % lower_case(X,X).
    
    
    % /*****************************************************************************/
    % % read_lc_string(-String)
    % %  Reads a line of input into String as a list of ASCII codes,
    % %  with all capital letters changed to lower case.
    
    % read_lc_string(String) :-
    %     get0(FirstChar),
    %     lower_case(FirstChar,LChar),
    %     read_lc_string_aux(LChar,String).
    
    % read_lc_string_aux(10,[]) :- !.  % end of line
    
    % read_lc_string_aux(-1,[]) :- !.  % end of file
    
    % read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).
    
    
    % /*****************************************************************************/
    % % extract_word(+String,-Rest,-Word) (final version)
    % %  Extracts the first Word from String; Rest is rest of String.
    % %  A word is a series of contiguous letters, or a series
    % %  of contiguous digits, or a single special character.
    % %  Assumes String does not begin with whitespace.
    
    % extract_word([C|Chars],Rest,[C|RestOfWord]) :-
    %     my_char_type(C,Type),
    %     extract_word_aux(Type,Chars,Rest,RestOfWord).
    
    % extract_word_aux(special,Rest,Rest,[]) :- !.
    %    % if Char is special, don't read more chars.
    
    % extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
    %     my_char_type(C,Type), !,
    %     extract_word_aux(Type,Chars,Rest,RestOfWord).
    
    % extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.
    
    
    % /*****************************************************************************/
    % % remove_initial_blanks(+X,?Y)
    % %   Removes whitespace characters from the
    % %   beginning of string X, giving string Y.
    
    % remove_initial_blanks([C|Chars],Result) :-
    %     my_char_type(C,whitespace), !,
    %     remove_initial_blanks(Chars,Result).
    
    % remove_initial_blanks(X,X).   % if previous clause did not succeed.
    
    
    % /*****************************************************************************/
    % % digit_value(?D,?V)
    % %  Where D is the ASCII code of a digit,
    % %  V is the corresponding number.
    
    % digit_value(48,0).
    % digit_value(49,1).
    % digit_value(50,2).
    % digit_value(51,3).
    % digit_value(52,4).
    % digit_value(53,5).
    % digit_value(54,6).
    % digit_value(55,7).
    % digit_value(56,8).
    % digit_value(57,9).
    
    
    % /*****************************************************************************/
    % % string_to_number(+S,-N)
    % %  Converts string S to the number that it
    % %  represents, e.g., "234" to 234.
    % %  Fails if S does not represent a nonnegative integer.
    
    % string_to_number(S,N) :-
    %     string_to_number_aux(S,0,N).
    
    % string_to_number_aux([D|Digits],ValueSoFar,Result) :-
    %     digit_value(D,V),
    %     NewValueSoFar is 10*ValueSoFar + V,
    %     string_to_number_aux(Digits,NewValueSoFar,Result).
    
    % string_to_number_aux([],Result,Result).
    
    
    % /*****************************************************************************/
    % % string_to_atomic(+String,-Atomic)
    % %  Converts String into the atom or number of
    % %  which it is the written representation.
    
    % string_to_atomic([C|Chars],Number) :-
    %     string_to_number([C|Chars],Number), !.
    
    % string_to_atomic(String,Atom) :- name(Atom,String).
    %   % assuming previous clause failed.
    
    
    % /*****************************************************************************/
    % % extract_atomics(+String,-ListOfAtomics) (second version)
    % %  Breaks String up into ListOfAtomics
    % %  e.g., " abc def  123 " into [abc,def,123].
    
    % extract_atomics(String,ListOfAtomics) :-
    %     remove_initial_blanks(String,NewString),
    %     extract_atomics_aux(NewString,ListOfAtomics).
    
    % extract_atomics_aux([C|Chars],[A|Atomics]) :-
    %     extract_word([C|Chars],Rest,Word),
    %     string_to_atomic(Word,A),       % <- this is the only change
    %     extract_atomics(Rest,Atomics).
    
    % extract_atomics_aux([],[]).
    
    
    % /*****************************************************************************/
    % % clean_string(+String,-Cleanstring)
    % %  removes all punctuation characters from String and return Cleanstring
    
    % clean_string([C|Chars],L) :-
    %     my_char_type(C,punctuation),
    %     clean_string(Chars,L), !.
    % clean_string([C|Chars],[C|L]) :-
    %     clean_string(Chars,L), !.
    % clean_string([C|[]],[]) :-
    %     my_char_type(C,punctuation), !.
    % clean_string([C|[]],[C]).
    
    
    % /*****************************************************************************/
    % % read_atomics(-ListOfAtomics)
    % %  Reads a line of input, removes all punctuation characters, and converts
    % %  it into a list of atomic terms, e.g., [this,is,an,example].
    
    % read_atomics(ListOfAtomics) :-
    %     read_lc_string(String),
    %     clean_string(String,Cleanstring),
    %     extract_atomics(Cleanstring,ListOfAtomics).
    
    
    
    % /* --------------------------------------------------------------------- */
    % /*                                                                       */
    % /*        ECRIRE_REPONSE : ecrit une suite de lignes de texte            */
    % /*                                                                       */
    % /* --------------------------------------------------------------------- */
    
    % ecrire_reponse(L) :-
    %    nl, write('TBot :'),
    %    ecrire_li_reponse(L,1,1).
    
    % % ecrire_li_reponse(Ll,M,E)
    % % input : Ll, liste de listes de mots (tout en minuscules)
    % %         M, indique si le premier caractere du premier mot de 
    % %            la premiere ligne doit etre mis en majuscule (1 si oui, 0 si non)
    % %         E, indique le nombre d'espaces avant ce premier mot 
    
    % ecrire_li_reponse([],_,_) :- 
    %     nl.
    
    % ecrire_li_reponse([Li|Lls],Mi,Ei) :- 
    %    ecrire_ligne(Li,Mi,Ei,Mf),
    %    ecrire_li_reponse(Lls,Mf,2).
    
    % % ecrire_ligne(Li,Mi,Ei,Mf)
    % % input : Li, liste de mots a ecrire
    % %         Mi, Ei booleens tels que decrits ci-dessus
    % % output : Mf, booleen tel que decrit ci-dessus a appliquer 
    % %          a la ligne suivante, si elle existe
    
    % ecrire_ligne([],M,_,M) :- 
    %    nl.
    
    % ecrire_ligne([M|L],Mi,Ei,Mf) :-
    %    ecrire_mot(M,Mi,Maux,Ei,Eaux),
    %    ecrire_ligne(L,Maux,Eaux,Mf).
    
    % % ecrire_mot(M,B1,B2,E1,E2)
    % % input : M, le mot a ecrire
    % %         B1, indique s'il faut une majuscule (1 si oui, 0 si non)
    % %         E1, indique s'il faut un espace avant le mot (1 si oui, 0 si non)
    % % output : B2, indique si le mot suivant prend une majuscule
    % %          E2, indique si le mot suivant doit etre precede d'un espace
    
    % ecrire_mot('.',_,1,_,1) :-
    %    write('. '), !.
    % ecrire_mot('\'',X,X,_,0) :-
    %    write('\''), !.
    % ecrire_mot(',',X,X,E,1) :-
    %    espace(E), write(','), !.
    % ecrire_mot(M,0,0,E,1) :-
    %    espace(E), write(M).
    % ecrire_mot(M,1,0,E,1) :-
    %    name(M,[C|L]),
    %    D is C - 32,
    %    name(N,[D|L]),
    %    espace(E), write(N).
    
    % espace(0).
    % espace(N) :- N>0, Nn is N-1, write(' '), espace(Nn).
    
    
    % /* --------------------------------------------------------------------- */
    % /*                                                                       */
    % /*                            TEST DE FIN                                */
    % /*                                                                       */
    % /* --------------------------------------------------------------------- */
    
    % fin(L) :- member(fin,L).
    
    
    % /* --------------------------------------------------------------------- */
    % /*                                                                       */
    % /*                         BOUCLE PRINCIPALE                             */
    % /*                                                                       */
    % /* --------------------------------------------------------------------- */
    
    % tourdefrance :- 
    
    %    nl, nl, nl,
    %    write('Bonjour, je suis TBot, le bot explicateur du Tour de France.'), nl,
    %    write('En quoi puis-je vous aider ?'), 
    %    nl, nl, 
    
    %    repeat,
    %       write('Vous : '), ttyflush,
    %       lire_question(L_Mots),
    %       produire_reponse(L_Mots,L_ligne_reponse),
    %       ecrire_reponse(L_ligne_reponse),
    %    fin(L_Mots), !.
       
    
    % /* --------------------------------------------------------------------- */
    % /*                                                                       */
    % /*             ACTIVATION DU PROGRAMME APRES COMPILATION                 */
    % /*                                                                       */
    % /* --------------------------------------------------------------------- */
    
    % % :- tourdefrance.