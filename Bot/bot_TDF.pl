% en gros de ce que je comprends, c'est surtout la partie qui produit une réponse qui devra être refaite, parce que celle de Jcaquet est toute pétée et dégeu
% Question est une liste de mots
% Reponse est une liste de mots
% le cas de base est pour le moment de dire fin au bot

% il faudra une liste des questions qu'un utilisateur peut poser, donc voir les règles


    %  cas de base
produire_reponse([fin],Reponse):-
    Reponse=[fin,de,la,conversation].

    % pour produire une réponse, il faut faut
    % -trouver le mot clé de la question
    % -appeler une règle qui correspond à ce mot clé
    % je comprends pas pourquoi le match pattenr de Jacquet? Voir si la question correspond à un pattern, si on a un mot clé, pourquoi?
    % peut-être que ce sera utile dans le cas où on a deux questions un peu similaire, à voir, mais pour ça il faudra la liste des questions

produire_reponse(Question,Reponse):-
    mot_cle(Mot),member(Mot,Question),
    reponse(Mot,Reponse).


mot_cle(commence).

reponse(commence,Reponse):-
    Reponse=[ "c'est", au, joueur, ayant, la, plus, haute, carte, secondes, de ,"commencer." ].

mot_cle(equipe).

reponse(equipe,Reponse):-
    Reponse=[ chaque, equipe, compte, X, "coureurs." ],nb_coureurs(X).

nb_coureurs(3).
nb_equipes(4).

mot_cle(but).

reponse(but,Reponse):-
    Reponse= [le, but, du, jeu, est, "d'obtenir", le, meilleur, score, possible].

mot_cle(jaune).

reponse(jaune,Reponse):-
    Reponse=[le, maillot, jaune, appartient, au, coureur, le, plus, rapide].

mot_cle(vainqueur).

reponse(vainqueur,Reponse):-
    Reponse=[le, vainqueur, est, le, joueur, qui, "possède", le, meilleur, temps, et, qui, a, le, plus, de, points, au, classement].

mot_cle(cartes).

reponse(cartes,Reponse):-
    Reponse=[chaque, joueur, "reçoit", 5, cartes, par, coureur, en, "début", de, partie, ",", puis, 5, cartes, par, coureur, quand, elles, sont, toutes,"épuisées"].

% l'accent de déplacer, c'est un peu compliqué pour prolog...
mot_cle(deplacer).

reponse(deplacer,Reponse):-
    Reponse=[un, coureur, peut, "être", "déplacé", tout, droit, ou, en, diagonale, mais, pas, sur, le, "côté", ou, en, "arrière",".",le, coureur, en ,"tête", est, "déplacé", en, premier,",",suivi, du, coureur, 2, puis, 3, etc,"."].

mot_cle(chute).

reponse(chute,Reponse):-
    Reponse=[une, chute,en,"série",a,lieu,"lorsqu'un",coureur,tente,de,passer,sur,la,case,"d'un",autre,coureur,ou,de,"s'y","arrêter",".",le,chute,a,alors,lieu,sur,toute,la,largeur,de,la,route,et,implique,le,coureur,ayant,"provoqué",la,chute,",",le,coureur,avec,lequel,il,est,"entré",en,contact,",",tous,les,coureurs,se,trouvant,sur,une,case,adjacente,portant,le,"même","numéro",et,tous,les,coureurs,arrivant,sur,une,case,"affectée",lors,de,ce,tour,".",si,les,deux,voies,"d'une",route,sont,"séparées",par,un,"terre-plein","l'autre","côté",de,la,route,"n'est",alors,pas,"affectéé,",".","lorsqu'une",chute,en,"série",a,lieu,on,place,tous,les,coureurs,"concernés",sur,le,bord,de,la,route,dans,"l'ordre",de,"l'accident",du,coureur,ayant,"provoqué",la,chute,au,coureur,"arrivé",en,dernier,".",les,coureurs,perdent,alors,1,carte,seconde,et,repartiront,dans,"l'ordre",dans,lesquel,ils,sont,"placés",sur,le,bord,de,la,route].

mot_cle(priorite).

reponse(priorite,Reponse):-
    Reponse=[un, coureur, "situé",sur,une,case,"numérotée",a,la,"priorité",et,passe,avant,les,autres,coureurs].

mot_cle(aspiration).

reponse(aspiration,Reponse):-
    Reponse=[si,un,coureur,est,"situé",directement,"derrière",un,autre,coureur,ou,un,peloton,",",ce,coureur,peut,gagner,une,seconde,"supllémentaire",sur,sa,carte,seconde,si,cela,lui,permet,de,se,positioner,"derrière",ou,"à","côté",de,"l'autre",coureur,".",le,coureur,de,"tête",ne,peut,pas,profiter,de,ce,"phénomène",".",les,aspirations,ne,sont,pas,obligatoires].

mot_cle(echange).

reponse(echange,Reponse):-
    Reponse=[une,case,"échange",permet,de,"défauser",3,cartes,et,"d'en",repriocher,3,autres,si,le,coureur,"s'arrête",sur,cette,case,".",si,le,joueur,"possède",moins,de,3,cartes,",","celui-ci",les,"défausse",toutes,et,repioche,le,"même",nombre,de,cartes,que,celui,"qu'il","possdédait",ensuite].

mot_cle(chance).

reponse(chance,Reponse):-
    Reponse=[une,carte,chance,doit,"être","utilisée","imméditamment","après",avoir,"été","piochée",",",sauf,mention,contraire,".",une,carte,chance,ne,provoque,pas,de,chute,en,"série",si,elle,cause,un,"déplacement",mais,elle,peut,conduire,vers,une,case,"où",une,chute,a,eu,lieu,",",ou,peut,avoir,pour, but,"d'en",causer,une].

mot_cle(sprint).

reponse(sprint,Reponse):-
    Reponse=[lors,"d'un",sprint,1,ou,plusieurs,coureurs,peuvent,gagner,des,secondes,qui,seront,"décomptées",du,temps,final,du,coureur].

mot_cle(montees).

reponse(montees,Reponse):-
    Reponse=[les,"montées",sont,"indiquées",par,des,"flèches",rouges,sur,la,carte,".",elles,ralentissent,les,coureurs,de,"moitié",".",un,coureur,en,"montée",ne,peut,pas,profiter,"d'une",apsiration].

mot_cle(descente).

reponse(descente,Reponse):-
    Reponse=[les,descentes,sont,"indiquées",par,des,"flèches",bleues,sur,la,carte,".",dans,une,descente,un,coureur,gagne,2,secondes,et,non,une,lors,"d'un",apsiration,et,peut,alors,"dépasser",le,coureur,devant,lui].

mot_cle(depassement).

reponse(depassement,Reponse):-
    Reponse=[un,coureur,qui,choisi,"d'avancer",lentement,doit,prendre,en,compte,les,coureurs,qui,le,suivent,afin,"d'éviter",une,chute,".",il,doit,alors,laisser,la,"possibilité",au,coureur,qui,le,suit,"d'utiliser",une,carte,4,secondes,ou,6,en,"montée",sauf,si,le,coureur,"derrière",lui,peut,le,doubler,normalement,".",les,coureurs,suivants,"n'ont",alors,le,droit,de,provoquer,une,chute,que,si,cette,"dernière",est,"inévitable"].

mot_cle(fin).

reponse(fin,Reponse):-
    Reponse=[le,coureur,qui,termine,en,premier,le,plus,loin,"derrière",la,ligne,"d'arrivée",obtient,le,meilleur,score,en,terme,de,temps,et,gagne,"l'étape",".",une,apsiration,ne,peut,plus,avoir,lieu,lorsque,la,ligne,"d'arrivée",est,"passée",tout,comme,une,chute,en,"série",".",si,plusieurs,coureurs,obtiennent,le,"même",score,celui,"situé",sur,la,case,"numérotée","l'emporte",".",un,coureur,passant,la,ligne,"d'arrivée","après","d'autres",coureurs,et,obtenant,un,meilleur,score,ne,"l'emporte",pas,car,il,est,"arrivé","à",un,tour,"ultérieur",".",pour,ce,coureur,on,ajoute,donc,10,secondes,"supplémentaires","à",son,score,"après",chaque,tour].

mot_cle(gagne).

reponse(gagne,Reponse):-
    Reponse=[le,coureur,qui,arrive,en,premier,le,plus,loin,"derrière",la,ligne,"d'arrivée",remporte,"l'étape"].




    
    
    % -----------------------------------------------------------------------
    %     code de Jacquet pas trop pété qui devrait tenir le coup
    % -----------------------------------------------------------------------
    
    /* --------------------------------------------------------------------- */
    /*                                                                       */
    /*          CONVERSION D'UNE QUESTION DE L'UTILISATEUR EN                */
    /*                        LISTE DE MOTS                                  */
    /*                                                                       */
    /* --------------------------------------------------------------------- */
    
    % lire_question(L_Mots) 
    
    lire_question(LMots) :- read_atomics(LMots).
    
    
    
    /*****************************************************************************/
    % my_char_type(+Char,?Type)
    %    Char is an ASCII code.
    %    Type is whitespace, punctuation, numeric, alphabetic, or special.
    
    my_char_type(46,period) :- !.
    my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
    my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
    my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
    my_char_type(X,whitespace) :- X =< 32, !.
    my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
    my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
    my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
    my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
    my_char_type(_,special).
    
    
    /*****************************************************************************/
    % lower_case(+C,?L)
    %   If ASCII code C is an upper-case letter, then L is the
    %   corresponding lower-case letter. Otherwise L=C.
    
    lower_case(X,Y) :-
        X >= 65,
        X =< 90,
        Y is X + 32, !.
    
    lower_case(X,X).
    
    
    /*****************************************************************************/
    % read_lc_string(-String)
    %  Reads a line of input into String as a list of ASCII codes,
    %  with all capital letters changed to lower case.
    
    read_lc_string(String) :-
        get0(FirstChar),
        lower_case(FirstChar,LChar),
        read_lc_string_aux(LChar,String).
    
    read_lc_string_aux(10,[]) :- !.  % end of line
    
    read_lc_string_aux(-1,[]) :- !.  % end of file
    
    read_lc_string_aux(LChar,[LChar|Rest]) :- read_lc_string(Rest).
    
    
    /*****************************************************************************/
    % extract_word(+String,-Rest,-Word) (final version)
    %  Extracts the first Word from String; Rest is rest of String.
    %  A word is a series of contiguous letters, or a series
    %  of contiguous digits, or a single special character.
    %  Assumes String does not begin with whitespace.
    
    extract_word([C|Chars],Rest,[C|RestOfWord]) :-
        my_char_type(C,Type),
        extract_word_aux(Type,Chars,Rest,RestOfWord).
    
    extract_word_aux(special,Rest,Rest,[]) :- !.
       % if Char is special, don't read more chars.
    
    extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
        my_char_type(C,Type), !,
        extract_word_aux(Type,Chars,Rest,RestOfWord).
    
    extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.
    
    
    /*****************************************************************************/
    % remove_initial_blanks(+X,?Y)
    %   Removes whitespace characters from the
    %   beginning of string X, giving string Y.
    
    remove_initial_blanks([C|Chars],Result) :-
        my_char_type(C,whitespace), !,
        remove_initial_blanks(Chars,Result).
    
    remove_initial_blanks(X,X).   % if previous clause did not succeed.
    
    
    /*****************************************************************************/
    % digit_value(?D,?V)
    %  Where D is the ASCII code of a digit,
    %  V is the corresponding number.
    
    digit_value(48,0).
    digit_value(49,1).
    digit_value(50,2).
    digit_value(51,3).
    digit_value(52,4).
    digit_value(53,5).
    digit_value(54,6).
    digit_value(55,7).
    digit_value(56,8).
    digit_value(57,9).
    
    
    /*****************************************************************************/
    % string_to_number(+S,-N)
    %  Converts string S to the number that it
    %  represents, e.g., "234" to 234.
    %  Fails if S does not represent a nonnegative integer.
    
    string_to_number(S,N) :-
        string_to_number_aux(S,0,N).
    
    string_to_number_aux([D|Digits],ValueSoFar,Result) :-
        digit_value(D,V),
        NewValueSoFar is 10*ValueSoFar + V,
        string_to_number_aux(Digits,NewValueSoFar,Result).
    
    string_to_number_aux([],Result,Result).
    
    
    /*****************************************************************************/
    % string_to_atomic(+String,-Atomic)
    %  Converts String into the atom or number of
    %  which it is the written representation.
    
    string_to_atomic([C|Chars],Number) :-
        string_to_number([C|Chars],Number), !.
    
    string_to_atomic(String,Atom) :- name(Atom,String).
      % assuming previous clause failed.
    
    
    /*****************************************************************************/
    % extract_atomics(+String,-ListOfAtomics) (second version)
    %  Breaks String up into ListOfAtomics
    %  e.g., " abc def  123 " into [abc,def,123].
    
    extract_atomics(String,ListOfAtomics) :-
        remove_initial_blanks(String,NewString),
        extract_atomics_aux(NewString,ListOfAtomics).
    
    extract_atomics_aux([C|Chars],[A|Atomics]) :-
        extract_word([C|Chars],Rest,Word),
        string_to_atomic(Word,A),       % <- this is the only change
        extract_atomics(Rest,Atomics).
    
    extract_atomics_aux([],[]).
    
    
    /*****************************************************************************/
    % clean_string(+String,-Cleanstring)
    %  removes all punctuation characters from String and return Cleanstring
    
    clean_string([C|Chars],L) :-
        my_char_type(C,punctuation),
        clean_string(Chars,L), !.
    clean_string([C|Chars],[C|L]) :-
        clean_string(Chars,L), !.
    clean_string([C|[]],[]) :-
        my_char_type(C,punctuation), !.
    clean_string([C|[]],[C]).
    
    
    /*****************************************************************************/
    % read_atomics(-ListOfAtomics)
    %  Reads a line of input, removes all punctuation characters, and converts
    %  it into a list of atomic terms, e.g., [this,is,an,example].
    
    read_atomics(ListOfAtomics) :-
        read_lc_string(String),
        clean_string(String,Cleanstring),
        extract_atomics(Cleanstring,ListOfAtomics).
    
    
    
    /* --------------------------------------------------------------------- */
    /*                                                                       */
    /*        ECRIRE_REPONSE : ecrit une suite de lignes de texte            */
    /*                                                                       */
    /* --------------------------------------------------------------------- */
    
    ecrire_reponse(L) :-
       nl, write('TBot :'),
       ecrire_li_reponse(L,1,1).
    
    % ecrire_li_reponse(Ll,M,E)
    % input : Ll, liste de listes de mots (tout en minuscules)
    %         M, indique si le premier caractere du premier mot de 
    %            la premiere ligne doit etre mis en majuscule (1 si oui, 0 si non)
    %         E, indique le nombre d'espaces avant ce premier mot 
    
    ecrire_li_reponse([],_,_) :- 
        nl.
    
    ecrire_li_reponse([Li|Lls],Mi,Ei) :- 
       ecrire_ligne(Li,Mi,Ei,Mf),
       ecrire_li_reponse(Lls,Mf,2).
    
    % ecrire_ligne(Li,Mi,Ei,Mf)
    % input : Li, liste de mots a ecrire
    %         Mi, Ei booleens tels que decrits ci-dessus
    % output : Mf, booleen tel que decrit ci-dessus a appliquer 
    %          a la ligne suivante, si elle existe
    
    ecrire_ligne([],M,_,M) :- 
       nl.
    
    ecrire_ligne([M|L],Mi,Ei,Mf) :-
       ecrire_mot(M,Mi,Maux,Ei,Eaux),
       ecrire_ligne(L,Maux,Eaux,Mf).
    
    % ecrire_mot(M,B1,B2,E1,E2)
    % input : M, le mot a ecrire
    %         B1, indique s'il faut une majuscule (1 si oui, 0 si non)
    %         E1, indique s'il faut un espace avant le mot (1 si oui, 0 si non)
    % output : B2, indique si le mot suivant prend une majuscule
    %          E2, indique si le mot suivant doit etre precede d'un espace
    
    ecrire_mot('.',_,1,_,1) :-
       write('. '), !.
    ecrire_mot('\'',X,X,_,0) :-
       write('\''), !.
    ecrire_mot(',',X,X,E,1) :-
       espace(E), write(','), !.
    ecrire_mot(M,0,0,E,1) :-
       espace(E), write(M).
    ecrire_mot(M,1,0,E,1) :-
       name(M,[C|L]),
       D is C - 32,
       name(N,[D|L]),
       espace(E), write(N).
    
    espace(0).
    espace(N) :- N>0, Nn is N-1, write(' '), espace(Nn).
    
    
    /* --------------------------------------------------------------------- */
    /*                                                                       */
    /*                            TEST DE FIN                                */
    /*                                                                       */
    /* --------------------------------------------------------------------- */
    
    fin(L) :- member(fin,L).
    
    
    /* --------------------------------------------------------------------- */
    /*                                                                       */
    /*                         BOUCLE PRINCIPALE                             */
    /*                                                                       */
    /* --------------------------------------------------------------------- */
    
    tourdefrance :- 
    
       nl, nl, nl,
       write('Bonjour, je suis TBot, le bot explicateur du Tour de France.'), nl,
       write('En quoi puis-je vous aider ?'), 
       nl, nl, 
    
       repeat,
          write('Vous : '), ttyflush,
          lire_question(L_Mots),
          produire_reponse(L_Mots,L_ligne_reponse),
          ecrire_reponse(L_ligne_reponse),
       fin(L_Mots), !.
       
    
    /* --------------------------------------------------------------------- */
    /*                                                                       */
    /*             ACTIVATION DU PROGRAMME APRES COMPILATION                 */
    /*                                                                       */
    /* --------------------------------------------------------------------- */
    
    % :- tourdefrance.