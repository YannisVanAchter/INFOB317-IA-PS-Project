:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library(http/http_cors)). %module pour éviter l'erreur "Reason: CORS header ‘Access-Control-Allow-Origin’ missing" 
:- use_module(bot_TDF). %module pour le bot
:- use_module(ia). %module pour l'ia
%-------------------------------------- 
%set up du serveur
:- initialization set_setting(http:cors, [*]).
:- initialization http_server([port(8080)]). 

%différentes root 
:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]). %redirige vers la page d'accueil
:- http_handler(root(home), home_page, []). %page d'accueil (documentation de l'API)
:- http_handler(root(bot/Question), answer(Question),[]). %root pour le bot
:- http_handler(root(ia), answer_ia(Method), [method(Method), methods([post, options])]). %root pour l'ia

home_page(_Request) :- %page d'accueil permetaant de voir la documentation de notre API
    reply_html_page(
    title('API Tour de France Documentation'),
    [ 
        h1('API Tour de France Documentation'),
        p('This is the API for the Tour de France project.'),
        h2('Bot'),
        p('To use the bot, go to /bot/Question where Question is the question you want to ask the bot.'),
        p('The bot will return the answer to the question throught a JSON file. The answer is in the field "answer".'),
        h2('IA'),
        p('To use the IA, send a get request to /ia with the data of the game board.')
    ]).
%---------------------------------------
%bot
answer(Question, _Request):- %predicat pour le bot
    produire_reponse(Question, Resp), %on récupère la réponse du bot
    cors_enable, 
    reply_json(json([answer=Resp])). %on renvoie la réponse du bot

%---------------------------------------
%pour tester l'IA directement par prolog
test_ia(Data, Response) :-
    spliter(Data, Board), %on extrait les données du string
    write("Board: "), writeln(Board),
    get_move_IA(Board, Move), %On recupère le move de l'ia
    write("Move: "), writeln(Move),
    format_move(Move, NewMove), %on met les infos du move dans une liste
    move_to_char(NewMove, Response), %on tranforme la liste en string
    write("Response: "), writeln(Response),
    reply_json(json([response=Response])). %on renvoie le json
    
%IA
answer_ia(post, Request) :- %predicat pour extraire le json de la requête
    cors_enable,
    http_read_data(Request, Data, []), %on lit les données de la requête
    spliter(Data, Board), %on extrait les données dans une liste de liste pour que l'ia puisse les utiliser
    get_move_IA(Board, Move), %On recupère le move de l'ia
    format_move(Move, NewMove), %on met les infos du move dans une liste
    move_to_char(NewMove, Response), %on tranforme la liste en string
    reply_json(json([response=Response])). %on renvoie le json

%---------------------------------------
%Pour extraire les données qui sont en texte brut (string) et renvoyer en liste de liste pour que l'IA puisse les utiliser

%1. Récupérer les données en string (http_read_data) 
%2. Split les données à chaque "*" pour avoir les données de chaque joueur
%3. Split les données de chaque joueur à chaque "." pour avoir les différentes info
%4. Split les données de chaque info à chaque ";" pour avoir les différentes valeurs
%5. Transformer les valeurs en int ou atom
%6. Renvoyer les données en liste de liste

spliter(Text, List) :- %predicat pour transformer les données en liste de liste
    split_string(Text, "*", "", PlayerText), %on split les données à chaque "*" pour avoir les données de chaque joueur
    maplist(sub_split, PlayerText, List), %on s'occupe pour chaque joueur de faire des sous-splits
    write("List: "), writeln(List).

sub_split(PlayerText, SubList) :- %predicat pour faire un sous-split
    split_string(PlayerText, ".", "", ParamText), %on split les données de chaque joueur à chaque "." pour avoir les différentes info
    split_again(ParamText, SubList), %on s'occupe pour chaque paramètre de faire un sous-sous-split
    write("SubList: "), writeln(SubList).

split_again([ID|OtherParam], [IDInt|NewParam]) :- 
    number_string(IDInt, ID), %on transforme l'ID en int car on a pas besoin de le split encore
    subsub_split(OtherParam, NewParam), %on s'occupe pour chaque paramètre de faire un sous-sous-split
    write("NewParam: "), writeln(NewParam).

subsub_split([FirstElement|SecondElement], [FirstList|SecondList]) :- %predicat pour faire un sous-sous-split
    split_int(FirstElement, FirstList), %on split les éléments de la main du joueur à chaque ";" 
    split_atom(SecondElement, SecondList), %on split les éléments de la position des vélos à chaque ";"
    write("FirstList: "), writeln(FirstList),
    write("SecondList: "), writeln(SecondList).

split_int(Input, Output) :- %predicat pour sous-sous-split et transformer les valeurs en int
    split_string(Input, ";", "", ElementString),
    maplist(number_string, Output, ElementString),
    write("Output: "), writeln(Output).

split_atom([Input], [Output]) :- %predicat pour sous-sous-split et transformer les valeurs en atom
    split_string(Input, ";", "", ElementString),
    maplist(string_to_atom, ElementString, Output),
    write("Output: "), writeln(Output).

%---------------------------------------
%Pour renvoyer le move de l'IA en JSON

%1. Récupérer le move de l'IA (get_move_IA)
%2. Mettre les infos de move dans une liste (format_move)
%3. Transformer la liste en string (move_to_char)
%4. Renvoyer le move en JSON (reply_json)

format_move(Move, NewMove) :- %renvoie un move du style (Number, (String1, String2))
    Move =.. [_|NewMove],
    write("NewMove: "), writeln(NewMove).

move_to_char(Move, String) :- %predicat pour transformer le move en String
    Move = [Number, (String1, String2)],
    number_chars(Number, NumList), atom_chars(NumChar, NumList),
    List = [NumChar, String1, String2],
    atomic_list_concat(List, ', ', String),
    write("String: "), writeln(String).
