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
:- http_handler(root(home), home_page, []). %page d'accueil (uniquement pour tester)
:- http_handler(root(bot/Question), answer(Question),[]). %root pour le bot
:- http_handler(root(ia), extract_json, []). %root pour l'ia

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
        p('To use the IA, send a get request to /ia with the JSON data of the game board.')
    ]).

%---------------------------------------
%bot
answer(Question, _Request):- %predicat pour le bot
    produire_reponse(Question, Resp), %on récupère la réponse du bot
    cors_enable, 
    reply_json(json([answer=Resp])). %on renvoie la réponse du bot

%---------------------------------------
%ia (extraire données)
openfile_json(File) :- %predicat pour ouvrir un fichier json (uniquement pour les tests)
    open(File, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    answer_ia(Dict, 2).

extract_json(Request) :- %predicat pour extraire le json de la requête
    http_read_json_dict(Request, Dict), 
    answer_ia(Dict, 1). %on appelle le prédicat pour l'ia

%ia
answer_ia(Board, _Request) :- %predicat pour l'ia
    extract_board(Board, BoardData), %on tranforme le format des données pour que l'ia puisse les utiliser
    write("BoardData: "), writeln(BoardData),
    get_move_IA(BoardData, Move), %On recupère le move de l'ia
    cors_enable,
    write("Move: "), writeln(Move),
    format_move(Move, NewMove), %on met les infos du move dans une liste
    move_to_char(NewMove, Response), %on tranforme la liste en string
    write("Response: "), writeln(Response),
    reply_json(json([response=Response])). %on renvoie le json

%---------------------------------------
%changer le format des données du JSON pour que l'IA puisse les utiliser (extract_board)

%1. extraire les données du JSON (openfile_json ou extract_json)
%2. réorganiser les joueurs pour que le joueur actuel soit le premier de la liste (reorder_players et current_player)
%3. extraire les infos nécéssaires du JSON pour l'IA (extract_player et bike_position)
%4. changer les ID des joueurs pour que l'ID du joueur actuel soit 0 (change_ID)


extract_board(Board, FinalBoard) :- %Board est un dict contenant le contenu du JSON et on veut que BoardData soit une liste de liste contenant uniquement les infos nécéssaires pour l'IA
    Players = Board.players, 
    CurrentPlayerID = Board.currentPlayer.playerID, 
    reorder_players(Players, CurrentPlayerID, ReorderedPlayers), %On réorganise les joueurs pour que le joueur actuel soit le premier de la liste
    maplist(extract_player, ReorderedPlayers, NewBoard), %on applique le prédicat extract_player à chaque joueur pour récupérer les infos nécéssaires pour l'IA
    change_ID(NewBoard, 0, [], BoardData), %on change les ID des joueurs pour que l'ID du joueur actuel soit 0
    reverse(BoardData, FinalBoard). 

reorder_players(Players, CurrentPlayerID, ReorderedPlayers) :- %prédicat pour réorganiser les joueurs
    partition(current_player(CurrentPlayerID), Players, CurrentAndGreater, LessThanCurrent), %On sépare les joueurs avec des ids > ou = à celui du joueur actuel des autres
    append(CurrentAndGreater, LessThanCurrent, ReorderedPlayers). %On met le joueur actuel en premier dans la liste 

current_player(CurrentPlayerID, Player) :- %prédicat pour vérifier si un joueur a un id supérieur ou égal à celui du joueur actuel
    Player.playerID >= CurrentPlayerID.

extract_player(Player, PlayerData) :- %Player est un dict contenant les infos d'un joueur 
    PlayerID = Player.playerID, 
    Hand = Player.hand, 
    Bikes = Player.bikes, 
    maplist(bike_position, Bikes, BikePositions), %On applique le prédicat bike_position à chaque vélo pour récupérer leur position
    maplist(string_to_atom, BikePositions, BikePositionsChar), %pour le fichier .json
    PlayerData = [PlayerID, Hand, BikePositionsChar]. %On crée une liste contenant l'ID du joueur, sa main et les positions de ses vélos

    bike_position(Bike, Position) :- %Bike est un dict contenant les infos d'un vélo
    Position = Bike.position. 

change_ID([], _, BoardData, BoardData). %prédicat pour changer les ID des joueurs
change_ID([[_, Hand, Position]|OtherPlayer], I, Acc, BoardData) :- 
    NewID = I,
    I1 is I + 1,
    change_ID(OtherPlayer, I1, [[NewID, Hand, Position]|Acc], BoardData).

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

%---------------------------------------
%A supprimer après

get_move_IA_test(Board, Move) :- %uniquement pour tester en attendant l'ia, à supprimer après
    write("Board: "), writeln(Board), 
    %MoveResponse = "ROARRRRRRRRRRRRRRRRR", %pour tester une valeur de retour pour move
    Move = Board.

