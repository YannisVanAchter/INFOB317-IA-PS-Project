:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- use_module(library(http/http_cors)). %module pour éviter l'erreur "Reason: CORS header ‘Access-Control-Allow-Origin’ missing" 

:- use_module(bot_TDF). %module pour le bot
:- use_module(ia). %module pour l'ia

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

answer(Question, _Request):- %predicat pour le bot
    produire_reponse(Question, Resp), %on récupère la réponse du bot
    cors_enable, 
    reply_json(json([answer=Resp])). %on renvoie la réponse du bot

openfile_json(File) :- %predicat pour ouvrir un fichier json (uniquement pour les tests)
    open(File, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    answer_ia(Dict, 2).

extract_json(Request) :- %predicat pour extraire le json de la requête
    http_read_json_dict(Request, Dict), 
    answer_ia(Dict, 1). %on appelle le prédicat pour l'ia
    
answer_ia(Board, _Request) :- %predicat pour l'ia
    extract_board(Board, BoardData), %on tranforme le format des données pour que l'ia puisse les utiliser
    write("BoardData: "), writeln(BoardData),
    get_move_IA(BoardData, Move), %A changer pour le predicat qui renvoie la réponse de l'ia
    cors_enable,
    write("Move: "), writeln(Move),
    %reply_json(json([response=Move])). %on renvoie la réponse de l'ia
    format_move(Move, NewMove),
    move_to_char(NewMove, Response), %on crée le json à partir de la réponse de l'ia
    write("Response: "), writeln(Response), %pour tester
    reply_json(json([response=Response])). %on renvoie le json

format_move(Move, NewMove) :- %predicat pour tester
    Move =.. [_|NewMove],
    write("NewMove: "), writeln(NewMove).

move_to_char(Move, String) :- %predicat pour transformer le move en String
    Move = [Number, (String1, String2)],
    number_chars(Number, NumList), atom_chars(NumChar, NumList),
    List = [NumChar, String1, String2],
    atomic_list_concat(List, ', ', String),
    write("String: "), writeln(String).

get_move_IA2(Board, Move) :- %uniquement pour tester en attendant l'ia, à supprimer après
    write("Board: "), writeln(Board), 
    %MoveResponse = "ROARRRRRRRRRRRRRRRRR", %pour tester une valeur de retour pour move
    Move = Board.

extract_board(Board, FinalBoard) :- %Board est un dict contenant le contenu du JSON et on veut que BoardData soit une liste de liste contenant uniquement les infos nécéssaires pour l'IA
    Players = Board.players, %On récupère les infos des joueurs
    CurrentPlayerID = Board.currentPlayer.playerID, %On récupère l'ID du joueur actuel
    reorder_players(Players, CurrentPlayerID, ReorderedPlayers), %On réorganise les joueurs pour que le joueur actuel soit le premier
    maplist(extract_player, ReorderedPlayers, NewBoard), %on applique le prédicat extract_player à chaque joueur pour récupérer les infos nécéssaires pour l'IA
    change_ID(NewBoard, 0, [], BoardData),
    reverse(BoardData, FinalBoard).

change_ID([], _, BoardData, BoardData).
change_ID([[_, Hand, Position]|OtherPlayer], I, Acc, BoardData) :- 
    NewID = I,
    I1 is I + 1,
    change_ID(OtherPlayer, I1, [[NewID, Hand, Position]|Acc], BoardData).

reorder_players(Players, CurrentPlayerID, ReorderedPlayers) :- %prédicat pour réorganiser les joueurs
    partition(current_player(CurrentPlayerID), Players, CurrentAndGreater, LessThanCurrent), %On sépare les joueurs avec des ids > ou = à celui du joueur actuel des autres
    append(CurrentAndGreater, LessThanCurrent, ReorderedPlayers). %On met le joueur actuel en premier dans la liste 

current_player(CurrentPlayerID, Player) :- %prédicat pour vérifier si un joueur a un id supérieur ou égal à celui du joueur actuel
    Player.playerID >= CurrentPlayerID.

extract_player(Player, PlayerData) :- %Player est un dict contenant les infos d'un joueur 
    PlayerID = Player.playerID, %On récupère l'ID du joueur
    Hand = Player.hand, %On récupère la main du joueur
    Bikes = Player.bikes, %On récupère les infos des vélos du joueur
    maplist(bike_position, Bikes, BikePositions), %On applique le prédicat bike_position à chaque vélo pour récupérer leur position
    maplist(string_to_atom, BikePositions, BikePositionsChar), %pour le fichier
    PlayerData = [PlayerID, Hand, BikePositionsChar]. %On crée une liste contenant l'ID du joueur, sa main et les positions de ses vélos

    bike_position(Bike, Position) :- %Bike est un dict contenant les infos d'un vélo
    Position = Bike.position. %On récupère la position du vélo