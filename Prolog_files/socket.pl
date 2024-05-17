:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- use_module(library(http/http_cors)). %module pour éviter l'erreur "Reason: CORS header ‘Access-Control-Allow-Origin’ missing" 

:- use_module(bot_TDF). %module pour le bot
%:- use_module(ia). %module pour l'ia

:- initialization set_setting(http:cors, [*]). %On donne l'accès à tout le monde pour envoyer des requetes ici

:- initialization http_server([port(8080)]). 

%différentes root 
:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]). %redirige vers la page d'accueil

:- http_handler(root(home), home_page, []). %page d'accueil (uniquement pour tester)

:- http_handler(root(bot/Question), answer(Question),[]). %root pour le bot

:- http_handler(root(ia), extract_json, []). %root pour l'ia

home_page(_Request) :- %page d'accueil (uniquement pour tester)
    reply_html_page(
    title('Demo server'),
    [ h1('test')]).


answer(Question, _Request):- %predicat pour le bot
    produire_reponse(Question, Resp), %on récupère la réponse du bot
    cors_enable, 
    reply_json(json([answer=Resp])). %on renvoie la réponse du bot

openfile_json(File) :- %predicat pour ouvrir un fichier json (uniquement pour les tests)
    open(File, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    answer_ia(Dict, _).

extract_json(Request) :- %predicat pour extraire le json de la requête
    http_read_json_dict(Request, Dict), %extraire le json de la requête et le trasnforme en dictionnaire
    answer_ia(Dict, _Request). %on appelle le prédicat pour l'ia
        
answer_ia(Board, _Request) :- %predicat pour l'ia
    write("Board: "), writeln(Board), %pour tester
    extract_board(Board, BoardData), %on tranforme le format des données pour que l'ia puisse les utiliser
    get_move_IA(BoardData, Move), %A changer pour le predicat qui renvoie la réponse de l'ia
    cors_enable,
    reply_json(json([response=Move])). %on renvoie la réponse de l'ia

get_move_IA(Board, Move) :- %uniquement pour tester en attendant l'ia, à supprimer après
    write("Board: "), writeln(Board), 
    %MoveResponse = "ROARRRRRRRRRRRRRRRRR", %pour tester une valeur de retour pour move
    Move = Board.

extract_board(Board, BoardData) :- %Board est un dict contenant le contenu du JSON et on veut que BoardData soit une liste de liste contenant uniquement les infos nécéssaires pour l'IA
    Players = Board.players, %On récupère les infos des joueurs
    %CurrentPlayer = Board.currentPlayer, %je sais pas si cet info est utile pour l'IA
    maplist(extract_player, Players, BoardData). %on applique le prédicat extract_player à chaque joueur pour récupérer les infos nécéssaires pour l'IA

extract_player(Player, PlayerData) :- %Player est un dict contenant les infos d'un joueur 
    PlayerID = Player.playerID, %On récupère l'ID du joueur
    Hand = Player.hand, %On récupère la main du joueur
    Bikes = Player.bikes, %On récupère les infos des vélos du joueur
    maplist(bike_position, Bikes, BikePositions), %On applique le prédicat bike_position à chaque vélo pour récupérer leur position
    PlayerData = [PlayerID, Hand, BikePositions]. %On crée une liste contenant l'ID du joueur, sa main et les positions de ses vélos
    
bike_position(Bike, Position) :- %Bike est un dict contenant les infos d'un vélo
    Position = Bike.position. %On récupère la position du vélo
