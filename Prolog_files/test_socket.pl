:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- use_module(library(http/http_cors)). %Test pour régler l'erreur "Reason: CORS header ‘Access-Control-Allow-Origin’ missing" 

:- use_module(bot_TDF).
%:- use_module(ia).

:- initialization set_setting(http:cors, [*]). %On peut changer ici à qui on veut donner l'autorisation d'accès. '*' c'est pour accepter tt

:- initialization http_server([port(8080)]).

% différentes root 
:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]).

:- http_handler(root(home), home_page, []).

:- http_handler(root(bot/Question), answer(Question),[]).

:- http_handler(root(ia/Board), openfile_answer_ia(Board), []).

home_page(_Request) :-
    reply_html_page(
    title('Demo server'),
    [ h1('test')]).


answer(Question, _Request):-
    produire_reponse(Question, Resp),
    cors_enable, %pour régler le soucis de CORS
    reply_json(json([answer=Resp])).

openfile_json(File) :-
    open(File, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),
    answer_ia(Dict).
        
answer_ia(Dict) :-
    write("Dict: "), writeln(Dict),
    extract_board(Dict, BoardData),
    get_move_IA(BoardData, Move), %A changer en fonction de l'IA
    cors_enable, %pour régler le soucis de CORS
    reply_json(json([response=Move])).

get_move_IA(Board, Move) :-
    write("Board: "), writeln(Board), %en attendant que l'ia soit fonctionnel et pour éviter les problèmes dans ce code, à supprimer par la suite
    MoveResponse = "ROARRRRRRRRRRRRRRRRR", 
    Move = MoveResponse.

extract_board(Board, BoardData) :- 
    %Board est le contenu du JSON et on veut que BoardData soit une liste de liste contenant uniquement les infos nécéssaires pour l'IA
    Players = Board.players,
    %CurrentPlayer = Board.currentPlayer, %je sais pas si cet info est utile pour l'IA
    maplist(extract_player, Players, BoardData).

extract_player(Player, PlayerData) :-
    PlayerID = Player.playerID,
    Hand = Player.hand,
    Bikes = Player.bikes,
    maplist(bike_position, Bikes, BikePositions),
    PlayerData = [PlayerID, Hand, BikePositions].
    
bike_position(Bike, Position) :-
    Position = Bike.position.