%je suis pas sur des modifs donc j'ai fait un nouveau fichier, je pense que pr les websocket, 
%il faudrait aussi une partie qui les gère du coté client comme dans l'exemple que j'ai donné

:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/websocket)). %module pr les websockets

:- use_module(bot_TDF).
:- use_module(ia).

% je suppose que le echo servira pour la réponse...? Bof, c'est pr recevoir l'input et renvoyer la réponse normalement mais c'est avec le protocole websocket


:- initialization http_server([port(8080)]).

% get

% différentes root 
:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]).

:- http_handler(root(home), home_page, []).

:- http_handler(root(bot/Question), http_upgrade_to_websocket(answer_bot, []), [spawn([])]). %upgrade_to_websocket pr passer du protocole http au protocole websocket

:- http_handler(root(ia/Board), http_upgrade_to_websocket(answer_ia, []), [spawn([])]). %idem mais pour la route de l'ia
%pour les deux routes, il faut que le client envoie un message en json via uen requête http


home_page(_Request) :-
    reply_html_page(
    title('Demo server'),
    [ h1('test')]).


answer(Que, _Request):- %je l'utilise pas du coup 
    produire_reponse(Que,Resp),
    reply_html_page(title('test'),[h1(Resp)]).

answer_bot(WebSocket) :- %recupère le message recu, va chercher la réponse attendu et la revoie à la root du bot
    ws_receive(WebSocket, Message, [format(json)]), 
    ( Message.opcode == close 
    -> true
    ; get_response_bot(Message.data, Response), 
      write("Response: "), writeln(Response), 
      ws_send(WebSocket, json(Response)), 
      answer_bot(WebSocket) 
    ).
      
get_response_bot(Question, Message) :-
        produire_reponse(Question, Response), %ici il faut recupérer la fct du bot du coup, chez moi ca ne va pas donc faut que je trouve comment faire un import module
        Message = _{response: Response}. 


answer_ia(WebSocket) :- %le code est similaire à celui pour le bot
    ws_receive(WebSocket, Message, [format(json)]), 
    ( Message.opcode == close 
    -> true
    ; get_response_ia(Message.data, Response), 
      write("Response: "), writeln(Response), 
      ws_send(WebSocket, json(Response)),
      answer_ia(WebSocket) 
    ).
          
    get_response_ia(Board, Message) :-
            get_move_IA(Board, Move),
            Message = _{response: Move}. 

%  ça pas encore fini, il faudra que nous on gère les requêtes post (du coup ça je sais pas trop ce que je dois faire avec)
:- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).

handle_post(post, Request):-
    http_read_data(Request, Data, []),
    format('Content-type: text/plain', []),
    reply_html_page(title('post'), [h1(Data)]).
    

