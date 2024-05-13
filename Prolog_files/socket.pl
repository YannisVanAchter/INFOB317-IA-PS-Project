:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

:- use_module(library(http/http_cors)). %Test pour régler l'erreur "Reason: CORS header ‘Access-Control-Allow-Origin’ missing" 

:- use_module(bot_TDF).
:- use_module(ia).


% je suppose que le echo servira pour la réponse...? Non ça aurait servi si on utilisait le protocole websocket mais c'est pas le cas ici

:- initialization set_setting(http:cors, [*]). %On peut changer ici à qui on veut donner l'autorisation d'accès. '*' c'est pour accepter tt

:- initialization http_server([port(8080)]).

% get

% différentes root 
:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]).

:- http_handler(root(home), home_page, []).

:- http_handler(root(bot/Question), answer(Question),[]).

:- http_handler(root(ia/Board), answer_ia(Board)).

home_page(_Request) :-
    reply_html_page(
    title('Demo server'),
    [ h1('test')]).


answer(Question, _Request):-
    produire_reponse(Question, Resp),
    cors_enable, %pour régler le soucis de CORS
    reply_json(json([answer=Resp])).
        
answer_ia(Board, Request) :-
    %http_read_json_dict(Request, Board),
    write("Board: "), writeln(Board),
    
    get_move_IA(Board, Move), %en imaginant qu'il s'agit du prédicat à utiliser pr l'ia, on peut changer 
    cors_enable, %pour régler le soucis de CORS
    reply_json(json([response=Move])).

get_move_IA(Board, Move). %en attendant que l'ia soit fonctionnel et pour éviter les problèmes dans ce code, à supprimer par la suite

% post

% ça pas encore fini, il faudra que nous on gère les requêtes post
% je sais pas si ce handler est vrmt necéssaire du coup
%:- http_handler(root(post_request), handle_post, [method(post)]).
%:- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).

%handle_post(Request, Response, ID) :-
%    http_read_data(Request, Data, []),
%    (   ID == "bot",
%        Reply = json([response=Response]),
%        http_post_data('http://localhost:8080/bot/', Reply, Received, []) %faut changer l'endroit de retour surement mais j'avoue ne pas trop savoir ou
%   ;   ID == "ia",
%        Reply = json([response=Response]),
%        http_post_data('http://localhost:8080/ia/', Reply, Received, []) %idem
%    ),
%    format('Content-type: text/plain~n~n', []),
%    format('Response: ~w~n', [Received]).


%handle_post(post, Request):-
%    http_read_data(Request, Data, []),
%    format('Content-type: text/plain', []),
%    reply_html_page(title('post'), [h1(Data)]).
    


% :- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).
    
% handle_post(post, Request):-
%     http_read_data(Request, Data, []),
%     format(’Content-type: text/plain’, []),
%     reply_html_page(title(’post’), [h1(Data)]).


