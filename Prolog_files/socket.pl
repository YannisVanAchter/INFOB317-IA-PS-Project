:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

:- use_module(bot_TDF).
:- use_module(ia).

% je suppose que le echo servira pour la réponse...? Non ça aurait servi si on utilisait le protocole websocket mais c'est pas le cas ici


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

answer(Que, _Request):-
    produire_reponse(Que,Resp),
    reply_html_page(title('test'),[h1(Resp)]).

answer_ia(Request) :-
    http_read_json_dict(Request, Board),
    get_move_IA(Board, Move), %en imaginant qu'il s'agit du prédicat à utiliser pr l'ia, on peut changer 
    reply_json(json([response=Move])).

%  ça pas encore fini, il faudra que nous on gère les requêtes post
:- http_handler(root(post_request), handle_post, [method(post)]).
%:- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).

handle_post(Request) :-
    http_read_data(Request, Data, []),
    format('Content-type: text/plain', []),
    reply_html_page(title('post'), [p('data'), pre(Data)]).
%handle_post(post, Request):-
%    http_read_data(Request, Data, []),
%    format('Content-type: text/plain', []),
%    reply_html_page(title('post'), [h1(Data)]).
    


% :- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).
    
% handle_post(post, Request):-
%     http_read_data(Request, Data, []),
%     format(’Content-type: text/plain’, []),
%     reply_html_page(title(’post’), [h1(Data)]).
    


