:- use_module(library(http/http_server)).
:- use_module(library(http/http_client)).
:- use_module(answer_question).


:- initialization http_server([port(8080)]).

:- http_handler(root(.),http_redirect(moved, location_by_id(home_page)),[]).

:- http_handler(root(home), home_page, []).

:- http_handler(root(bot/Question), answer(Question),[]).

home_page(_Request) :-
    reply_html_page(
    title('Demo server'),
    [ h1('Hello world!')]).



answer(Que, _Request):-
    Resp = 'test pour le bot',
    reply_html_page(title('test'),[h1(Resp)]).

% :- http_handler(root(post_request), handle_post(Method), [method(Method), methods([post])]).
    
% handle_post(post, Request):-
%     http_read_data(Request, Data, []),
%     format(’Content-type: text/plain’, []),
%     reply_html_page(title(’post’), [h1(Data)]).
    


