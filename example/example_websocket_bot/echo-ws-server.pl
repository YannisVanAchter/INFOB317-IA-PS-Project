% INSTRUCTIONS
% =swipl echo-ws-server.pl=
% =:- start_server.=
%
% Then navigate to http://localhost:3000 in your browser


:- module(echo_server,
  [ start_server/0,
    stop_server/0
  ]
).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).

% http_handler docs: http://www.swi-prolog.org/pldoc/man?predicate=http_handler/3
% =http_handler(+Path, :Closure, +Options)=
%
% * root(.) indicates we're matching the root URL
% * We create a closure using =http_reply_from_files= to serve up files
%   in the local directory
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(.),
                http_reply_from_files('.', []),
                [prefix]).
% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:3000/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
:- http_handler(root(echo),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

start_server :-
    default_port(Port),
    start_server(Port).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_server() :-
    default_port(Port),
    stop_server(Port).
stop_server(Port) :-
    http_stop_server(Port, []).

default_port(3000).

%! echo(+WebSocket) is nondet.
% This predicate is used to read in a message via websockets and echo it
% back to the client
echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      echo(WebSocket)
    ).

random_response(Response) :-
  Responses = ["Vive Krokmou", "Krokmou DOAT", "Krokmou El Mastro", "Connaissez Vous Krokmou?", "Krokmou Je T Aime", "Je Suis Un Dragon"],
  random_member(Response, Responses).

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response(_, Response) :-
  get_time(Time),
  random_response(Message),
  Response = _{message: Message, time: Time}.
