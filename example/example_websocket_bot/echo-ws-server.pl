% INSTRUCTIONS
% =swipl echo-ws-server.pl=
% =:- start_server.=
%
% Then navigate to http://localhost:3000 in your browser

%Declares a module echo_server + exports the predicates start_server/0 and stop_server/0
:- module(echo_server,
  [ start_server/0,
    stop_server/0
  ]
).

%les modules nécéssaires pr le code
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).


%Directs all HTTP requests to the local directory
%so if accessing http://localhost:3000, the server will serve files from the local directory

% =http_handler(+Path, :Closure, +Options)=
%
% * root(.) :  il est activé lorsque l'URL demandée est juste le nom de domaine,
% *  http_reply_from_files : les fichiers seront servis à partir du répertoire local où se trouve le fichier Prolog en cours d'exécution
:- http_handler(root(.),
                http_reply_from_files('.', []),
                [prefix]).

%Directs HTTP requests to /echo (WebSocket handler named echo)
%allowing the server to handle WebSocket connections

% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:3000/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)

% pour le moment, le code est relié du coup au fichier js echo qui est dans le dossier
:- http_handler(root(echo),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

%Starts the server. Either specify the port or use port 3000 by default
start_server :-
    default_port(Port),
    start_server(Port).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]). %The http_dispatch specifies the entry point where we have the dispatching of HTTP requests, essentially it's the routing

%Stops the server. Similar approach for stopping the server
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
  ws_receive(WebSocket, Message, [format(json)]), %Here we wait to receive a message via WebSocket, if we receive one, we put it into Message, specifying the format as JSON
  ( Message.opcode == close %Checking if it's a close message, in which case we return true
  -> true
  ; get_response(Message.data, Response), %We get the response to send in get_response
    write("Response: "), writeln(Response), %Here was to see errors in the terminal
    ws_send(WebSocket, json(Response)), %We send the response
    echo(WebSocket) %We maintain the connection with recursion
  ).



random_response(Response) :-
  Responses = ["Vive Krokmou", "Krokmou DOAT", "Krokmou El Mastro", "Connaissez Vous Krokmou?", "Krokmou Je T Aime", "Je Suis Un Dragon"],
  random_member(Response, Responses).


%Le get doit être modifié pour soit utiliser le predicat du bot à question soit le predicat de l'IA
get_response(Input, Response) :-
  get_time(Time),
  random_response(Message), %The received message doesn't influence the message sent here
  Response = _{message: Message, time: Time}. %he time isn't necessary, it was set by default. We can safely remove it
