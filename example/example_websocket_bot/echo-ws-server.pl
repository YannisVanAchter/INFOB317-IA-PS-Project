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

%Necessary modules from the SWI-Prolog library to handle HTTP servers, HTTP requests, static files, and WebSockets
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).

%Directs all HTTP requests to the local directory
%so if accessing http://localhost:3000, the server will serve files from the local directory

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

%Directs HTTP requests to /echo (WebSocket handler named echo)
%allowing the server to handle WebSocket connections

% * root(echo) indicates we're matching the echo path on the URL e.g.
%   localhost:3000/echo of the server
% * We create a closure using =http_upgrade_to_websocket=
% * The option =spawn= is used to spawn a thread to handle each new
%   request (not strictly necessary, but otherwise we can only handle one
%   client at a time since echo will block the thread)
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

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response(_, Response) :-
  get_time(Time),
  random_response(Message), %The received message doesn't influence the message sent here
  Response = _{message: Message, time: Time}. %he time isn't necessary, it was set by default. We can safely remove it
