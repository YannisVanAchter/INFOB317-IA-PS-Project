:- module(test_bot, [random_response/0]).

random_response(Response) :-
    Responses = ["Vive Krokmou", "Krokmou DOAT", "Krokmou El Mastro", "Connaissez Vous Krokmou?", "Krokmou Je T Aime", "Je Suis Un Dragon"],
    random_member(Response, Responses).