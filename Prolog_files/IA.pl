:- module(ia, [get_move/2]).


% faudra surement un param avec quel est le joueur pour lequel on cherche un move
get_move(Board,Move):-
    produce_tree(Tree),
    visit_tree(Tree,Move).

produce_tree(Tree).


% récursive, voir où est cas de base
visit_tree(Tree,Move).