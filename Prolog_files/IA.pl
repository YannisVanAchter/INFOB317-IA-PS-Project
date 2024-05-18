
% on a besoin de pour chaque joueur
% liste avec en premier elem sa main, puis ses coureurs représentés par un tuple de coord [[num_player, [main], [(x1,y1), (x2,y2)]], num_player]
%  ex: [[1,[1,1,1,1,1],['1','2','4']],[4,[4,4,4,4,4],['3','4','6']],[2,[2,2,2,2,2],['2','3','10a']],[3,[3,3,3,3,3],['4','5','22']]]
% minmax à 2, l'heuristique ça serait d'être le premier
get_move_IA(Infos,Move):-
    nth0(0,Infos,Data),
    get_infos_player(Infos,1,[Cards1,Bikes1]),
    % get_infos_player(Infos,2,[_,Bikes2]),
    % get_infos_player(Infos,3,[_,Bikes3]),
    % get_infos_player(Infos,4,[_,Bikes4]),
    length(Cards1,Depth),
    get_all_bikes(Bikes1,Bikes2,Bikes3,Bikes4,All_bikes),
    minimax(Bikes1,All_bikes,[Cards1,Bikes1],Depth,Move,_).

    % minimax([[4,2],['4-A-left','4-A-left']],1,A,B). 
% there is not base case since every player should be present one time only, and each shoul be present
get_infos_player([H_infos|_],Num_player,Infos):-
    writeln(H_infos),
    nth0(0,H_infos,Num),
    nth0(1,H_infos,Hand),
    nth0(2,H_infos,Bikes),
    Num = Num_player,
    Infos=[Hand,Bikes],!.

get_infos_player([_|T_infos],Num_player,Infos):-
    get_infos_player(T_infos,Num_player,Infos).

get_all_bikes(Bikes1,Bikes2,Bikes3,Bikes4,All):-
    append(Bikes1,Bikes2,Acc),
    append(Acc,Bikes3,Acc2),
    append(Acc3,Bikes4,All).

get_count_elem(_,[],Acc,Acc).
get_count_elem(Target,[Elem|Other_elems],Acc,Count):-
    Target=Elem,
    New_acc is Acc+1,
    get_count_elem(Target,Other_elems,New_acc,Count),!.
get_count_elem(Target,[Elem|Other_elems],Acc,Count):-
    get_count_elem(Target,Other_elems,Acc,Count),!.
    

minimax(Bikes_player,All_bikes,[Cards,Bike],Depth,Best_move,Min_score):-
    possible_moves(Bike,Cards,[],Moves),
    writeln(Moves),
    evaluate_moves(Bikes_player,All_bikes,Moves,[Cards,Bike],Depth,Min_score,Best_move),!.

% possible_moves('2-A-left', [2], [], _44446)
% evaluate_moves([(1, '2-A-left', '1-A-left'), (2, '3-A-left', '1-A-left'), (1, '3-A-left', '2-A-left'), (2, '4-A-left', '2-A-left')], 2, _20634) 
evaluate_moves(_,_,[],_,_,0,_).
evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    writeln(New_state),
    terminal_state(New_state),
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    terminal_state(New_state),
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.
evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,0,Best_score,Best_move):-
    make_move(State,Move,New_state),
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,0,Best_score,Best_move):-
    make_move(State,Move,New_state),
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.


evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    nth0(0,New_state,New_cards),
    length(New_cards,Lenght),
    Lenght=0,
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    nth0(0,New_state,New_cards),
    length(New_cards,Lenght),
    Lenght=0,
    score(State,New_state,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    not(valid_move(Move,All_bikes)),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    New_depth is Depth-1,
    minimax(Bikes_player,All_bikes,New_state,New_depth,_,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    New_depth is Depth-1,
    minimax(Bikes_player,All_bikes,New_state,New_depth,_,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.

make_move([Cards,_],(Card,New_pos,Old_pos),[New_cards,[New_pos]]):-
    select(Card,Cards,New_cards).

valid_move((_,New_pos,_),All_bikes):-
    get_count_elem(New_pos,All_bikes,0,Places_used),
    available_places(New_pos,Actual_places),
    Actual_places>Places_used.

% make_move([[1, 2], ['1-A-left', '2-A-left']], (1, '2-A-left', '1-A-left'), _7356)
% (1, '2-A-left', '1-A-left'), (2, '3-A-left', '1-A-left'), (1, '3-A-left', '2-A-left')
score([_,[Bike1|_]],[_,[Bike2|_]],Bikes,Score):-
    split_string(Bike1,"-","",List_position1),
    split_string(Bike2,"-","",List_position2),
    length(List_position1,Length1),
    length(List_position2,Lenght2),
    get_value_bike(Bike1,Value1),
    get_value_bike(Bike2,Value2),
    get_group_score(Bike1,Bike2,Bikes,Score_group),
    compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score).

get_group_score(Old_bike,New_bike,[Bike1,Bike2,Bike3],Score):-
    Old_bike=Bike1,
    get_value_bike(New_bike,Value1),
    get_value_bike(Bike2,Value2),
    get_value_bike(Bike3,Value3),
    Score is 4+(Value1-Value2-Value3).
get_group_score(Old_bike,New_bike,[Bike1,Bike2,Bike3],Score):-
    Old_bike=Bike2,
    get_value_bike(New_bike,Value1),
    get_value_bike(Bike1,Value2),
    get_value_bike(Bike3,Value3),
    Score is 4+(Value1-Value2-Value3).
get_group_score(Old_bike,New_bike,[Bike1,Bike2,Bike3],Score):-
    Old_bike=Bike3,
    get_value_bike(New_bike,Value1),
    get_value_bike(Bike2,Value2),
    get_value_bike(Bike3,Value1),
    Score is 4+(Value1-Value2-Value3).
    
get_value_bike(Bike,Value):-
    split_string(Bike,"-","",List_bike),
    length(List_bike,Length),
    Length=4,
    nth0(1,List_bike,Value_string),
    number_string(Value,String).
get_value_bike(Bike,Value):-
    split_string(Bike,"-","",List_bike),
    length(List_bike,Length),
    Length=3,
    nth0(0,List_bike,Value_string),
    number_string(Value,Value_string).

compute_score(_,_,Length1,Lenght2,Score_group,Score):-
    Score is 0,
    Length1>Lenght2,!.

compute_score(_,Value2,Length1,Lenght2,Score_group,Score):-
    Length1<Lenght2,
    Score is Value2+30+Score_group,!.

compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score):-
    Length1=Lenght2,
    Score is Value2-Value1+Score_group,!.




possible_moves([],_,Acc,Acc).
possible_moves([Bike|Other_bike],Cards,Acc,Moves):-
    iterate_card(Cards,Bike,[],Moves_bike),
    append(Acc,Moves_bike,New_acc),
    possible_moves(Other_bike, Cards, New_acc, Moves).

iterate_card([],_,Acc,Acc).
iterate_card([Card|Other_cards],Bike,Acc,Moves):-
    get_next_position(Bike,Card,New_position),
    append(Acc,[(Card,New_position,Bike)],New_acc),
    iterate_card(Other_cards,Bike,New_acc,Moves),!.




    
%     % minimax([[[1,2],'1-A-left'],[[1,1],'1-A-left']],2,1,0,A).
% minimax(State,_,Player,Best_score,_):-
%     terminal_state(State),
%     score(State,Player,Score),
%     Best_score is Score,
%     writeln(Best_score).

% minimax(State,0,Player,Best_score,_):-
%     score(State,Player,Score),
%     Best_score is Score,
%     writeln(Best_score).

% minimax([[[],Bike1],Player2],_,Player,Best_score,_):-
%     score([[[],Bike1],Player2],Player,Score),
%     Best_score is Score,
%     writeln(Best_score).
% minimax([Player1,[[],Bike2]],_,Player,Best_score,_):-
%     score([Player1,[[],Bike2]],Player,Score),
%     Best_score is Score,
%     writeln(Best_score).
% minimax([[[],Bike1],[[],Bike2]],_,Player,Best_score,_):-
%     score([[[],Bike1],[[],Bike2]],Player,Score),
%     Best_score is Score,
%     writeln(Best_score).

% minimax([[Cards,Bike],Player2], Depth, 1, Best_score, Best_move):-
%     % writeln([[Cards,Bike],Player2]),
%     % writeln(Best_move),
%     possible_moves(Cards,Bike,[],Moves),
%     evaluate_move(Moves,Depth,1,[[Cards,Bike],Player2],Best_score,Best_move).

% minimax([Player1,[Cards,Bike]], Depth, 2, Best_score, Best_move):-
%     % writeln([Player1,[Cards,Bike]]),
%     % writeln(Best_move),
%     possible_moves(Cards,Bike,[],Moves),
%     evaluate_move(Moves,Depth,2,[Player1,[Cards,Bike]],Best_score,Best_move).

% evaluate_move([Move|Other_moves],Depth,1,State,Best_score,Best_move):-
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,2,Best_branch,Best_move),
%     Best_branch>=Best_score,
%     New_best is Best_branch,
%     Best_move is Move,
%     evaluate_move(Other_moves,Depth,1,State,New_best,Best_move).

% evaluate_move([Move|Other_moves],Depth,1,State,Best_score,Best_move):-
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,2,Best_branch,Best_move),
%     Best_branch<Best_score,
%     New_best is Best_Score,
%     evaluate_move(Other_moves,Depth,1,State,New_best,Best_move).

% evaluate_move([Move|Other_moves],Depth,2,State,Best_score,Best_move):-
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,1,Best_branch,Best_move),
%     Best_branch>=Best_score,
%     New_best is Best_branch,
%     evaluate_move(Other_moves,Depth,2,State,New_best,Best_move).

% evaluate_move([Move|Other_moves],Depth,2,State,Best_score,Best_move):-
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,1,Best_branch,Best_move),
%     Best_branch<Best_score,
%     New_best is Best_score,
%     evaluate_move(Other_moves,Depth,2,State,New_best,Best_move).


% % cas base
% evaluate_move([Move],Depth,1,State,Best_score,Best_move):-
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,2,Best_branch,Best_move),
%     Best_branch>=Best_score,
%     Best_move is Move.

% evaluate_move([Move],Depth,1,State,Best_score,Best_move):-
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,2,Best_branch,Best_move),
%     Best_branch<Best_score,
%     Best_Score is Best_branch.

% evaluate_move([Move],Depth,2,State,Best_score,Best_move):-
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,1,Best_branch,Best_move),
%     Best_branch>=Best_score,
%     Best_score is 0.

% evaluate_move([Move],Depth,2,State,Best_score,Best_move):-
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state,New_depth,1,Best_branch,Best_move),
%     Best_branch<Best_score,
%     Best_score is Best_branch.

% evaluate_moves([Move], State, Depth, 2,Best_score, Best_move):-
%     % writeln(Best_score),
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 1, Temp_score, Move),
%     % writeln(Temp_score),
%     get_min(Best_score,Temp_score,New_best_score),
%     New_best_score>Best_score.

% evaluate_moves([Move], State, Depth, 2,Best_score, Best_move):-
%     % writeln(Best_score),
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 1, Best_score, Best_move).
%     % % writeln(Temp_score),
%     % get_min(Best_score,Temp_score,New_best_score),
%     % New_best_score=<Best_score.

% evaluate_moves([Move], State, Depth,1,Best_score, Best_move):-
%     % writeln(Best_score),
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 2, Best_score, Best_move).
%     % writeln(Temp_score),
%     % get_max(Best_score,Temp_score,New_best_score),
%     % New_best_score=<Best_score.

% % evaluate_moves([Move], State, Depth, 1,Best_score, Move):-
% %     % writeln(Best_score),
% %     make_move(State,1,Move,New_state),
% %     New_depth is Depth-1,
% %     minimax(New_state, New_depth, 2, Temp_score, Best_move),
% %     % writeln(Temp_score),
% %     get_max(Best_score,Temp_score,New_best_score),
% %     New_best_score>Best_score.

% evaluate_moves([Move | Moves], State, Depth, 2,Best_score, Move):-
%     % writeln(Best_score),
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 1, Temp_score, Move),
%     % writeln(Temp_score),
%     get_min(Best_score,Temp_score,New_best_score),
%     New_best_score>Best_score,
%     Best_score is New_best_score,
%     evaluate_moves(Moves,State,Depth,2,Best_score,Move).

% evaluate_moves([Move | Moves], State, Depth, 2,Best_score, Best_move):-
%     % writeln(Best_score),
%     make_move(State,2,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 1, Temp_score, Best_move),
%     % writeln(Temp_score),
%     get_min(Best_score,Temp_score,New_best_score),
%     New_best_score=<Best_score,
%     evaluate_moves(Moves,State,Depth,2,Best_score,Best_move).

% evaluate_moves([Move | Moves], State, Depth,1,Best_score, Best_move):-
%     % writeln(Best_score),
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 2, Temp_score, Best_move),
%     % writeln(Temp_score),
%     get_max(Best_score,Temp_score,New_best_score),
%     New_best_score=<Best_score,
%     evaluate_moves( Moves, State, New_depth, 1, Best_score, Best_move).

% evaluate_moves([Move | Moves], State, Depth, 1,Best_score, Move):-
%     % writeln(Best_score),
%     make_move(State,1,Move,New_state),
%     New_depth is Depth-1,
%     minimax(New_state, New_depth, 2, Temp_score, Move),
%     % writeln(Temp_score),
%     get_max(Best_score,Temp_score,New_best_score),
%     New_best_score>Best_score,
%     Best_score is New_best_score,
%     evaluate_moves(Moves,State,Depth,1,Best_score,Move).



% possible_moves([],_,Acc,Acc).
% possible_moves([Card|Other_cards],Bike,Acc,Moves):-
%     get_next_position(Bike,Card,New_position),
%     append(Acc,[(Card,New_position,Bike)],New_acc),
%     possible_moves(Other_cards,Bike,New_acc,Moves),!.

% make_move([[Cards,_],Player2],1,(Card,New_pos,Old_pos),[[New_cards,New_pos],Player2]):-
%     select(Card,Cards,New_cards).

% make_move([Player1,[Cards,_]],2,(Card,New_pos,Old_pos),[Player1,[New_cards,New_pos]]):-
%     select(Card,Cards,New_cards).

terminal_state([_,[Bike]]):-
    end(Bike).


get_min(Value1,Value2,Value1):-
    Value1=<Value2.
get_min(_,Value2,Value2).

get_max(Value1,Value2,Value1):-
    Value1>=Value2.
get_max(_,Value2,Value2).

% score([[_,Bike1],[_,Bike2]],Player,Score):-
%     split_string(Bike1,"-","",List_position1),
%     split_string(Bike2,"-","",List_position2),
%     length(List_position1,Length1),
%     length(List_position2,Lenght2),
%     compute_score(List_position1,List_position2,Length1,Lenght2,Player,Score).

% compute_score(_,_,Length1,Lenght2,1,Score):-
%     Score is 1,
%     Length1>Lenght2,!.

% compute_score(_,_,Length1,Lenght2,1,Score):-
%     Score is 0,
%     Length1<Lenght2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,1,Score):-
%     Score is 1,
%     Length1=Lenght2,
%     Length1=4,
%     nth0(1,List_position1,Value1),
%     nth0(1,List_position2,Value2),
%     Value1>Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,1,Score):-
%     Score is 0,
%     Length1=Lenght2,
%     Length1=4,
%     nth0(1,List_position1,Value1),
%     nth0(1,List_position2,Value2),
%     Value1=<Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,1,Score):-
%     Score is 0,
%     Length1=Lenght2,
%     Length1=3,
%     nth0(0,List_position1,Value1),
%     nth0(0,List_position2,Value2),
%     Value1=<Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,1,Score):-
%     Score is 1,
%     Length1=Lenght2,
%     Length1=3,
%     nth0(0,List_position1,Value1),
%     nth0(0,List_position2,Value2),
%     Value1>Value2,!.

% compute_score(_,_,Length1,Lenght2,2,Score):-
%     Score is 0,
%     Length1<Lenght2,!.

% compute_score(_,_,Length1,Lenght2,2,Score):-
%     Score is 1,
%     Length1>Lenght2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,2,Score):-
%     Score is 1,
%     Length1=Lenght2,
%     Length1=4,
%     nth0(1,List_position1,Value1),
%     nth0(1,List_position2,Value2),
%     Value1>=Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,2,Score):-
%     Score is 0,
%     Length1=Lenght2,
%     Length1=4,
%     nth0(1,List_position1,Value1),
%     nth0(1,List_position2,Value2),
%     Value1<Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,2,Score):-
%     Score is 1,
%     Length1=Lenght2,
%     Length1=3,
%     nth0(0,List_position1,Value1),
%     nth0(0,List_position2,Value2),
%     Value1>=Value2,!.

% compute_score(List_position1,List_position2,Length1,Lenght2,2,Score):-
%     Score is 0,
%     Length1=Lenght2,
%     Length1=3,
%     nth0(0,List_position1,Value1),
%     nth0(0,List_position2,Value2),
%     Value1<Value2,!.


get_next_position(Position,0,Position).
get_next_position(Position,Card_value,Res):-
    next(Position,List_Next),
    member(Next,List_Next),
    New_value is Card_value-1,
    get_next_position(Next, New_value,Res).

% number of places available on the board
available_places('1-A-left',3).
available_places('2-A-left',3).
available_places('3-A-left',3).
available_places('4-A-left',3).
available_places('5-A-left',3).
available_places('6-A-left',3).
available_places('7-A-left',3).
available_places('8-A-left',3).
available_places('9-A-left',1).
available_places('9-B-left',1).
available_places('9-C-left',1).
available_places('10-A-left',1).
available_places('10-B-left',1).
available_places('10-C-left',1).
available_places('11-A-left',2).
available_places('12-A-left',2).
available_places('13-A-left',2).
available_places('14-A-left',2).
available_places('15-A-left',2).
available_places('16-A-left',2).
available_places('17-A-left',2).
available_places('18-A-left',2).
available_places('19-A-left',3).
available_places('20-A-left',3).
available_places('21-A-left',3).
available_places('22-A-left',3).
available_places('23-A-left',2).
available_places('24-A-left',2).
available_places('25-A-left',2).
available_places('26-A-left',1).
available_places('26-B-left',1).
available_places('27-A-left',1).
available_places('27-A-left',1).
available_places('28-A-left',2).
available_places('29-A-left',2).
available_places('30-A-left',2).
available_places('31-A-left',2).
available_places('32-A-left',2).
available_places('33-A-left',2).
available_places('34-A-left',2).
available_places('35-A-left',2).
available_places('23-A-right',1).
available_places('24-A-right',1).
available_places('25-A-right',1).
available_places('26-D-right',1).
available_places('26-C-right',1).
available_places('27-D-right',1).
available_places('27-C-right',1).
available_places('28-A-right',1).
available_places('29-A-right',1).
available_places('30-A-right',1).
available_places('31-A-right',1).
available_places('32-A-right',1).
available_places('33-A-right',1).
available_places('34-A-right',1).
available_places('35-A-right',1).
available_places('36-A-left',2).
available_places('37-A-left',2).
available_places('38-A-left',2).
available_places('39-A-left',2).
available_places('40-A-left',2).
available_places('41-A-left',2).
available_places('42-A-left',2).
available_places('43-A-left',2).
available_places('44-A-left',2).
available_places('45-A-left',2).
available_places('46-A-left',2).
available_places('47-A-left',2).
available_places('48-A-left',2).
available_places('49-A-left',2).
available_places('50-A-left',2).
available_places('51-A-left',2).
available_places('52-A-left',2).
available_places('53-A-left',2).
available_places('54-A-left',2).
available_places('55-A-left',2).
available_places('56-A-left',2).
available_places('57-A-left',2).
available_places('58-A-left',2).
available_places('59-A-left',2).
available_places('60-A-left',2).
available_places('61-A-left',2).
available_places('62-A-left',2).
available_places('63-A-left',1).
available_places('63-B-left',1).
available_places('63-C-left',1).
available_places('64-A-left',1).
available_places('64-B-left',1).
available_places('64-C-left',1).
available_places('65-A-left',2).
available_places('66-A-left',2).
available_places('67-A-left',2).
available_places('68-A-left',2).
available_places('69-A-left',2).
available_places('70-A-left',2).
available_places('71-A-left',2).
available_places('72-A-left',2).
available_places('73-A-left',1).
available_places('74-A-left',1).
available_places('75-A-left',1).
available_places('76-A-left',2).
available_places('77-A-left',2).
available_places('78-A-left',2).
available_places('79-A-left',2).
available_places('80-A-left',2).
available_places('81-A-left',2).
available_places('82-A-left',2).
available_places('83-A-left',2).
available_places('84-A-left',1).
available_places('85-A-left',1).
available_places('86-A-left',1).
available_places('87-A-left',1).
available_places('88-A-left',1).
available_places('89-A-left',1).
available_places('90-A-left',1).
available_places('91-A-left',1).
available_places('92-A-left',1).
available_places('93-A-left',1).
available_places('94-A-left',1).
available_places('84-A-right',1).
available_places('85-A-right',1).
available_places('86-A-right',1).
available_places('87-A-right',1).
available_places('88-A-right',1).
available_places('89-B-right',1).
available_places('89-C-right',1).
available_places('90-B-right',1).
available_places('90-C-right',1).
available_places('91-A-right',1).
available_places('92-A-right',1).
available_places('93-A-right',1).
available_places('94-A-right',1).
available_places('95-A-left',3).
available_places('0-A-left',3).
available_places('-1-A-left',3).
available_places('-2-A-left',3).
available_places('-3-A-left',3).
available_places('-4-A-left',3).
available_places('-5-A-left',3).
available_places('-6-A-left',3).
available_places('-7-A-left',3).
available_places('-8-A-left',3).
available_places('-9-A-left',3).

% linked cases on the board
next('0-B-left',['1-A-left']).
next('1-A-left',['2-A-left']).
next('2-A-left',['3-A-left']).
next('3-A-left',['4-A-left']).
next('4-A-left',['5-A-left']).
next('5-A-left',['6-A-left']).
next('6-A-left',['7-A-left']).
next('7-A-left',['8-A-left']).
next('8-A-left',['9-A-left','9-C-left']).
next('9-A-left',['10-A-left','10-C-left']).
next('9-B-left',['10-A-left','10-C-left']).
next('9-C-left',['9-B-left']).
next('10-A-left',['11-A-left']).
next('10-B-left',['11-A-left']).
next('10-C-left',['10-B-left']).
next('11-A-left',['12-A-left']).
next('12-A-left',['13-A-left']).
next('13-A-left',['14-A-left']).
next('14-A-left',['15-A-left']).
next('15-A-left',['16-A-left']).
next('16-A-left',['17-A-left']).
next('17-A-left', ['18-A-left']).
next('18-A-left',['19-A-left']).
next('19-A-left',['20-A-left']).
next('20-A-left',['21-A-left']).
next('21-A-left',['22-A-left']).
next('22-A-left',['23-A-left','23-A-right']).
next('23-A-left',['24-A-left']).
next('24-A-left',['25-A-left']).
next('25-A-left',['26-A-left','26-B-left']).
next('26-A-left',['27-A-left','27-B-left']).
next('26-B-left',['27-A-left','27-B-left']).
next('27-A-left',['28-A-left']).
next('27-B-left',['28-A-left']).
next('28-A-left',['29-A-left']).
next('29-A-left',['30-A-left']).
next('30-A-left',['31-A-left']).
next('31-A-left',['32-A-left']).
next('32-A-left',['33-A-left']).
next('33-A-left',['34-A-left']).
next('34-A-left',['35-A-left']).
next('35-A-left',['36-A-left']).
next('23-A-right',['24-A-right']).
next('24-A-right',['25-A-right']).
next('25-A-right',['26-D-right']).
next('26-D-right',['26-C-right']).
next('26-C-right',['27-D-right']).
next('27-D-right',['27-C-right']).
next('27-C-right', ['28-A-right']).
next('28-A-right',['29-A-right']).
next('29-A-right',['30-A-right']).
next('30-A-right',['31-A-right']).
next('31-A-right',['32-A-right']).
next('32-A-right',['33-A-right']).
next('33-A-right',['34-A-right']).
next('34-A-right',['35-A-right']).
next('35-A-right',['36-A-left']).
next('36-A-left',['37-A-left']).
next('37-A-left',['38-A-left']).
next('38-A-left',['39-A-left']).
next('39-A-left',['40-A-left']).
next('40-A-left',['41-A-left']).
next('41-A-left',['42-A-left']).
next('42-A-left',['43-A-left']).
next('43-A-left',['44-A-left']).
next('44-A-left',['45-A-left']).
next('45-A-left',['46-A-left']).
next('46-A-left',['47-A-left']).
next('47-A-left',['48-A-left']).
next('48-A-left',['49-A-left']).
next('49-A-left',['50-A-left']).
next('50-A-left',['51-A-left']).
next('51-A-left',['52-A-left']).
next('52-A-left',['53-A-left']).
next('53-A-left',['54-A-left']).
next('54-A-left',['55-A-left']).
next('55-A-left',['56-A-left']).
next('56-A-left',['57-A-left']).
next('57-A-left',['58-A-left']).
next('58-A-left',['59-A-left']).
next('59-A-left',['60-A-left']).
next('60-A-left',['61-A-left']).
next('61-A-left',['62-A-left']).
next('62-A-left',['63-A-left','63-C-left']).
next('63-A-left',['64-A-left','64-C-left']).
next('63-B-left',['64-A-left','64-C-left']).
next('63-C-left',['63-B-left']).
next('64-A-left',['65-A-left']).
next('64-B-left',['65-A-left']).
next('64-C-left',['64-B-left']).
next('65-A-left',['66-A-left']).
next('66-A-left',['67-A-left']).
next('67-A-left',['68-A-left']).
next('68-A-left',['69-A-left']).
next('69-A-left',['70-A-left']).
next('70-A-left',['71-A-left']).
next('71-A-left',['72-A-left']).
next('72-A-left',['73-A-left']).
next('73-A-left',['74-A-left']).
next('74-A-left',['75-A-left']).
next('75-A-left',['76-A-left']).
next('76-A-left',['77-A-left']).
next('77-A-left',['78-A-left']).
next('78-A-left',['79-A-left']).
next('79-A-left',['80-A-left']).
next('80-A-left',['81-A-left']).
next('81-A-left',['82-A-left']).
next('83-A-left',['84-A-left','84-A-right']).
next('84-A-left',['85-A-left']).
next('85-A-left',['86-A-left']).
next('86-A-left',['87-A-left']).
next('87-A-left',['88-A-left']).
next('88-A-left',['89-A-left']).
next('89-A-left',['90-A-left']).
next('90-A-left',['91-A-left']).
next('91-A-left',['92-A-left']).
next('92-A-left',['93-A-left']).
next('93-A-left',['94-A-left']).
next('94-A-left',['95-A-left']).
next('84-A-right',['85-A-right']).
next('85-A-right',['86-A-right']).
next('86-A-right',['87-A-right']).
next('87-A-right',['88-A-right']).
next('88-A-right',['89-C-right']).
next('89-B-right',['90-C-right']).
next('89-C-right',['89-B-right']).
next('90-B-right',['91-A-right']).
next('90-C-right',['90-B-right']).
next('91-A-right',['92-A-right']).
next('92-A-right',['93-A-right']).
next('93-A-right',['94-A-right']).
next('94-A-right',['95-A-left']).
next('95-A-left',['0-A-left']).
next('0-A-left',['-1-A-left']).
next('-1-A-left',['-2-A-left']).
next('-2-A-left',['-3-A-left']).
next('-3-A-left',['-4-A-left']).
next('-4-A-left',['-5-A-left']).
next('-5-A-left',['-6-A-left']).
next('-6-A-left',['-7-A-left']).
next('-7-A-left',['-8-A-left']).
next('-8-A-left',['-9-A-left']).
% voir comment on fait avec le fait que ça soit vide pour la dernière
% il boucle sur lui-même :)
next('-9-A-left',['9-A-left']).

% end cases of the board
end('0-A-left').
end('-1-A-left').
end('-2-A-left').
end('-3-A-left').
end('-4-A-left').
end('-5-A-left').
end('-6-A-left').
end('-7-A-left').
end('-8-A-left').
end('-9-A-left').








% TODO


% verif qu'on ne bouge pas si on est déjà sur une case fin

% verif final state c'est pour tout le monde


% et si on fait un minmax pour chaque combo de piece et on voit ce qui est le mieux?
% genre on a les cartes et 1 pion pour chaque


% rendre le code plus compréhensible en terme de noms de variables et commenter plus 


% aspirations dans le make move, voir en fct des règles
