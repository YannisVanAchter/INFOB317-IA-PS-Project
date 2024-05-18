:- module(ia, [get_move_IA/2]).

% get_move_IA(Infos,Move)/2
% main predicate that receive the state of the board and returns the move
    % Infos : list
    % Move : tuple
get_move_IA(Infos,Move):-
    get_infos_player(Infos,0,[Cards1,Bikes1]),
    get_infos_player(Infos,1,[_,Bikes2]),
    get_infos_player(Infos,2,[_,Bikes3]),
    get_infos_player(Infos,3,[_,Bikes4]),
    length(Cards1,Depth),
    get_all_bikes(Bikes1,Bikes2,Bikes3,Bikes4,All_bikes),
    minimax(Bikes1,All_bikes,[Cards1,Bikes1],Depth,Move,_),!.

% get_infos_player(State,Num_player,Infos)/3
% there is not base case since every player should be present one time only, and each shoul be present
% extracts the infos of the player from the global data
    % State : list
    % Num_player : int
    % Infos : list
get_infos_player([H_infos|_],Num_player,Infos):-
    nth0(0,H_infos,Num),
    nth0(1,H_infos,Hand),
    nth0(2,H_infos,Bikes),
    Num = Num_player,
    Infos=[Hand,Bikes],!.

get_infos_player([_|T_infos],Num_player,Infos):-
    get_infos_player(T_infos,Num_player,Infos).

% get_all_bikes(Bike1,Bike2,Bike3,Bike4,All)/5
% gets a list of all the current bikes on the board
    % Bike1 : list
    % Bike2 : list
    % Bike3 : list
    % Bike4 : list
    % All : list
get_all_bikes(Bikes1,Bikes2,Bikes3,Bikes4,All):-
    append(Bikes1,Bikes2,Acc),
    append(Acc,Bikes3,Acc2),
    append(Acc2,Bikes4,All).

% get_count_elem(Target,Elements,Acc,Count)/4
% gets the number of occurences of an element in a list
    % Target : atom
    % Elements : list
    % Acc : int
    % Count : int
get_count_elem(_,[],Acc,Acc).

get_count_elem(Target,[Elem|Other_elems],Acc,Count):-
    Target=Elem,
    New_acc is Acc+1,
    get_count_elem(Target,Other_elems,New_acc,Count),!.

get_count_elem(Target,[_|Other_elems],Acc,Count):-
    get_count_elem(Target,Other_elems,Acc,Count),!.
    
% minimax(Bikes_player,All_bikes,State,Depth,Best_move,Min_score)/6
% creates and explore the three
    % Bikes_player : list
    % All_bikes : list 
    % State : list
    % Depth : int
    % Best_move : tuple
    % Min_score : int
minimax(Bikes_player,All_bikes,[Cards,Bike],Depth,Best_move,Min_score):-
    writeln([Cards,Bike]),
    possible_moves(Bike,Cards,[],Moves),
    evaluate_moves(Bikes_player,All_bikes,Moves,[Cards,Bike],Depth,Min_score,Best_move),!.

% evaluate_move(Bikes_player,All_bike,Moves,State,Depth,Best_score,Best_move)/7
% evalues a move in the three
    % Bikes_player : list
    % All_bike : list
    % Moves : list
    % State : list
    % Depth : int
    % Best_score : int
    % Best_move : tuple
evaluate_moves(_,_,[],_,_,0,_).

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    terminal_state(New_state),
    score(Move,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    terminal_state(New_state),
    score(Move,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,0,Best_score,Best_move):-
    score(Move,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,0,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,0,Best_score,Best_move):-
    score(Move,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,0,Next_max_score,Next_best_move),
    Next_max_score>Score,
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    nth0(0,New_state,New_cards),
    length(New_cards,Lenght),
    Lenght=0,
    score(Move,Bikes_player,_),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    make_move(State,Move,New_state),
    nth0(0,New_state,New_cards),
    length(New_cards,Lenght),
    Lenght=0,
    score(Move,Bikes_player,Score),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,_),
    Next_max_score=<Score,
    Best_score=Score,
    Best_move=Move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    not(valid_move(Move,All_bikes)),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
    Best_score=Next_max_score,
    Best_move=Next_best_move.

evaluate_moves(Bikes_player,All_bikes,[Move|Other_moves],State,Depth,Best_score,Best_move):-
    not(valid_pos(Move)),
    evaluate_moves(Bikes_player,All_bikes,Other_moves,State,Depth,Next_max_score,Next_best_move),
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

% make_move(State,Move,New_state)/3
% update the current state to the new state using the move done
    % State : list
    % Move : tuple
    % New_state : list
make_move([Cards,_],(Card,New_pos,_),[New_cards,[New_pos]]):-
    select(Card,Cards,New_cards).

% valid_pos(Move)/1
% test if a move is not made on a bike that has finished the game
    % Move : tuple
valid_pos((_,_,Old_pos)):-
    split_string(Old_pos,"-","",List_pos),
    length(List_pos,Lenght),
    Lenght=3,!.

% valid_move(Move,All_bikes)/2
% test if a move is valid
    % Move : tuple
    % All_bikes : list
valid_move((_,New_pos,_),All_bikes):-
    get_count_elem(New_pos,All_bikes,0,Places_used),
    available_places(New_pos,Actual_places),
    Actual_places>Places_used.

% score(Move,Bikes,Score)/3
% gets the score of a leaf
    % Move : tuple
    % Bikes : list
    % Score : int
score((_,New_pos,Old_pos),Bikes,Score):-
    split_string(New_pos,"-","",List_position1),
    split_string(Old_pos,"-","",List_position2),
    length(List_position1,Length1),
    length(List_position2,Lenght2),
    get_value_bike(New_pos,Value1),
    get_value_bike(Old_pos,Value2),
    get_group_score(Old_pos,New_pos,Bikes,Score_group),
    compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score).

% get_group_score(Old_bike,New_bike,Bikes_player,Score)/4
% gets the score depending of the distance between the bikes of the player
    % Old_bike : string
    % New_bike : string
    % Bikes_player : string
    % Score : int
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
    get_value_bike(Bike1,Value3),
    Score is 4+(Value1-Value2-Value3).

% get_value_bike(Bike,Value)/2
% get the value of the position of a bike
    % Bike : string
    % Value : int
get_value_bike(Bike,Value):-
    split_string(Bike,"-","",List_bike),
    length(List_bike,Length),
    Length=4,
    nth0(1,List_bike,Value_string),
    number_string(Value,Value_string).

get_value_bike(Bike,Value):-
    split_string(Bike,"-","",List_bike),
    length(List_bike,Length),
    Length=3,
    nth0(0,List_bike,Value_string),
    number_string(Value,Value_string).

% compute_score(Value1,Value2,Length1,Length2,Score_group,Score)/6
% compute the score for a leaf of the three
    % Value1 : int
    % Value2 : int
    % Length1 : int
    % Lenght2 : int
    % Score_group : int
    % Score : int
compute_score(_,_,Length1,Lenght2,_,Score):-
    Score is 0,
    Length1>Lenght2,!.

compute_score(_,Value2,Length1,Lenght2,Score_group,Score):-
    Length1<Lenght2,
    Score is Value2+30+Score_group,!.

compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score):-
    Length1=Lenght2,
    Score is Value2-Value1+Score_group,!.

% possible_moves(Bikes,Cards,Acc,Moves)/4
% gets all the possible moves with the actual hand and position of the bikes
    % Bikes : list
    % Cards : list
    % Acc : list
    % Moves : list
possible_moves([],_,Acc,Acc).

possible_moves([Bike|Other_bike],Cards,Acc,Moves):-
    iterate_card(Cards,Bike,[],Moves_bike),
    append(Acc,Moves_bike,New_acc),
    possible_moves(Other_bike, Cards, New_acc, Moves).

% iterate_card(CardsnBike,Acc,Moves)/4
% find the next position with a recursion on the value of the card
    % Cards : list
    % Bike : string
    % Acc : list
    % Moves : list
iterate_card([],_,Acc,Acc).

iterate_card([Card|Other_cards],Bike,Acc,Moves):-
    get_next_position(Bike,Card,New_position),
    append(Acc,[(Card,New_position,Bike)],New_acc),
    iterate_card(Other_cards,Bike,New_acc,Moves),!.

% terminal_state(State)/1
% test if the state is terminal
    % State : list
terminal_state([_,[Bike,_]]):-
    end(Bike).

% get_next_position(Position,Card_value,Res)/3
% gets the next position of a bike for a certain card
    % Position : string
    % Card_value : int
    % Res : String
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