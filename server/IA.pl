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
    check_num_cards_ia(Cards1,Depth,New_cards),
    get_all_bikes(Bikes1,Bikes2,Bikes3,Bikes4,All_bikes),
    minimax(Bikes1,All_bikes,[New_cards,Bikes1],Depth,Move,_),
    test_move(Move),!.

% test_move(Move)/1
% checks if the three found a move
    % Cards : tuple
test_move(Move):-
    Move=(_,_,_).

% check_num_cards_ia(Cards,Number_cards,New_cards)/3
% checks if the hand of the player has more than one card and if not, decuplates the actual card and creates a new hand
    % Cards : list
    % Number_cards : int
    % New_cards : list
check_num_cards_ia(Cards1,1,New_cards):-
    nth0(0,Cards1,Value),
    New_cards=[Value,Value],!.

check_num_cards_ia(Cards1,_,Cards1).

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
    not(end(Old_pos)).

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
    split_string(New_pos,"_","",List_position1),
    split_string(Old_pos,"_","",List_position2),
    nth0(0,List_position1,Val1),
    split_string(Val1,"-","",List_val1),
    length(List_val1,Length1),
    nth0(0,List_position2,Val2),
    split_string(Val2,"-","",List_val2),
    length(List_val2,Lenght2),
    get_value_bike(New_pos,Value1),
    get_value_bike(Old_pos,Value2),
    get_group_score(New_pos,Bikes,Score_group),!,
    compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score).

% get_group_score(New_bike,Bikes_player,Score)/3
% gets the score depending of the distance between the bikes of the player
    % New_bike : string
    % Bikes_player : string
    % Score : int
get_group_score(New_bike,[Bike1,Bike2,Bike3],Score):-
    get_sub_value_group(New_bike,Bike1,Bike2,V1),
    get_sub_value_group(New_bike,Bike1,Bike3,V2),
    get_sub_value_group(New_bike,Bike3,Bike2,V3),
    get_min(V1,V2,Min1),
    get_min(Min1,V3,Min2),
    Score is 5+Min2.

% get_sub_value_group(New_bike,Bike1,Bike2,Value)/4
% gets the score of a sub group
    % New_bike : string
    % Bike1 : string
    % Bike2 : string
    % Value : int
get_sub_value_group(New_bike,Bike1,Bike2,Value):-
    get_value_bike(New_bike,Value1),
    get_value_bike(Bike2,Value2),
    get_value_bike(Bike1,Value3),
    Value is Value2+Value3-Value1.
    

% get_value_bike(Bike,Value)/2
% get the value of the position of a bike
    % Bike : string
    % Value : int
get_value_bike(Bike,Value):-
    split_string(Bike,"_","",List_bike),
    nth0(0,List_bike,Val),
    split_string(Val,"-","",List_val),
    length(List_val,Length),
    Length=2,
    nth0(1,List_val,Value_string),
    number_string(Value,Value_string).

get_value_bike(Bike,Value):-
    split_string(Bike,"_","",List_bike),
    nth0(0,List_bike,Val),
    split_string(Val,"-","",List_val),
    length(List_val,Length),
    length(List_val,Length),
    Length=1,
    nth0(0,List_val,Value_string),
    number_string(Value,Value_string).

% get_min(Value1,Value2,Result)/3
% gets the minimum between two integers
    % Value1 : int
    % Value2 : int
    % Result : int
get_min(Val1,Val2,Val1):-
    Val1=<Val2.

get_min(_,Val2,Val2).

% compute_score(Value1,Value2,Length1,Length2,Score_group,Score)/6
% compute the score for a leaf of the three
    % Value1 : int
    % Value2 : int
    % Length1 : int
    % Lenght2 : int
    % Score_group : int
    % Score : int
compute_score(_,_,Length1,Length2,_,Score):-
    Length2>Length1,
    Score is 0,!.

compute_score(_,Value2,_,_,Score_group,Score):-
    Value2=0,
    Score is 100+Score_group,!.

compute_score(Value1,_,Length1,Length2,Score_group,Score):-
    Length2<Length1,
    Score is Value1+30+Score_group,!.

compute_score(Value1,Value2,Length1,Lenght2,Score_group,Score):-
    Length1=Lenght2,
    Score is Value1-Value2+Score_group,!.

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
terminal_state([_,[Bike1,Bike2,Bike3]]):-
    end(Bike1);
    end(Bike2);
    end(Bike3).

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
available_places('0_B_left',15).
available_places('1_A_left',3).
available_places('2_A_left',3).
available_places('3_A_left',3).
available_places('4_A_left',3).
available_places('5_A_left',3).
available_places('6_A_left',3).
available_places('7_A_left',3).
available_places('8_A_left',3).
available_places('9_A_left',1).
available_places('9_B_left',1).
available_places('9_C_left',1).
available_places('10_A_left',1).
available_places('10_B_left',1).
available_places('10_C_left',1).
available_places('11_A_left',2).
available_places('12_A_left',2).
available_places('13_A_left',2).
available_places('14_A_left',2).
available_places('15_A_left',2).
available_places('16_A_left',2).
available_places('17_A_left',2).
available_places('18_A_left',2).
available_places('19_A_left',3).
available_places('20_A_left',3).
available_places('21_A_left',3).
available_places('22_A_left',3).
available_places('23_A_left',2).
available_places('24_A_left',2).
available_places('25_A_left',2).
available_places('26_A_left',1).
available_places('26_B_left',1).
available_places('27_A_left',1).
available_places('27_A_left',1).
available_places('28_A_left',2).
available_places('29_A_left',2).
available_places('30_A_left',2).
available_places('31_A_left',2).
available_places('32_A_left',2).
available_places('33_A_left',2).
available_places('34_A_left',2).
available_places('35_A_left',2).
available_places('23_A_right',1).
available_places('24_A_right',1).
available_places('25_A_right',1).
available_places('26_D_right',1).
available_places('26_C_right',1).
available_places('27_D_right',1).
available_places('27_C_right',1).
available_places('28_A_right',1).
available_places('29_A_right',1).
available_places('30_A_right',1).
available_places('31_A_right',1).
available_places('32_A_right',1).
available_places('33_A_right',1).
available_places('34_A_right',1).
available_places('35_A_right',1).
available_places('36_A_left',2).
available_places('37_A_left',2).
available_places('38_A_left',2).
available_places('39_A_left',2).
available_places('40_A_left',2).
available_places('41_A_left',2).
available_places('42_A_left',2).
available_places('43_A_left',2).
available_places('44_A_left',2).
available_places('45_A_left',2).
available_places('46_A_left',2).
available_places('47_A_left',2).
available_places('48_A_left',2).
available_places('49_A_left',2).
available_places('50_A_left',2).
available_places('51_A_left',2).
available_places('52_A_left',2).
available_places('53_A_left',2).
available_places('54_A_left',2).
available_places('55_A_left',2).
available_places('56_A_left',2).
available_places('57_A_left',2).
available_places('58_A_left',2).
available_places('59_A_left',2).
available_places('60_A_left',2).
available_places('61_A_left',2).
available_places('62_A_left',2).
available_places('63_A_left',1).
available_places('63_B_left',1).
available_places('63_C_left',1).
available_places('64_A_left',1).
available_places('64_B_left',1).
available_places('64_C_left',1).
available_places('65_A_left',2).
available_places('66_A_left',2).
available_places('67_A_left',2).
available_places('68_A_left',2).
available_places('69_A_left',2).
available_places('70_A_left',2).
available_places('71_A_left',2).
available_places('72_A_left',2).
available_places('73_A_left',1).
available_places('74_A_left',1).
available_places('75_A_left',1).
available_places('76_A_left',2).
available_places('77_A_left',2).
available_places('78_A_left',2).
available_places('79_A_left',2).
available_places('80_A_left',2).
available_places('81_A_left',2).
available_places('82_A_left',2).
available_places('83_A_left',2).
available_places('84_A_left',1).
available_places('85_A_left',1).
available_places('86_A_left',1).
available_places('87_A_left',1).
available_places('88_A_left',1).
available_places('89_A_left',1).
available_places('90_A_left',1).
available_places('91_A_left',1).
available_places('92_A_left',1).
available_places('93_A_left',1).
available_places('94_A_left',1).
available_places('84_A_right',1).
available_places('85_A_right',1).
available_places('86_A_right',1).
available_places('87_A_right',1).
available_places('88_A_right',1).
available_places('89_B_right',1).
available_places('89_C_right',1).
available_places('90_B_right',1).
available_places('90_C_right',1).
available_places('91_A_right',1).
available_places('92_A_right',1).
available_places('93_A_right',1).
available_places('94_A_right',1).
available_places('95_A_left',3).
available_places('0_A_left',3).
available_places('-1_A_left',3).
available_places('-2_A_left',3).
available_places('-3_A_left',3).
available_places('-4_A_left',3).
available_places('-5_A_left',3).
available_places('-6_A_left',3).
available_places('-7_A_left',3).
available_places('-8_A_left',3).
available_places('-9_A_left',3).

% linked cases on the board
next('0_B_left',['1_A_left']).
next('1_A_left',['2_A_left']).
next('2_A_left',['3_A_left']).
next('3_A_left',['4_A_left']).
next('4_A_left',['5_A_left']).
next('5_A_left',['6_A_left']).
next('6_A_left',['7_A_left']).
next('7_A_left',['8_A_left']).
next('8_A_left',['9_A_left','9_C_left']).
next('9_A_left',['10_A_left','10_C_left']).
next('9_B_left',['10_A_left','10_C_left']).
next('9_C_left',['9_B_left']).
next('10_A_left',['11_A_left']).
next('10_B_left',['11_A_left']).
next('10_C_left',['10_B_left']).
next('11_A_left',['12_A_left']).
next('12_A_left',['13_A_left']).
next('13_A_left',['14_A_left']).
next('14_A_left',['15_A_left']).
next('15_A_left',['16_A_left']).
next('16_A_left',['17_A_left']).
next('17_A_left', ['18_A_left']).
next('18_A_left',['19_A_left']).
next('19_A_left',['20_A_left']).
next('20_A_left',['21_A_left']).
next('21_A_left',['22_A_left']).
next('22_A_left',['23_A_left','23_A_right']).
next('23_A_left',['24_A_left']).
next('24_A_left',['25_A_left']).
next('25_A_left',['26_A_left','26_B_left']).
next('26_A_left',['27_A_left','27_B_left']).
next('26_B_left',['27_A_left','27_B_left']).
next('27_A_left',['28_A_left']).
next('27_B_left',['28_A_left']).
next('28_A_left',['29_A_left']).
next('29_A_left',['30_A_left']).
next('30_A_left',['31_A_left']).
next('31_A_left',['32_A_left']).
next('32_A_left',['33_A_left']).
next('33_A_left',['34_A_left']).
next('34_A_left',['35_A_left']).
next('35_A_left',['36_A_left']).
next('23_A_right',['24_A_right']).
next('24_A_right',['25_A_right']).
next('25_A_right',['26_D_right']).
next('26_D_right',['26_C_right']).
next('26_C_right',['27_D_right']).
next('27_D_right',['27_C_right']).
next('27_C_right', ['28_A_right']).
next('28_A_right',['29_A_right']).
next('29_A_right',['30_A_right']).
next('30_A_right',['31_A_right']).
next('31_A_right',['32_A_right']).
next('32_A_right',['33_A_right']).
next('33_A_right',['34_A_right']).
next('34_A_right',['35_A_right']).
next('35_A_right',['36_A_left']).
next('36_A_left',['37_A_left']).
next('37_A_left',['38_A_left']).
next('38_A_left',['39_A_left']).
next('39_A_left',['40_A_left']).
next('40_A_left',['41_A_left']).
next('41_A_left',['42_A_left']).
next('42_A_left',['43_A_left']).
next('43_A_left',['44_A_left']).
next('44_A_left',['45_A_left']).
next('45_A_left',['46_A_left']).
next('46_A_left',['47_A_left']).
next('47_A_left',['48_A_left']).
next('48_A_left',['49_A_left']).
next('49_A_left',['50_A_left']).
next('50_A_left',['51_A_left']).
next('51_A_left',['52_A_left']).
next('52_A_left',['53_A_left']).
next('53_A_left',['54_A_left']).
next('54_A_left',['55_A_left']).
next('55_A_left',['56_A_left']).
next('56_A_left',['57_A_left']).
next('57_A_left',['58_A_left']).
next('58_A_left',['59_A_left']).
next('59_A_left',['60_A_left']).
next('60_A_left',['61_A_left']).
next('61_A_left',['62_A_left']).
next('62_A_left',['63_A_left','63_C_left']).
next('63_A_left',['64_A_left','64_C_left']).
next('63_B_left',['64_A_left','64_C_left']).
next('63_C_left',['63_B_left']).
next('64_A_left',['65_A_left']).
next('64_B_left',['65_A_left']).
next('64_C_left',['64_B_left']).
next('65_A_left',['66_A_left']).
next('66_A_left',['67_A_left']).
next('67_A_left',['68_A_left']).
next('68_A_left',['69_A_left']).
next('69_A_left',['70_A_left']).
next('70_A_left',['71_A_left']).
next('71_A_left',['72_A_left']).
next('72_A_left',['73_A_left']).
next('73_A_left',['74_A_left']).
next('74_A_left',['75_A_left']).
next('75_A_left',['76_A_left']).
next('76_A_left',['77_A_left']).
next('77_A_left',['78_A_left']).
next('78_A_left',['79_A_left']).
next('79_A_left',['80_A_left']).
next('80_A_left',['81_A_left']).
next('81_A_left',['82_A_left']).
next('83_A_left',['84_A_left','84_A_right']).
next('84_A_left',['85_A_left']).
next('85_A_left',['86_A_left']).
next('86_A_left',['87_A_left']).
next('87_A_left',['88_A_left']).
next('88_A_left',['89_A_left']).
next('89_A_left',['90_A_left']).
next('90_A_left',['91_A_left']).
next('91_A_left',['92_A_left']).
next('92_A_left',['93_A_left']).
next('93_A_left',['94_A_left']).
next('94_A_left',['95_A_left']).
next('84_A_right',['85_A_right']).
next('85_A_right',['86_A_right']).
next('86_A_right',['87_A_right']).
next('87_A_right',['88_A_right']).
next('88_A_right',['89_C_right']).
next('89_B_right',['90_C_right']).
next('89_C_right',['89_B_right']).
next('90_B_right',['91_A_right']).
next('90_C_right',['90_B_right']).
next('91_A_right',['92_A_right']).
next('92_A_right',['93_A_right']).
next('93_A_right',['94_A_right']).
next('94_A_right',['95_A_left']).
next('95_A_left',['0_A_left']).
next('0_A_left',['-1_A_left']).
next('-1_A_left',['-2_A_left']).
next('-2_A_left',['-3_A_left']).
next('-3_A_left',['-4_A_left']).
next('-4_A_left',['-5_A_left']).
next('-5_A_left',['-6_A_left']).
next('-6_A_left',['-7_A_left']).
next('-7_A_left',['-8_A_left']).
next('-8_A_left',['-9_A_left']).
next('-9_A_left',['-9_A_left']).

% end cases of the board
end('0_A_left').
end('-1_A_left').
end('-2_A_left').
end('-3_A_left').
end('-4_A_left').
end('-5_A_left').
end('-6_A_left').
end('-7_A_left').
end('-8_A_left').
end('-9_A_left').