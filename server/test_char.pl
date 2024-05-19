split_main_string(Input, Substrings) :-
    split_string(Input, "*", "*", MainChar),
    maplist(split_inner_string, MainChar, Substrings).

split_inner_string(MainString, InnerSubstrings) :-
    split_string(MainString, ".", ".", ElementChar),
    split_again(ElementChar, InnerSubstrings).
    maplist(split_values, ElementChar, InnerSubstrings).

split_again([ID|Rest], InnerSubstrings) :-
    split_string(Rest, ";", ";", InnerSubstrings).

split_values(String, Values) :-
    (   sub_string(String, Before, _, After, ";") ->
        sub_string(String, 0, Before, _, Value1),
        sub_string(String, Before, After, _, Value2),
        sub_string(String, _, After, 0, Value3),
        Values = [Value1, Value2, Value3]
    ;   Values = [String]
    ).

% Exemple d'utilisation :
% Input = "0.12;2;11;11;3.0_B_left;0_B_left;0_B_left%1.10;5;1;12;7.0_B_left;0_B_left;0_B_left%2.7;2;3;4;9.0_B_left;0_B_left;0_B_left%3.8;6;8;12;5.0_B_left;0_B_left;0_B_left",
% split_main_string(Input, Substrings).