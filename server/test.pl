spliter(Text, List) :-
    split_string(Text, "*", "", PlayerText),
    maplist(sub_split, PlayerText, List),
    write("List: "), writeln(List).

sub_split(PlayerText, SubList) :-
    split_string(PlayerText, ".", "", ParamText),
    split_again(ParamText, SubList),
    write("SubList: "), writeln(SubList).

split_again([ID|OtherParam], [IDInt|NewParam]) :-
    number_string(IDInt, ID),
    subsub_split(OtherParam, NewParam).

subsub_split([FirstElement|SecondElement], [FirstList|SecondList]) :-
    split_int(FirstElement, FirstList),
    split_atom(SecondElement, SecondList),
    write("FirstList: "), writeln(FirstList),
    write("SecondList: "), writeln(SecondList).

split_int(Input, Output) :-
    split_string(Input, ";", "", ElementString),
    maplist(number_string, Output, ElementString),
    write("Output: "), writeln(Output).

split_atom([Input], [Output]) :-
    split_string(Input, ";", "", ElementString),
    maplist(string_to_atom, ElementString, Output),
    write("Output: "), writeln(Output).



% Exemple d'utilisation :
% Input = "0.12;2;11;11;3.0_B_left;0_B_left;0_B_left%1.10;5;1;12;7.0_B_left;0_B_left;0_B_left%2.7;2;3;4;9.0_B_left;0_B_left;0_B_left%3.8;6;8;12;5.0_B_left;0_B_left;0_B_left",
% split_main_string(Input, Substrings).