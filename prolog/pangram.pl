% a = 97, z = 122

is_pangram(S) :-
    sorted_codes(S, Codes),
    alpha_only(Codes, AlphaCodes),
    length(AlphaCodes, Length),
    Length =:= 26.

sorted_codes(S, Codes) :-
    string_lower(S, Lower),
    string_chars(Lower, Chars),
    sort(Chars,Sorted),
    string_codes(Sorted, Codes).

alpha_only([], []).
alpha_only([H|T], [H|Rest]) :-
    H >= 97,  
    H =< 122, 
    alpha_only(T, Rest).
alpha_only([H|T], Rest) :-
    (H < 97 ; H > 122),
    alpha_only(T, Rest).


% prefix([],List).
% prefix([X|Prefix],[X|List]) :- prefix(Prefix,List).


    