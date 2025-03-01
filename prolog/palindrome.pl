dumb :-
    H = "hello",
    reverse_string(H, R),
    writeln(H),
    writeln(R).

reverse_string(S, R) :-
    string_chars(S, C),
    reverse(C, R1),
    string_chars(R, R1).

palindrome(S) :-
    reverse_string(S, R),
    S = R.

