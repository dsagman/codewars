% F(0) = 1
% M(0) = 0
% F(n) = n - M(F(n - 1))
% M(n) = n - F(M(n - 1))

:- table f/2.
:- table m/2.

f(0,1) :- !.
f(N,What) :- 
    N > 0, 
    N1 is N - 1, 
    f(N1,F1), 
    m(F1,M1), 
    What is N - M1.
    
m(0,0) :- !.
m(N,What) :- 
    N > 0, 
    N1 is N - 1, 
    m(N1,M1), 
    f(M1,F1), 
    What is N - F1.
