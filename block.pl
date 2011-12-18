:- dynamic blocking/3.

:- op(1150,fx,block).

block(X):-
    parse_block(1, X, L),
    writeln('Blocking at: '),
    writeln(L).

% Parse a block specification. Third argument is the return value: a list of
% indices at which the code should block.
parse_block(N, X, L) :-
    functor(X, _, Size),
    ( N > Size ->
        L = []
    ;
        arg(N, X, Descr),
        N1 is N + 1,
        parse_block(N1, X, L1),
        ( Descr = '?' ->
            L = L1
        ;
            L = [N|L1]
        )
    ).

:- block merge(?,-,?,-,-).

eval(G) :-
    ( (G1, G2) = G ->
        eval(G1),
        eval(G2)

    ; true = G ->
        true

    ; (X = Y) = G ->
        X = Y

    ; (A -> B; C) = G ->
        (A ->
            B
        ;
            C
        )

    ;
        clause(G, NG),
        writeln(NG),
        eval(NG)
    ).

% eval((G1,G2)) :- !,
    % eval(G1),
    % eval(G2).
% eval(true) :- !.
% eval(X = Y) :- !,
    % X = Y.
% eval(G) :-
    % clause(G,NG),
    % eval(NG).
