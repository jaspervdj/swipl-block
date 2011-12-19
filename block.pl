:- dynamic blocking/2.

:- op(1150,fx,block).

% Necessary so the user can specify multiple blocking calls in a single
% operation
block((X,Y)) :-
    block(X),
    block(Y).
block(X):-
    parse_block(1, X, L),
    functor(X, Name, _),

    % Write some debug information
    write('Blocking '),
    write(Name),
    write(' at: '),
    writeln(L),

    write('Asserting: '),
    writeln((blocking(Name, L))),
    assert(blocking(Name, L)).

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

% Check whether or not a clause should block, based on already added rules
should_block(G) :-
    functor(G, N, _),
    findall(X, (blocking(N, L), blocking_args(G, L, B), length(B, X)), Lens),
    findall(X, (member(X, Lens), X > 0), Blocking),
    length(Blocking, X),
    X > 0.

% Check for a single argument
blocking_args(_, [], []).
blocking_args(G, [I|Is], Blocking) :-
    blocking_args(G, Is, B),
    arg(I, G, X),
    ( var(X) ->
        Blocking = [I|B]
    ;
        Blocking = B
    ).

eval_blocking_wrap(G) :-
    eval_blocking(G, [], _).

eval_blocking(G, Blocking, Blocked) :-
    ( (G1, G2) = G ->
        eval_blocking(G1, Blocking, B1),
        eval_blocking(G2, B1, Blocked)

    ; true = G ->
        Blocked = Blocking,
        true

    ; (X = Y) = G ->
        Blocked = Blocking,
        X = Y

    ; (A -> B; C) = G ->
        (A ->
            eval_blocking(B, Blocking, Blocked)
        ;
            eval_blocking(C, Blocking, Blocked)
        )

    ; functor(G, Name, Args) ->
        ( blocking(Name, L) ->
            writeln('Caught blocking call!')

        ;
            clause(G, NG),
            writeln(NG),
            eval_blocking(NG, Blocking, Blocked)
        )
    ).

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

    ; functor(G, Name, Args) ->
        ( blocking(Name, L) ->
            writeln('Caught blocking call!')

        ;
            clause(G, NG),
            writeln(NG),
            eval(NG)
        )
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

:- block merge(-,?,-), merge(?,-,-).
% :- block merge(-,?,-).
merge([], Y, Y).
merge(X, [], X).
merge([H|X], [E|Y], [H|Z]) :-
    H @< E,
    merge(X, [E|Y], Z).
merge([H|X], [E|Y], [E|Z]) :-
    H @>= E,
merge([H|X], Y, Z).

psort(L, R) :-
    sorted(R),
    permute(L, R),

permute([], []).
permute(L, [X|R]) :-
    select(X, L, R1),
    permute(R1, R).

sorted([]).
sorted([_]).
sorted([X|[Y|Z]]) :-
    X =< Y,
    sorted([Y|Z]).
