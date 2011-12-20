:- use_module(library(lists)).
:- use_module(block).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The interpreter which takes block operators into account                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main entry point to the interpreter
eval(G) :-
    eval(G, []).

% Interpreter which allows an additional list of currently blocking computations
eval(G, Blocking) :-
    eval(G, Blocking, Blocked),
    ( [B|Bs] = Blocked ->
        eval(B, Bs)
    ;
        true
    ).

% Interpreter which takes a list of currently blocking computations, and returns
% an updated list of blocked computations
eval(G, Blocking, Blocked) :-
    % write('Evaluating: '),
    % writeln(G),

    findall(X, (member(X, Blocking), not(should_block(X))), Runnable),

    % length(Runnable, NumRunnable),
    % write('Runnable: '),
    % writeln(NumRunnable),

    ( [R|_] = Runnable ->
        % write('Resuming: '),
        % writeln(R),

        delete(Blocking, R, B),
        eval((R, G), B, Blocked)

    ; (G1, G2) = G ->
        eval(G1, Blocking, B1),
        eval(G2, B1, Blocked)

    ; true = G ->
        Blocked = Blocking,
        true

    ; (X = Y) = G ->
        Blocked = Blocking,
        X = Y

    ; (X =\= Y) = G ->
        Blocked = Blocking,
        X =\= Y

    ; (X =< Y) = G ->
        Blocked = Blocking,
        X =< Y

    ; (X @< Y) = G ->
        Blocked = Blocking,
        X @< Y

    ; (X @>= Y) = G ->
        Blocked = Blocking,
        X @>= Y

    ; (X is Y) = G ->
        Blocked = Blocking,
        X is Y

    ; length(L, X) = G ->
        Blocked = Blocking,
        length(L, X)

    ; findall(X, P, L) = G ->
        Blocked = Blocking,
        findall(X, P, L)

    ; (A -> B; C) = G ->
        (A ->
            eval(B, Blocking, Blocked)
        ;
            eval(C, Blocking, Blocked)
        )

    ; should_block(G) ->
        % write('Blocking on: '),
        % writeln(G),

        Blocked = [G|Blocking]

    ;
        clause(G, NG),
        eval(NG, Blocking, Blocked)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- block merge(-, ?, -), merge(?, -, -).
merge([], Y, Y).
merge(X, [], X).
merge([H|X], [E|Y], [H|Z]) :-
    H @< E,
    merge(X, [E|Y], Z).
merge([H|X], [E|Y], [E|Z]) :-
    H @>= E,
merge([H|X], Y, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% psort                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

psort(L, R) :-
    sorted(R),
    permute(L, R).

permute([], []).
permute(L, [X|P]) :-
    select(X, L, L1),
    permute(L1, P).

sorted([]).
sorted([_]).
sorted([X|[Y|Z]]) :-
    sorted2(X, Y, Z).

% Auxiliary function which allows us to block until the first two elements of
% the list have become available.
:- block sorted2(-, ?, ?), sorted2(?, -, ?).
sorted2(X, Y, []) :-
    X =< Y.
sorted2(X, Y, [Z|Zr]) :-
    X =< Y,
    sorted2(Y, Z, Zr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% N-Queens                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

queens(N, Qs) :-
    range(1, N, Ns),

    % This seems necessary in order to restrict the length of `Qs`. Without this
    % line, `safe` will generate arbitrarily long lists.
    length(Qs, N),

    safe(Qs),
    permute(Ns, Qs).

range(L, U, R) :-
    findall(X, between(L, U, X), R).

safe([Q|Qs]) :-
    no_attack(Q, Qs),
    safe(Qs).
safe([]).

no_attack(_, []).
no_attack(X, [Y|Z]) :-
    no_attack(X, Y, 1, Z).

% Again we have an auxiliary function so we can block on the first two elements
% in the list.
:- block no_attack(-, ?, ?, ?), no_attack(?, -, ?, ?).
no_attack(X, Y, N, []) :-
    X =\= Y + N,
    X =\= Y - N.
no_attack(X, Y, N, [Z|Zs]) :-
    X =\= Y + N,
    X =\= Y - N,
    N1 is N + 1,
    no_attack(X, Z, N1, Zs).
