:- module(interpreter, [eval/1]).

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
        % If B and G can be unified, we need to get out, otherwise we'd be
        % blocking forever.
        ( B = G ->
            false
        ;
            eval(B, Bs)
        )
    ;
        true
    ).

% Interpreter which takes a list of currently blocking computations, and returns
% an updated list of blocked computations
eval(G, Blocking, Blocked) :-
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
