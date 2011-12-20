:- use_module(library(apply)).
:- use_module(block).

% Convert a list to tuples e.g. [a, b, c] -> (a, (b, c)).
tuples([X], X).
tuples([X|Xs], (X, Y)) :-
    tuples(Xs, Y).

% Given a list of indices, get a list of arguments at these indices
collect_args(_, [], []).
collect_args(Head, [I|Is], [A|As]) :-
    arg(I, Head, A),
    collect_args(Head, Is, As).

% Given a list of indices at which we should block, generate a when condition
block_when(Head, Is, C) :-
    collect_args(Head, Is, Args),
    grounds(Args, Grounds),
    tuples(Grounds, C).

% Apply the ground constructor to each element
grounds([], []).
grounds([X|Xs], [Y|Ys]) :-
    Y = ground(X),
    grounds(Xs, Ys).

term_expansion(In, Out) :-
    (Head :- Body) = In,
    functor(Head, Name, Arity),

    % We generate a list of lists with indices of arguments at which the
    % predicate should block
    findall(X, blocking(Name, Arity, X), Blocking),

    % For each element, we generate a when condition
    maplist(block_when(Head), Blocking, Whens),

    % Finally, we chain these conditions together
    tuples(Whens, When),

    write('For '),
    write(Head),
    write(' condition: '),
    writeln(When),

    Out = (Head :- when(When, Body)).

:- block increment(-,?).
increment(X,Y) :-
    Y is X + 1.

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
