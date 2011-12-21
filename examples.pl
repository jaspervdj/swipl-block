:- module(examples, [
    merge/3,
    psort/2, permute/2, sorted/1, sorted/3,
    queens/2, range/3, safe/1, no_attack/2, no_attack/4]).

:- use_module(block).

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
:- block sorted(-, ?, ?), sorted(?, -, ?).
sorted(X, Y, []) :-
    X =< Y.
sorted(X, Y, [Z|Zr]) :-
    X =< Y,
    sorted(Y, Z, Zr).

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
