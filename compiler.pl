:- use_module(block).

term_expansion(In, Out) :-
    write('Expanding '),
    writeln(In),

    (Head :- Body) = In,
    functor(Head, Name, Arity),

    write('Checking: '),
    write(Name),
    write('/'),
    writeln(Arity),

    % We generate a list of lists with indices of arguments at which the
    % predicate should block
    findall(X, blocking(Name, Arity, X), Blocking),

    write('Blocking: '),
    writeln(Xs),

    Out = (Head :- Body).

% Given a list of indices, get a list of arguments at these indices
collect_args(_, [], []).
collect_args(Head, [I|Is], [A|As]) :-
    arg(I,Head,A),
    collect_args(Head,Is,As).

% Given a list of indices at which we should block, generate a when condition
block_when(Head, Is, C) :-
    collect_args(Head, Is, Args),
    grounds(Args, Grounds),
    foldr1((,), Grounds, C).

% Apply the ground constructor to each element
grounds([], []).
grounds([X|Xs], [Y|Ys]) :-
    Y = ground(X),
    grounds(Xs, Ys).

% Right fold using a constructor
foldr1(_, [X], X).
foldr1(P, [X|Xs], R) :-
    foldr1(P, Xs, Y), 
    functor(R, P, 2),
    arg(1, R, X),
    arg(2, R, Y).

when_and([X], C) :-
    C = ground(X).
when_and([X|Xs], C) :-
    when_or(Xs, Y),
    C = (ground(X), Y).

:- block increment(-,?).
increment(X,Y) :-
    Y is X + 1.
