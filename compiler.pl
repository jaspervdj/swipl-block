:- use_module(library(apply)).
:- use_module(block).

% Right fold using a constructor
foldr1(_, [X], X).
foldr1(P, [X|Xs], R) :-
    foldr1(P, Xs, Y), 
    functor(R, P, 2),
    arg(1, R, X),
    arg(2, R, Y).

% Given a list of indices, get a list of arguments at these indices
collect_args(_, [], []).
collect_args(Head, [I|Is], [A|As]) :-
    arg(I, Head, A),
    collect_args(Head, Is, As).

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

term_expansion(In, Out) :-
    (Head :- Body) = In,
    functor(Head, Name, Arity),

    % We generate a list of lists with indices of arguments at which the
    % predicate should block
    findall(X, blocking(Name, Arity, X), Blocking),

    % For each element, we generate a when condition
    maplist(block_when(Head), Blocking, Whens),

    % Finally, we chain these conditions together
    foldr1((;), Whens, When),

    write('For '),
    write(Head),
    write(' condition: '),
    writeln(When),

    Out = (Head :- when(When, Body)).

:- block increment(-,?).
increment(X,Y) :-
    Y is X + 1.
