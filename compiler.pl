:- module(compiler, [compile/2]).

:- use_module(library(apply)).
:- use_module(block).

% Right fold using a constructor
foldr1(_, [X], X).
foldr1(P, [X|Xs], C) :-
    foldr1(P, Xs, Y),
    C =.. [P, X, Y].

% Given a list of indices, get a list of arguments at these indices
collect_args(_, [], []).
collect_args(Head, [I|Is], [A|As]) :-
    arg(I, Head, A),
    collect_args(Head, Is, As).

% Given a list of indices at which we should block, generate a when condition
block_when(Head, Is, C) :-
    collect_args(Head, Is, Args),
    nonvars(Args, Nonvars),
    foldr1((;), Nonvars, C).

% Apply the nonvar constructor to each element
nonvars([], []).
nonvars([X|Xs], [nonvar(X)|Ys]) :-
    nonvars(Xs, Ys).

% Unify the arguments of two functors
unify_args(F, G, (A1 = A2)) :-
    F =.. [_|A1],
    G =.. [_|A2].

% A term expansion to expand a fact into a predicate with a body
expand_fact(In, Out) :-
    ( (_ :- _) = In ->
        Out = In
    ;
        Out = (In :- true)
    ).

% A term expansion to add blocking rules to predicates
add_blocking(In, Out) :-
    % Create a new head to wrap all the arguments
    (Head :- Body) = In,
    functor(Head, Name, Arity),
    functor(WrapHead, Name, Arity),
    unify_args(WrapHead, Head, UnifyArgs),

    % We generate a list of lists with indices of arguments at which the
    % predicate should block. For each element, we generate a when condition and
    % then we chain these conditions together.
    findall(X, blocking(Name, Arity, X), Blocking),
    maplist(block_when(WrapHead), Blocking, Whens),
    foldr1((,), Whens, When),

    Out = (WrapHead :- when(When, (UnifyArgs, Body))),

    write(In),
    write(' -> '),
    writeln(Out).

% Our compiler
compile(In, Out) :-
    expand_fact(In, X),
    add_blocking(X, Out).
