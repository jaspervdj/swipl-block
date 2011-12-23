:- module(compiler, [compile/2]).

:- use_module(library(apply)).
:- use_module(block).

% Right fold using a constructor
foldr1(_, [X], X).
foldr1(P, [X | Xs], C) :-
    foldr1(P, Xs, Y),
    C =.. [P, X, Y].

% Given a list of indices, get a list of arguments at these indices
collect_args(_, [], []).
collect_args(Head, [I | Is], [A | As]) :-
    arg(I, Head, A),
    collect_args(Head, Is, As).

% Given a list of indices at which we should block, generate a when condition
block_when(Head, Is, C) :-
    collect_args(Head, Is, Args),
    nonvars(Args, Nonvars),
    foldr1((;), Nonvars, C).

% Apply the nonvar constructor to each element
nonvars([], []).
nonvars([X | Xs], [nonvar(X) | Ys]) :-
    nonvars(Xs, Ys).

% Unify the arguments of two functors
unify_args(F, G, (A1 = A2)) :-
    F =.. [_ | A1],
    G =.. [_ | A2].

% Get the name for the wrapper method
hide_name(In, Out) :-
    concat_atom([compiler_block_, In], Out).

% Compiler for block statements
compile_block(In, Out) :-
    % Detect a block statement. Assert the tripes as well, so we can check for
    % them in hide_rule!
    (:- Body) = In,
    Body =.. [block | [Decl]],
    parse_block_decl(Decl, Triples),
    assert_blocking_triples(Triples),

    % For now, we assume that all declarations in a single block statement refer
    % to the same method.
    [(Name, Arity, _) | _] = Triples,

    % Create the head for our rule
    functor(Head, Name, Arity),

    % We generate a list of lists with indices of arguments at which the
    % predicate should block. For each element, we generate a when condition and
    % then we chain these conditions together.
    findall(X, member((_, _, X), Triples), Blocking),
    maplist(block_when(Head), Blocking, Whens),
    foldr1((,), Whens, When),

    % Create the body for our rule which calls our renamed function
    Head =.. [_ | Args],
    hide_name(Name, HiddenName),
    Call =.. [HiddenName | Args],
    Out = (Head :- when(When, Call)).

% Hide the actual rules; the user will use those produced by compile_block.
hide_rule(In, Out) :-
    ( (Head :- Body) = In ->
        true
    ;
        Head = In,
        Body = true
    ),

    % Change the name if we should block
    functor(Head, Name, Arity),
    findall(X, blocking(Name, Arity, X), Blocking),
    ( Blocking = [] ->
        false
    ;
        hide_name(Name, HiddenName),
        Head =.. [_ | Args],
        HiddenHead =.. [HiddenName | Args],
        Out = (HiddenHead :- Body)
    ).

% Our compiler
compile(In, Out) :-
    ( compile_block(In, Out)
    ; hide_rule(In, Out)
    ).
