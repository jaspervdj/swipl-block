% Module providing Parsing and analyzing the block operators
:- module(block, [op(1150, fx, block), block/1, blocking/3, should_block/1]).

% We keep a dynamic predicate of predicates at which we should block. In this 
% structure, we store a triple for each blocking predicate. This triple holds:
%
% - The name of the predicate
% - Its arity
% - The indices of the arguments that need to be instantiated.
%
% For example,
%
%     :- block merge(-, ?, -), merge(?, -, -).
%
% will result in the triples:
%
% - (merge, 3, [1, 3])
% - (merge, 3, [2, 3])
:- dynamic blocking/3.

% Necessary so the user can specify multiple blocking calls in a single
% operation
block((X, Y)) :-
    block(X),
    block(Y).
block(X):-
    parse_block(1, X, L),
    functor(X, Name, Arity),

    % Write some debug information
    write('Blocking '),
    write(Name),
    write('/'),
    write(Arity),
    write(' at: '),
    writeln(L),

    % write('Asserting: '),
    % writeln((blocking(Name, L))),
    assert(blocking(Name, Arity, L)).

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
        ( Descr = '-' ->
            L = [N|L1]
        ;
            L = L1
        )
    ).

% Check whether or not a clause should block, based on already added rules
should_block(G) :-
    functor(G, N, A),
    findall(B, (blocking(N, A, L), blocking_args(G, L, B), L = B), Lens),
    length(Lens, X),
    X > 0.

% Given a term and a list of argument indices at which the term call should
% block when the argument is uninstantiated, generate the indices at which we
% will actually block.
blocking_args(_, [], []).
blocking_args(G, [I|Is], Blocking) :-
    blocking_args(G, Is, B),
    arg(I, G, X),
    ( ground(X) ->
        Blocking = B
    ;
        Blocking = [I|B]
    ).
