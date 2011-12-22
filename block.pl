% Module providing Parsing and analyzing the block operators
:- module(block, [
    blocking/3,
    op(1150, fx, block),
    block/1,
    assert_blocking_triples/1,
    parse_block_decl/2,
    should_block/1]).

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
block(X):-
    parse_block_decl(X, L),
    assert_blocking_triples(L).

assert_blocking_triples([]).
assert_blocking_triples([(Name, Arity, L)|Ts]) :-
    write('Blocking '),
    write(Name),
    write('/'),
    write(Arity),
    write(' at: '),
    writeln(L),
    assert(blocking(Name, Arity, L)),
    assert_blocking_triples(Ts).

% Parse a block declaration return a list of triples.
parse_block_decl(D, R) :-
    ( (D1, D2) = D ->
        parse_block_decl(D1, R1),
        parse_block_decl(D2, R2),
        append(R1, R2, R)
    ;
        parse_block_decl(1, D, L),
        functor(D, Name, Arity),
        R = [(Name, Arity, L)]
    ).

% Parse a block specification. Third argument is the return value: a list of
% indices at which the code should block.
parse_block_decl(N, X, L) :-
    functor(X, _, Size),
    ( N > Size ->
        L = []
    ;
        arg(N, X, Descr),
        N1 is N + 1,
        parse_block_decl(N1, X, L1),
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
