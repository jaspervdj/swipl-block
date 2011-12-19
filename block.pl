:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing and evaluating the block operators                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We keep a dynamic predicate of predicates at which we should block. In this 
% structure, we store a triple for each blocking predicate. This triple holds:
%
% - The name of the predicate
% - Its arity
% - The indices of the arguments that need to be instantiated.
%
% For example,
%
%     :- block merge(-,?,-), merge(?,-,-).
%
% will result in the triples:
%
% - (merge, 3, [1, 3])
% - (merge, 3, [2, 3])
:- dynamic blocking/3.

:- op(1150,fx,block).

% Necessary so the user can specify multiple blocking calls in a single
% operation
block((X,Y)) :-
    block(X),
    block(Y).
block(X):-
    parse_block(1,X,L),
    functor(X,Name,Arity),

    % Write some debug information
    write('Blocking '),
    write(Name),
    write('/'),
    write(Arity),
    write(' at: '),
    writeln(L),

    % write('Asserting: '),
    % writeln((blocking(Name,L))),
    assert(blocking(Name,Arity,L)).

% Parse a block specification. Third argument is the return value: a list of
% indices at which the code should block.
parse_block(N,X,L) :-
    functor(X,_,Size),
    ( N > Size ->
        L = []
    ;
        arg(N,X,Descr),
        N1 is N + 1,
        parse_block(N1,X,L1),
        ( Descr = '?' ->
            L = L1
        ;
            L = [N|L1]
        )
    ).

% Check whether or not a clause should block, based on already added rules
should_block(G) :-
    functor(G,N,A),
    findall(X,(blocking(N,A,L),blocking_args(G,L,B),length(B,X)),Lens),
    findall(X,(member(X,Lens),X > 0),Blocking),
    length(Blocking,X),
    X > 0.

% Check for a single argument
blocking_args(_,[],[]).
blocking_args(G,[I|Is],Blocking) :-
    blocking_args(G,Is,B),
    arg(I,G,X),
    ( ground(X) ->
        Blocking = B
    ;
        Blocking = [I|B]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The interpreter which takes block operators into account                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval(G) :-
    eval(G,[],_).

eval(G,Blocking,Blocked) :-
    % write('Evaluating: '),
    % writeln(G),

    findall(X,(member(X,Blocking),not(should_block(X))),Runnable),

    % length(Runnable,NumRunnable),
    % write('Runnable: '),
    % writeln(NumRunnable),

    ( [R|_] = Runnable ->
        % write('Resuming: '),
        % writeln(R),

        delete(Blocking,R,B),
        eval((R,G),B,Blocked)

    ; (G1,G2) = G ->
        eval(G1,Blocking,B1),
        eval(G2,B1,Blocked)

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

    ; (X is Y) = G ->
        Blocked = Blocking,
        X is Y

    ; length(L,X) = G ->
        Blocked = Blocking,
        length(L,X)

    ; findall(X,P,L) = G ->
        Blocked = Blocking,
        findall(X,P,L)

    ; (A -> B; C) = G ->
        (A ->
            eval(B,Blocking,Blocked)
        ;
            eval(C,Blocking,Blocked)
        )

    ; should_block(G) ->
        Blocked = [G|Blocking]

    ;
        clause(G,NG),
        eval(NG,Blocking,Blocked)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% psort                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

psort(L,R) :-
    sorted(R),
    permute(L,R).

permute([],[]).
permute(L,[X|P]) :-
    select(X,L,L1),
    permute(L1,P).

sorted([]).
sorted([_]).
sorted([X|[Y|Z]]) :-
    sorted2(X,Y,Z).

% Auxiliary function which allows us to block until the first two elements of
% the list have become available.
:- block sorted2(-,-,?).
sorted2(X,Y,[]) :-
    X =< Y.
sorted2(X,Y,[Z|Zr]) :-
    X =< Y,
    sorted2(Y,Z,Zr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- block merge(-,?,-),merge(?,-,-).
merge([],Y,Y).
merge(X,[],X).
merge([H|X],[E|Y],[H|Z]) :-
    H @< E,
    merge(X,[E|Y],Z).
merge([H|X],[E|Y],[E|Z]) :-
    H @>= E,
merge([H|X],Y,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% N-Queens                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

queens(N,Qs) :-
    range(1,N,Ns),

    % This seems necessary in order to restrict the length of `Qs`. Without this
    % line, `safe` will generate arbitrarily long lists.
    length(Qs,N),

    safe(Qs),
    permute(Ns,Qs).

range(L,U,R) :-
    findall(X,between(L,U,X),R).

safe([Q|Qs]) :-
    no_attack(Q,Qs),
    safe(Qs).
safe([]).

no_attack(_,[]).
no_attack(X,[Y|Z]) :-
    no_attack(X,Y,1,Z).

% Again we have an auxiliary function so we can block on the first two elements
% in the list.
:- block no_attack(-,-,-,?).
no_attack(X,Y,N,[]) :-
    X =\= Y + N,
    X =\= Y - N.
no_attack(X,Y,N,[Z|Zs]) :-
    X =\= Y + N,
    X =\= Y - N,
    N1 is N + 1,
    no_attack(X,Z,N1,Zs).
