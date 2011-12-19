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

    Out = (Head :- Body).

test(X) :-
    herp(X,X).

:- block herp(-,?), herp(?,-).
herp(2,3).
