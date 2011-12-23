:- module(tests_util, [test_equal/3]).

test_equal(Name, Expected, Got) :-
    write('Testing '),
    write(Name),
    write(': '),
    ( Expected = Got ->
        writeln('OK')
    ;
        write('expected '),
        write(Expected),
        write(', got: '),
        writeln(Got)
    ).
