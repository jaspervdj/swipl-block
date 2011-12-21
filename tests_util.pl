:- module(tests_util, [test_equal/3, test_fail/2, test_not_ground/2]).

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

test_fail(Name, Fail) :-
    write('Testing '),
    write(Name),
    write(': '),
    ( Fail ->
        writeln(' should have failed!')
    ;
        writeln('OK')
    ).

test_not_ground(Name, Got) :-
    write('Testing '),
    write(Name),
    write(': '),
    write(Got),
    ( ground(Got) ->
        write(' should not be instantiated!')
    ;
        writeln('OK')
    ).
