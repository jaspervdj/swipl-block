:- use_module(tests_util).
:- use_module(compiler).

% Enable compilation using our compiler
term_expansion(I, O) :-
    compile(I, O).

:- use_module(examples).

test_all :-
    test_merge_1,
    test_merge_2,
    test_queens_1,
    test_psort_1.

test_merge_1 :-
    merge2([2], X, [1, 2]),
    test_equal(test_merge_3, [1], X).

test_merge_2 :-
    merge2(X, [2, 4], Y),
    X = [1, 3],
    test_equal(test_merge_4, [1, 2, 3, 4], Y).

test_queens_1 :-
    queens(10, Qs),
    !, % Only take the first solution into account
    test_equal(test_queens_1, [1, 3, 6, 8, 10, 5, 9, 2, 4, 7], Qs).

test_psort_1 :-
    psort([8, 1, 2, 9, 10, 5, 7, 4, 3, 6], L),
    test_equal(test_psort_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], L).
