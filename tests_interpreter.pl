:- use_module(examples).
:- use_module(interpreter).
:- use_module(tests_util).

test_all :-
    test_merge_1,
    test_merge_2,
    test_queens_1,
    test_psort_1.

test_merge_1 :-
    eval(merge2([2], X, [1, 2])),
    test_equal(test_merge_1, [1], X).

test_merge_2 :-
    eval((merge2(X, [2, 4], Y), X = [1, 3])),
    test_equal(test_merge_2, [1, 2, 3, 4], Y).

test_queens_1 :-
    eval(queens(10, Qs)),
    !, % Only take the first solution into account
    test_equal(test_queens_1, [1, 3, 6, 8, 10, 5, 9, 2, 4, 7], Qs).

test_psort_1 :-
    eval(psort([8, 1, 2, 9, 10, 5, 7, 4, 3, 6], L)),
    test_equal(test_psort_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], L).
