/* Version December 24, 2020 */

/* Your predicates may use the finite domain constraint solver. */

:- use_module(library(clpfd)).

/* Write your predicates in this presently empty space. Each predicate
 * foo that passes the corresponding test predicate test_foo is worth
 * three points to your course grade, up to the maximum of thirty points
 * for ten properly solved problems. */

%--------------------------------------------------------------------------
only_odd_digits(X) :- X =< 0, !, fail.

only_odd_digits(X) :-
	integer(X),
    numToList(X, [], Y),
	[A | B] = Y,
	is_odd(A, B).

is_odd(H, _):- H mod 2 =:= 0, !, fail.
is_odd(_, []):- !, true.
is_odd(_, T):- 
    [A | B] = T,
    is_odd(A, B).

numToList(N, L, X):- N < 10,!, append([N], L, X).
numToList(N, L, X):- 
	Digit is N mod 10,
    N2 is N // 10,
    append([Digit], L, L2),
    numToList(N2, L2, X).

%--------------------------------------------------------------------------

domino_cycle(C):-
    is_list(C),
	[F | _] = C,
	last(C, L),
    (F1, _) = F,
    (_, L2) = L,
    between(1, 6, F1),
    between(1, 6, L2),
	F1 =:= L2,
    is_cyclic(C).

is_pair(A,B):-
    (_,A2) = A,
    (B1,_) = B,
    between(1, 6, A2),
    between(1, 6, B1),
    A2 = B1.

is_cyclic(L):- length(L,1), true.

is_cyclic(L):-
    [A, B | _] = L,
    (_,A2) = A,
    (B1,_) = B,
    between(1, 6, A2),
    between(1, 6, B1),
    A2 = B1,
    [_ | T] = L,
    is_cyclic(T).


%--------------------------------------------------------------------------

and(L,R) :- L,R.
or(L,R) :- L;R.

first_missing_positive(Items, Result):-
    clean_list(Items, [], FinalList),
    sort(FinalList, SortedList),
    find_missing(SortedList, 1, Result).

clean_list([], Current, Current):- !.
clean_list(Items, Current, Final):-
    %write(Items),nl,
    [H | T] = Items,
    (and(integer(H), H>0) ->  append([H], Current, NewCurrent), clean_list(T, NewCurrent, Final);
    clean_list(T, Current, Final)).

find_missing([], N, N):- !.
find_missing(L, N, R):-
    [H | T] = L,
    (H =:= N -> N1 is N+1, find_missing(T, N1, R) ; !, N=R).

%--------------------------------------------------------------------------

riffle(L, R, Result, left):-
    not(is_list(Result)),
	length(L, X),
    length(R, Y), 
    X > 0, Y > 0,
    X =:= Y,
    %write("Both Equal"),
    riffle(L, R, [], Result, left).

riffle(L, R, Result, right):-
    not(is_list(Result)),
	length(L, X),
    length(R, Y), 
    X > 0, Y > 0,
    X =:= Y,
    %write("Both Equal"),
    riffle(L, R, [], Result, right).

riffle(L, R, Result, Mode):-
    %write("In Other"),    
    is_list(Result),
    !,
    rifBuild(L, R, Result, Mode).

riffle(L, R, Collector, Result, left):-
    %write(left),
    [HL | TL] = L,
    [HR | TR] = R,
	riffle(TL, TR, [HR, HL | Collector], Result, left), !.

riffle(L, R,Collector, Result, right):-
    %write(right),
    [HL | TL] = L,
    [HR | TR] = R,
	riffle(TL, TR, [HL, HR | Collector], Result, right), !.

riffle([], [], Collector, Result, _):- !, reverse(Collector, Result), !.

rifBuild(L, R, List, M):-
    rifBuildLeft([], [], List, M, L, R).

rifBuild(L, R, List, M):-
    rifBuildRight([], [], List, M, L, R).
    
rifBuildLeft(LL, RL, List, M, L, R):-
    [F, S | T] = List,
	[F | LL] = NL,
    [S | RL] = NR,
    %M = left,
    rifBuildLeft(NL, NR, T, M, L, R).

rifBuildLeft(LL, RL, [], M, L, R):- reverse(LL,L), reverse(RL,R), M=left.

rifBuildRight(LL, RL, List, M, L, R):-
    [F, S | T] = List,
	[S | LL] = NL,
    [F | RL] = NR,
    rifBuildRight(NL, NR, T, M, L, R),!.

rifBuildRight(LL, RL, [], M, L, R):- reverse(LL,L), reverse(RL,R), M=right.


%--------------------------------------------------------------------------

group_and_skip(N, Out, In, Leftovers):-
    group_and_skip(N, Out, In, Leftovers, []).

group_and_skip(0, _, _, Leftovers, Collector):- Leftovers = Collector, !.
group_and_skip(N, Out, In, Leftovers, Collector):-
    Groups is N//Out,
    Remainder is N mod Out,
    [Remainder | Collector] = NC,
    NewN is In * Groups,
    group_and_skip(NewN, Out, In, Leftovers, NC).

%--------------------------------------------------------------------------

taxi_zum_zum(Moves, Pos):-
   	string_chars(Moves, L),
    [H | T] = L,
	travel(H, T, (0,1), (0,0), Pos).

travel('f', [], Facing, CurrentPosition, Pos):-
    add_move(CurrentPosition, Facing, NextPosition),
	Pos = NextPosition, !.
travel(_, [], _, CurrentPosition, Pos):- Pos = CurrentPosition, !.

travel('f', T, Facing, CurrentPosition, Pos):-
    add_move(CurrentPosition, Facing, NextPosition),
    [H | T2] = T,
    travel(H, T2, Facing, NextPosition, Pos).

travel('r', T, (0,1), CurrentPos, Pos):- Facing = (1,0), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).
travel('l', T, (0,1), CurrentPos, Pos):- Facing = (-1,0), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).

travel('r', T, (1,0), CurrentPos, Pos):- Facing = (0,-1), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).
travel('l', T, (1,0), CurrentPos, Pos):- Facing = (0,1), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).

travel('r', T, (0,-1), CurrentPos, Pos):- Facing = (-1,0), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).
travel('l', T, (0,-1), CurrentPos, Pos):- Facing = (1,0), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).

travel('r', T, (-1,0), CurrentPos, Pos):- Facing = (0,1), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).
travel('l', T, (-1,0), CurrentPos, Pos):- Facing = (0,-1), [H | T2] = T, travel(H, T2, Facing, CurrentPos, Pos).

add_move(X, Y, Z):-
    (X1, X2) = X,
    (Y1, Y2) = Y,
    Z1 is X1 + Y1,
    Z2 is X2 + Y2,
    Z = (Z1, Z2).

%--------------------------------------------------------------------------

extract_increasing(Digits, Nums):-
    string_chars(Digits, DigitList), 
    [F, S| T] = DigitList, 
    atom_number(F, NF),
    atom_number(S, NS),
	%extract_increasing_helper(DigitList, [], Nums).
	increasing_helper(NF, NS, T, [NF], Nums).

increasing_helper(First, Second, [], Collector, Nums):-
    ( First < Second ->  
    append([Second], Collector, NewCollector), reverse(NewCollector, Nums) ; reverse(Collector, Nums) ),
    !.

increasing_helper(First, Second, DigitList, Collector, Nums):-
    First < Second,
    append([Second], Collector, NewCollector),
    %write(NewCollector), nl,	
    %write("First Smaller than Second"),
    [Next | T] = DigitList,
    atom_number(Next, NextInt),
    increasing_helper(Second, NextInt, T, NewCollector, Nums), !.
    %extract_increasing_helper(T, NewC, Nums).

increasing_helper(First, Second, DigitList, Collector, Nums):-
    %First < Second,
    [Next | T] = DigitList,
	string_concat(Second, Next, NewNext),
    atom_number(NewNext, NextInt),
    %append([Second], Collector, NewCollector),
    %write(NewNext), nl,
    %write("Was not larger"),
    increasing_helper(First, NextInt, T, Collector, Nums).

%--------------------------------------------------------------------------

%slice(List, 0, Collector, Result, LeftOver):-	reverse(Collector, Result), LeftOver = List,!.
slice(List, 0, Collector, Result, LeftOver):-	Collector = Result, LeftOver = List,!.

slice(List, To, Collector, Result, LeftOver):-
    [H | T] = List,
    NextTo is To-1,
    append([H], Collector, NextCollector),
    slice(T, NextTo, NextCollector, Result, LeftOver).

pancake_scramble(Text, Result):-
    string_chars(Text, TextList),
    length(TextList, Len),
    Len < 2,
    Result = Text, !.

pancake_scramble(Text, Result):-
    string_chars(Text, TextList),
    length(TextList, Len),
    Len >= 2,
    scramble_helper(2, Len,TextList, Result).

scramble_helper(Point, End, List, Result):-
	Point =:= End,
    reverse(List, TempResult),
    string_chars(Result, TempResult),
    !.

scramble_helper(Point, End, List, Result):-
    slice(List, Point, [], CutPart, LeftPart),
	append(CutPart, LeftPart, Scrambled),
    NewPoint = Point +1,
    scramble_helper(NewPoint, End, Scrambled, Result).

%--------------------------------------------------------------------------

tukeys_ninther(Items, M):-
    ninther_helper(Items, M), !.

ninther_helper(Items, M):-
    length(Items, 1),
    [M | _] = Items.

ninther_helper(Items, M):-
	%middle(Items, M).
    [A, B, C | T] = Items,
	%Call Middle thing,
	middle(A,B,C,T, Result),
	ninther_helper(Result, M).

%A,B,C
middle(A, B, C, List, Result):-
	A < B,
    B < C,
    append(List, [B], Result).

%A,C,B
middle(A, B, C, List, Result):-
	A < C,
    C < B,
    append(List, [C], Result).

%B,A,C
middle(A, B, C, List, Result):-
	B < A,
    A < C,
    append(List, [A], Result).

%B,C,A
middle(A, B, C, List, Result):-
	B < C,
    C < A,
    append(List, [C], Result).

%C,A,B
middle(A, B, C, List, Result):-
	C < A,
    A < B,
    append(List, [A], Result).

%C,B,A
middle(A, B, C, List, Result):-
	C < B,
    B < A,
    append(List, [B], Result).

%--------------------------------------------------------------------------

count_dominators([], Result):-
    Result is 0,!.

count_dominators(List, Result):-
    length(List, 1),
    Result is 1, !.

count_dominators(Items, Result):-
    reverse(Items, RevList),
    dominator_helper(RevList, 0, 0, Result).

dominator_helper([], _, Count, Result):-
    Result is Count, !.

dominator_helper(List, CurrentMax, Count, Result):-
	[H | T] = List,
    ( H > CurrentMax ->  
    NewCount = Count + 1, dominator_helper(T, H, NewCount, Result) ;
    dominator_helper(T, CurrentMax, Count, Result)).

%--------------------------------------------------------------------------


/* Complete the predicate all/0 to call the test predicates for the
 * Prolog predicates that you have defined in the above space. */

all :-
	/* Fill in the calls to test predicates here. */
    test_only_odd_digits,
    test_domino_cycle,
    test_first_missing_positive,
    test_riffle,
    test_group_and_skip,
    test_taxi_zum_zum,
    test_extract_increasing,
	test_pancake_scramble,
    test_tukeys_ninther,
    test_count_dominators,
	true.

/* DO NOT MODIFY ANYTHING BELOW THIS LINE!!!! */

/* General tester logic, same for all predicates. */

run_tests(Tests, I, F) :-
    run_tests(Tests, 0, 0, I, F).

run_tests([], I, F, I, F) :- !.

run_tests([T|Tests], CI, CF, I, F) :-
    statistics(inferences, I1),
    /* Trick to execute query once without binding its variables. */
    not(not(call(T))),
    !,
    statistics(inferences, I2),
    II is I2 - I1,
    C is CI + II,
    run_tests(Tests, C, CF, I, F).

run_tests([T|Tests], CI, CF, I, F) :-
    write("FAILED: "), write(T), nl,
    CFF is CF + 1,
    run_tests(Tests, CI, CFF, I, F).

/* Helper predicates to write some mass tests. */

total(L, S) :-
    total(L, S, 0).
total([], S, S).
total([H|T], S, SS) :-
    plus(SS, H, SSS),
    total(T, S, SSS).

/* The element X appears in list L exactly C times. */

count(X, L, C) :-
    count(X, L, C, 0).

count(_, [], C, C) :- !.

count(X, [X|T], C, Curr) :-
    !,
    plus(Curr, 1, C2),
    count(X, T, C, C2).

count(X, [_|T], C, Curr) :-
    count(X, T, C, Curr).

/* Run the tests for the given predicate and print the report. */

test_harness(Pred, Tests) :-
    write(Pred), write(": "),
    run_tests(Tests, I, F),
    write("Executed "), write(I), write(" total inferences. "),
    write("Failed "), write(F), write(" test cases."), nl.

/* Test predicates for the individual predicates. */

test_duplicate_digit_bonus :-
	test_harness("duplicate_digit_bonus", [
	duplicate_digit_bonus(333444555666, 50),
	duplicate_digit_bonus(1223334444555556666667777777, 211111),
	duplicate_digit_bonus(9999999999088888888888, 2100000000),
	duplicate_digit_bonus(2111111747111117777700, 12002),
	(X is 2^50, duplicate_digit_bonus(X, 11)),
	(X is 444^555, duplicate_digit_bonus(X, 216))            
	]).             
              
test_three_summers :-
	test_harness("three_summers", [
	(findall(X, between(1, 20, X), L),
	findall((A, B, C), three_summers(L, 40, A, B, C), LL), length(LL, 33)),
	(findall(Z, (between(1, 20, X), Z is X*X), L),
	findall(N, (between(100, 200, N), three_summers(L, N, A, B, C)), LL),
	sort(LL, Ls), length(Ls, 66))                
	]).
                        
test_tukeys_ninther :-
    test_harness("tukeys_ninther", [
    tukeys_ninther([55, 99, 131, 42, 88, 11, 17, 16, 104, 2,
                     8, 7, 0, 1, 69, 8, 93, 9, 12, 11, 16, 1, 77, 90, 15, 4, 123], 15),
    (L = [4, 42, 987, 3123, 83120, 555321, 9815212, 34343434, 982264982],
        findall(M, (permutation(L, LL), tukeys_ninther(LL, M)), TN),
        count(987, TN, 0),
    	count(3123, TN, 77760),
    	count(83120, TN, 207360),
        count(555321, TN, 77760),
        count(9815212, TN, 0))
    ]).          

test_give_change :-
	test_harness("give_change", [
	give_change(100, [55, 10, 1], [55, 10, 10, 10, 10, 1, 1, 1, 1, 1]),
	\+ give_change(34, [20, 9, 6], _),
	(findall(Y, (between(1, 1000, N), give_change(N, [42, 17, 5, 1], Y)), L), flatten(L, LL), total(LL, 500500))
	]).         

test_extract_increasing :-
	test_harness("extract_increasing", [
	extract_increasing("0123456789", [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
	extract_increasing("77777777777777777777777",
	              	   [7, 77, 777, 7777, 77777, 777777]),
	extract_increasing("3141592653589793238462643383279502884",
                       [3, 14, 15, 92, 653, 5897, 9323, 84626, 433832, 795028]),
	extract_increasing("2718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260",
                       [2, 7, 18, 28, 182, 845, 904, 5235, 36028, 74713, 526624, 977572,
                         4709369, 9959574, 96696762, 772407663, 3535475945, 7138217852,
                         51664274274, 66391932003, 599218174135, 966290435729])
	]).          

test_pancake_scramble :-
	test_harness("pancake_scramble", [
	pancake_scramble("", ""),
	pancake_scramble("q", "q"),
	pancake_scramble("ab", "ba"),
	pancake_scramble("artificial intelligence", "englen acftariiilitliec"),
	pancake_scramble("pancakes with jam", "mjhi eanpackswt a"),
	pancake_scramble("Prolog Schmolog", "glmc ooPrlgShoo")
	]).          
              
test_domino_cycle :-
	test_harness("domino_cycle", [
	domino_cycle([(3, 5), (5, 2), (2, 3)]),
	domino_cycle([(4, 4)]),
	\+ domino_cycle([(4, 1), (1, 7), (7, 2)]),
	(domino_cycle([(A, 3), (3, 1), (1, A), (1, 1), (1, A)]), A = 1),
	\+ domino_cycle([(B, 5), (5, 2), (B, 3), (3, 4)]),
	findall(C, (length(C, 5), domino_cycle(C)), L), length(L, 7776)            
	]).          

test_taxi_zum_zum :-
	test_harness("taxi_zum_zum", [
	taxi_zum_zum("f", (0, 1)),
	taxi_zum_zum("fflllfrlflrfrlrrl", (3, 2)),
	taxi_zum_zum("rrrrrrrrrllllrrrrrrrrrrrr", (0, 0)),
	taxi_zum_zum("frfflffllfffr", (2, 0)),
	taxi_zum_zum("lffrfrrfflfllrfflf", (-2, 1))                              
	]).

test_group_and_skip :-
	test_harness("group_and_skip", [
	group_and_skip(99, 5, 3, [3, 4, 3, 3, 2, 4]),
	group_and_skip(123456789, 1000, 1, [123, 456, 789]),
	group_and_skip(255, 2, 1, [1, 1, 1, 1, 1, 1, 1, 1]),
	group_and_skip(10^9, 13, 3, [3, 8, 5, 10, 8, 6, 11, 8, 9, 7, 0, 2, 1, 12])
	]).              
              
test_bulgarian_solitaire :-
	test_harness("bulgarian_solitaire", [
	bulgarian_solitaire([1, 1, 1], 2, 2),                                    
	bulgarian_solitaire([5, 4, 1], 4, 10),                                    
	bulgarian_solitaire([6, 4, 2, 1, 3, 5], 6, 0),
	bulgarian_solitaire([8, 3, 3, 1], 5, 9),
	bulgarian_solitaire([10, 10, 10, 10, 10, 5], 10, 74),
	bulgarian_solitaire([3000, 2050], 100, 7325)          
	]). 

test_only_odd_digits :-
	test_harness("only_odd_digits", [
	only_odd_digits(1),
	only_odd_digits(999919999199991),
	only_odd_digits(135797531),
	\+ only_odd_digits(1354797531),
	\+ only_odd_digits(7717936191),
	\+ only_odd_digits(0),
	(findall(N, (between(1, 1000, N), only_odd_digits(N)), L), length(L, 155)),
	(findall(N, (between(1, 100000, N), only_odd_digits(N)), L), length(L, 3905))
	]).

test_josephus :-
	test_harness("josephus", [
	josephus([joe, moe, bob, rob, josephus], 2, bob),
	josephus([joe, moe, bob, rob, josephus], 99, josephus),
	(findall(N, between(1, 30, N), L), josephus(L, 4, 6)),
	(findall(N, between(1, 1000, N), L), josephus(L, 13, 396)),
	(findall(N, between(1, 10000, N), L), josephus(L, 77, 7373)),
	findall(N, (permutation([1, 2, 3, 4, 5], P), josephus(P, 3, N)), L), count(2, L, 24)
	]).

test_first_missing_positive :-
	test_harness("first_missing_positive", [ 
	first_missing_positive([99999, 123, 1, 24, 5, 9999999, 222, 3, 4, 7777777, 2], 6),
	(findall(X, first_missing_positive([99, 4, 1, 3, 7, 2], X), L), L = [5]),
	(findall(Y, between(1, 1000, Y), LLL), reverse(LLL, LL), findall(X, first_missing_positive(LL, X), L), L = [1001]),
	first_missing_positive([-1, -2, -3, -4, -4, 0, 1], 2),
	first_missing_positive([1, 2, [3, 4]], 3)
	]).

test_riffle :-
	test_harness("riffle", [
	riffle([1,2,3,4], [5,6,7,8], [1,5,2,6,3,7,4,8], left),
	riffle([1,2,3,4], [5,6,7,8], [5,1,6,2,7,3,8,4], right),
	(riffle([42, bob, 99], [55, jack, tom], [55|_], M), M = right),
	\+ riffle([11, 12, 13, 14], [1, 2, 3, 4, 5, 6], L, M),
	(findall(M, riffle([11, 12, 13, 14], [1, 2, 3, 4], L, M), Z), length(Z, Z2), Z2 = 2)
	]).

test_sz :-
	test_harness("sz", [
	sz(272, 77777777777777770000),
	(findall(S, sz(555, S), L), L = [7770]),
	(findall(S, sz(2727, S), L), L = [777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777]),
	sz(1, 7),
	sz(2^20, 700000000000000000000),
	sz(129, 777777777777777777777)
	]).

test_crag :-
	test_harness("crag", [
	crag(5, 4, 5, 10),
	(findall(S, crag(3, 4, X, S), L), sort(L, [4, 5, 6, 8, 26])),
	(findall((A, B, C), crag(A, B, C, 26), L), length(L, 12)),
	\+ crag(6, 6, 6, 18),
	(findall((A, B, C), crag(A, B, C, 25), X), length(X, 6))          
	]).          

test_count_dominators :-
	test_harness("count_dominators", [
	count_dominators([], 0),
	count_dominators([33, 22, 11, 64, -2, 5], 2),
	(findall(X, between(1, 1000, X), L), reverse(L, LL), findall(D, count_dominators(LL, D), LD), LD = [1000]),
	(findall(L, (between(1, 5, X), between(1, 5, Y), L = [X, Y], count_dominators(L, 2)), V), length(V, 10)),
	count_dominators([[1,2,3]], 1)
	]).

test_running_median :-
	test_harness("running_median", [
	running_median([99, 42, 17, 55, -4, 18, 77], [99, 42, 42, 42, 17, 18, 18]),
	(running_median([42, 42, 42, 42, 42, 42, 42], L), L = [42, 42, 42, 42, 42, 42, 42]),
	running_median([1,2,3,4,5,6], [1,2,2,3,4,5]),
	running_median([1, 1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1, 1]),
	running_median([A, B], [A, B])
	]).

test_safe_squares_rooks :-
	test_harness("safe_squares_rooks", [
	safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, 4),
	(findall((X, X), between(1, 50, X), L), safe_squares_rooks(L, 50, S), S = 0),
	safe_squares_rooks([(4,3), (2,2), (1,2)], 10, 56),
	safe_squares_rooks([(1, 1), (3, 1), (3, 2)], 5, 9),
	safe_squares_rooks([(1, 1), (2, 2), (3, 4)], 1000, 994009)
	]).          

test_trick_winner :-
	test_harness("trick_winner", [
	trick_winner([(five, spades), (queen, diamonds), (ace, spades), (ten, spades)], (ace, spades)),
	(findall(X, trick_winner([(six, spades), (two, hearts), (X, spades), (nine, clubs)], (six, spades)), L), length(L, 4)),
	(findall(X, trick_winner([(five, diamonds), X, (ten, hearts), (ten, diamonds)], X), L), length(L, 4)),
	\+ trick_winner([(seven, spades), (two, hearts), (six, spades), (nine, clubs)], (two, hearts)),
	(findall(X, trick_winner([(ace, S), (two, S), (six, S), (king, S)], X), Z), length(Z, 4))
	]).

test_sum_of_two_squares :-
	test_harness("sum_of_two_squares", [
	\+ sum_of_two_squares(11, _, _),
	sum_of_two_squares(50, 7, 1),
	(X is 123^2 + 456^2, sum_of_two_squares(X, 456, 123)),
	(X is 555^2 + 666^2, sum_of_two_squares(X, 810, 309)),
	(findall(N, (between(1, 2000, N), sum_of_two_squares(N, _, _)), L), length(L, 591))
	]).

test_hitting_integer_powers :-
	test_harness("hitting_integer_powers", [
	hitting_integer_powers(2, 7, 100, 73, 26),
	hitting_integer_powers(3, 6, 100, 137, 84),
	hitting_integer_powers(4, 5, 1000, 916, 789),
	hitting_integer_powers(10, 11, 1000, 1107, 1063),
	hitting_integer_powers(42, 51, 10000, 29546, 28087)
	]).

test_sum_of_distinct_cubes :-
	test_harness("sum_of_distinct_cubes", [
	sum_of_distinct_cubes(777777777, [919, 117, 29, 6]),
	(sum_of_distinct_cubes(123456789, L), L = [497, 88, 22, 8, 7, 6, 5]),
	(X is 10^16+1, sum_of_distinct_cubes(X, L), L = [215443, 4027, 139, 12, 10, 8, 5, 3]),
	sum_of_distinct_cubes(1, [1])
	]).

test_fibonacci_sum :-
	test_harness("fibonacci_sum", [
	fibonacci_sum(10, [8, 2]),
	fibonacci_sum(42, [34, 8]),
	fibonacci_sum(100, [89, 8, 3]),
	fibonacci_sum(12345, [10946, 987, 377, 34, 1]),
	fibonacci_sum(665544332211, [591286729879, 53316291173, 20365011074, 433494437, 102334155, 39088169, 1346269, 28657, 6765, 1597, 34, 2]),
	(X is 10^100, fibonacci_sum(X, L), length(L, 137)),
	(X is 10^1000, fibonacci_sum(X, L), length(L, 1316)),
	fibonacci_sum(1, [1]),
	fibonacci_sum(58001746501815487425285,
	              [43566776258854844738105, 10284720757613717413913, 3928413764606871165730, 218922995834555169026, 2880067194370816120, 23416728348467685, 8944394323791464, 190392490709135, 72723460248141, 27777890035288, 4052739537881, 1548008755920, 86267571272, 4807526976, 1836311903, 701408733, 165580141, 63245986, 24157817, 514229, 196418, 46368, 10946, 4181, 1597, 233, 55, 21, 1])
	]).