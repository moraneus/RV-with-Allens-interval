:- initialization(main).

:- dynamic i_start/2.
:- dynamic i_end/2.

:- multifile(starts/2).
:- multifile(started_by/2).
:- multifile(meets/2).
:- multifile(met_by/2).
:- multifile(overlaps/2).
:- multifile(overlapp_by/2).
:- multifile(after/2).
:- multifile(before/2).
:- multifile(contains/2).
:- multifile(during/2).
:- multifile(finishes/2).
:- multifile(finished_by/2).
:- multifile(equals/2).


is_empty([]).

continue :- true.

is_between(Start, End, Target) :-
    Start < Target,
    Target < End.
	
print_error(Type, Arg_1, Arg_2) :-
	ansi_format([fg(red)], '[ERROR]: ~w ~w ~w.~n', [Arg_1, Type, Arg_2]).
	

main(_) :-
	consult('limitaions.pl'),
	read_intervals_from_file('intervals.pl').
	
read_intervals_from_file(File) :-
    open(File, read, Stream),
    execute_intervals(Stream),
    close(Stream).

execute_intervals(Stream) :-
  read_term(Stream, Interval_Operation, []),
  (   
        Interval_Operation \= end_of_file             
        ->  
            call(Interval_Operation),                                 
            execute_intervals(Stream)                
        ; 
        continue
  ).


start(Interval, Start_Time) :-
	(
		i_start(Interval, Time)
		->
		ansi_format([fg(blue)], '[WARNING]: ~w already started at ~w TIME.~n', [Interval, Time])
		;
		(
			assert(i_start(Interval, Start_Time)),
			ansi_format([fg(green)], 
			'[INFO]: ~w START at ~w TIME [~w, _)~n', 
			[Interval, Start_Time, Start_Time]),
			start_interval_chk(Interval, Start_Time))).

end(Interval, End_Time) :-
    i_start(Interval, Start_Time),
    assert(i_end(Interval, End_Time)),
    ansi_format([fg(green)], 
        '[INFO]: ~a END at ~a TIME [~w, ~w]~n', 
        [Interval, End_Time, Start_Time, End_Time]),
    end_interval_chk(Interval, Start_Time, End_Time).

start_interval_chk(Interval, Start_Time) :-
    before_rel_chk('start', Interval, Start_Time),
    after_rel_chk('start', Interval, Start_Time),
    meets_rel_chk('start', Interval, Start_Time),
    met_by_rel_chk('start', Interval, Start_Time).

end_interval_chk(Interval, Start_Time, End_Time) :-
    before_rel_chk('end', Interval, End_Time),
    after_rel_chk('end', Interval, End_Time),
    contains_rel_chk(Interval, Start_Time, End_Time),
    during_rel_chk(Interval, Start_Time, End_Time),
    overlaps_rel_chk(Interval, Start_Time, End_Time),
    over_lapped_by_rel_chk(Interval, Start_Time, End_Time),
    meets_rel_chk('end', Interval, End_Time),
    met_by_rel_chk('end', Interval, End_Time),
    starts_rel_chk(Interval, Start_Time, End_Time),
    started_by_rel_chk(Interval, Start_Time, End_Time),
    finishes_rel_chk(Interval, Start_Time, End_Time),
    finished_by_rel_chk(Interval, Start_Time, End_Time),
    equal_rel_chk(Interval, Start_Time, End_Time).


/* Case: A BEFORE B.
      A               B
|----------|	|----------|

Two options should be checked for this interval:
   * B start
   * A end
*/	
before_rel_chk(Type, Interval, Time) :-
    (   
        Type == 'start'
        -> 
            findall(X, (before(X, Interval), i_end(X, T), T < Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
				print_error('BEFORE', Bag, Interval)
            )
        ;
            findall(X, (before(Interval, X), i_start(X, T), T > Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
				print_error('BEFORE', Interval, Bag)
            )
    ).

/* Case: B AFTER A.
      A               B
|----------|	|----------|

Two options should be checked for this interval:
   * B start
   * A end
*/	
after_rel_chk(Type, Interval, Time) :-
    (   
        Type == 'start'
        -> 
            findall(X, (after(Interval, X), i_end(X, T), T < Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
				print_error('AFTER', Interval, Bag)
            )
        ;
            findall(X, (after(X, Interval), i_start(X, T), T > Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
				print_error('AFTER', Bag, Interval)
            )
    ).

/* Case: A CONTAINS B.
      A              
|------------|	
   |-----|
      B

Two options should be checked for this interval:
   * A end
   * B end
*/
contains_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
            contains(Interval, X), 
            i_start(X, T_S), 
            i_end(X, T_E), 
            T_S > Start_Time, 
            T_E < End_Time), 
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                    contains(X, Interval), 
                    i_start(X, T_S), 
                    i_end(X, T_E), 
                    T_S < Start_Time, 
                    T_E > End_Time), 
                Inner_Bag),
            (
                is_empty(Inner_Bag)
                ->
                    continue
                ;
					print_error('CONTAINS', Inner_Bag, Interval)
            )
        ;
		print_error('CONTAINS', Interval, Bag)
    ).



/* Case: B DURING A.
      A              
|------------|	
   |-----|
      B

Two options should be checked for this interval:
   * A end
   * B end
*/
during_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
            during(Interval, X), 
            i_start(X, T_S), 
            i_end(X, T_E), 
            T_S > Start_Time, 
            T_E < End_Time), 
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                    during(X, Interval), 
                    i_start(X, T_S), 
                    i_end(X, T_E), 
                    T_S > Start_Time, 
                    T_E < End_Time), 
                Inner_Bag),
            (
                is_empty(Inner_Bag)
                ->
                    continue
                ;
                print_error('DURING', Inner_Bag, Interval)
            )
        ;
        print_error('DURING', Interval, Bag)
    ).


/* Case: A OVERLAPS B.
      A              
|------------|	
        |------------|
              B

Two options should be checked for this interval:
   * A end
   * B end
*/
overlaps_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
            overlaps(Interval, X), 
            i_start(X, T_S), 
            i_end(X, T_E), 
            is_between(Start_Time, End_Time, T_S), 
            is_between(T_S, T_E, End_Time)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                    overlaps(X, Interval), 
                    i_start(X, T_S), 
                    i_end(X, T_E), 
                    is_between(T_S, T_E, Start_Time),
                    is_between(Start_Time, End_Time, T_E)),

                Inner_Bag),
            (
                is_empty(Inner_Bag)
                ->
                    continue
                ;
                print_error('OVERLAPS', Inner_Bag, Interval)
            )
        ;
        print_error('OVERLAPS', Interval, Bag)
    ).



/* Case: B OVERLAPPED BY A.
      A              
|------------|	
        |------------|
              B

Two options should be checked for this interval:
   * A end
   * B end
*/
over_lapped_by_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
            overlapp_by(Interval, X), 
            i_start(X, T_S), 
            i_end(X, T_E), 
            is_between(T_S, T_E, Start_Time), 
            is_between(Start_Time, End_Time, T_E)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                    overlapp_by(X, Interval), 
                    i_start(X, T_S), 
                    i_end(X, T_E), 
                    is_between(T_S, T_E, End_Time),
                    is_between(Start_Time, End_Time, T_S)),
                Inner_Bag),
            (
                is_empty(Inner_Bag)
                ->
                    continue
                ;
                print_error('OVERLAPPD BY', Inner_Bag, Interval)
            )
        ;
        print_error('OVERLAPPED_BY', Interval, Bag)
    ).

/* Case: A MEETS B.
      A              
|------------|	
             |------------|
                    B

Two options should be checked for this interval:
   * A end
   * B starts
*/
meets_rel_chk(Type, Interval, Time) :-
    (   
        Type == 'start'
        -> 
            findall(X, (meets(X, Interval), i_end(X, X_E), X_E == Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
                print_error('MEETS', Bag, Interval)
            )
        ;
            findall(X, (meets(Interval, X), i_start(X, X_S), X_S == Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
                print_error('MEETS', Interval, Bag)
            )
    ).

/* Case: B MET BY A.
      A              
|------------|	
             |------------|
                    B

Two options should be checked for this interval:
   * A end
   * B start
*/
met_by_rel_chk(Type, Interval, Time) :-
    (   
        Type == 'start'
        -> 
            findall(X, (met_by(Interval, X), i_end(X, X_E), X_E == Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
                print_error('MET BY', Bag, Interval)
            )
        ;
            findall(X, (met_by(X, Interval), i_start(X, X_S), X_S == Time), Bag),
            (
                is_empty(Bag)
                ->
                    continue
                ;
                print_error('MET BY', Interval, Bag)
            )
    ).

/* Case: A STARTS B.
    A              
|-------|	
|-------------|
       B

Two options should be checked for this interval:
   * A end
   * B end
*/
starts_rel_chk(Interval, Start_Time, End_Time) :-
   findall(X, 
        (
        starts(Interval, X), 
        i_start(X, T_S), 
        i_end(X, T_E), 
        Start_Time == T_S,
        is_between(T_S, T_E, End_Time)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                starts(X, Interval), 
                i_start(X, T_S), 
                i_end(X, T_E), 
                Start_Time == T_S,
                is_between(Start_Time, End_Time, T_E)),
            Inner_Bag),
        (
            is_empty(Inner_Bag)
            ->
                continue
            ;
            print_error('STARTS', Inner_Bag, Interval)
        )
    ;
    print_error('STARTS', Interval, Bag)
).

/* Case: B STARTED BY A.
    A              
|-------|	
|-------------|
       B

Two options should be checked for this interval:
   * A end
   * B end
*/
started_by_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
        started_by(Interval, X), 
        i_start(X, T_S), 
        i_end(X, T_E), 
        Start_Time == T_S,
        is_between(Start_Time, End_Time, T_E)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                started_by(X, Interval), 
                i_start(X, T_S), 
                i_end(X, T_E), 
                Start_Time == T_S,
                is_between(T_S, T_E, End_Time)),
            Inner_Bag),
        (
            is_empty(Inner_Bag)
            ->
                continue
            ;
            print_error('STARTED_BY', Inner_Bag, Interval)
        )
    ;
    print_error('STARTED_BY', Interval, Bag)
).

/* Case: A FINISHES B.
          A              
      |-------|	
|-------------|
       B

Two options should be checked for this interval:
   * A end
   * B end
*/
finishes_rel_chk(Interval, Start_Time, End_Time) :-
   findall(X, 
        (
        finishes(Interval, X), 
        i_start(X, T_S), 
        i_end(X, T_E), 
        End_Time == T_E,
        is_between(T_S, T_E, Start_Time)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                finishes(X, Interval), 
                i_start(X, T_S), 
                i_end(X, T_E), 
                End_Time == T_E,
                is_between(Start_Time, End_Time, T_S)),
            Inner_Bag),
        (
            is_empty(Inner_Bag)
            ->
                continue
            ;
            print_error('FINISHES', Inner_Bag, Interval)
        )
    ;
    print_error('FINISHES', Interval, Bag)
).

/* Case: B FINISHED BY A.
          A              
      |-------|	
|-------------|
       B

Two options should be checked for this interval:
   * A end
   * B end
*/
finished_by_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (
        finished_by(Interval, X), 
        i_start(X, T_S), 
        i_end(X, T_E), 
        End_Time == T_E,
        is_between(Start_Time, End_Time, T_S)),
        Bag),
    (
        is_empty(Bag)
        ->
            findall(X, 
                (
                finished_by(X, Interval), 
                i_start(X, T_S), 
                i_end(X, T_E), 
                End_Time == T_E,
                is_between(T_S, T_E, Start_Time)),
            Inner_Bag),
        (
            is_empty(Inner_Bag)
            ->
                continue
            ;
            print_error('FINISHED_BY', Inner_Bag, Interval)
        )
    ;
    print_error('FINISHED_BY', Interval, Bag)
).

/* Case: A EQUALS B.
       A              
|-------------|	
|-------------|
       B

Two options should be checked (both) for this interval:
   * A end
   * B end
*/
equal_rel_chk(Interval, Start_Time, End_Time) :-
    findall(X, 
        (equals(Interval, X), 
        i_start(X, T_S), 
        i_end(X, T_E), 
        Start_Time == T_S,
        End_Time == T_E),
        Bag),
		
		(is_empty(Bag)
        ->
			continue
		;
		print_error('EQUAL', Interval, Bag)
		),
	findall(X, 
		(equals(X, Interval), 
		i_start(X, T_S), 
		i_end(X, T_E), 
		Start_Time == T_S,
		End_Time == T_E),
		Inner_Bag),
		(is_empty(Inner_Bag)
		->
			continue
		;
		print_error('EQUAL', Inner_Bag, Interval)).
