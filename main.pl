/* Defines the entry point */
:- initialization(main).

/* Defines the dynamic facts which inserted during execution */ 
:- dynamic i_start/2.
:- dynamic i_end/2.

/* Check if list is empty */
is_empty([]).

/* Continue same as true, for readability */
continue :- true.


/* Check if goal target is between 2 values */
is_between(Start, End, Target) :-
    Start < Target,
    Target < End.

/* Print relative interval violations if found */
print_error(Type, Arg_1, Arg_2) :-
	ansi_format([fg(red)], '[ERROR]: ~w ~w ~w.~n', [Arg_1, Type, Arg_2]).
	

/* Main which load automatically during the execution */
main(_) :-
	consult('limitaions.pl'),						% Load the limititions facts into the program.
	read_intervals_from_file('intervals.pl').		% Load the intervals and prepare them for execution.
	
/* Read file which contains Prolog term that 
defines the interval start/end operations */
read_intervals_from_file(File) :-
    open(File, read, Stream),
    execute_intervals(Stream),
    close(Stream).

/*A recursive function that simulates loop which runs overall 
interval start/end operations and try to see if any violation is
occurring during the execution */
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

/* Call the necessary check if the interval is 'start' type */
start(Interval, Start_Time) :-
	(
		i_start(Interval, Time)				% If it's already started before, notification will send to console.
		->
		ansi_format([fg(yellow)], '[WARNING]: ~w already started at ~w TIME.~n', [Interval, Time])
		;
		(									% Else	
			assert(i_start(Interval, Start_Time)),
			ansi_format([fg(green)], 
			'[INFO]: ~w START at ~w TIME [~w, _)~n', 
			[Interval, Start_Time, Start_Time]),
			start_interval_chk(Interval, Start_Time))).

/* Call the necessary check if the interval is 'end' type */
end(Interval, End_Time) :-
	(
	i_start(Interval, Start_Time)				% If the interval started.	
	->
		(
			Start_Time > End_Time				% If start time is bigger then end time.
			->
			ansi_format([fg(yellow)], '[WARNING]: ~w ended before it started.~n', [Interval]),
			fail
			;									% Else (If start time is lower then end time)
			assert(i_end(Interval, End_Time)),
			ansi_format([fg(green)], 
			'[INFO]: ~a END at ~a TIME [~w, ~w]~n', 
			[Interval, End_Time, Start_Time, End_Time]),
			end_interval_chk(Interval, Start_Time, End_Time)
		)
	;											% Else (If it didn't start)
	ansi_format([fg(yellow)], '[WARNING]: ~w did not started yet.~n', [Interval]),
	fail).
	
/* List of the needed checks for starting intervals */
start_interval_chk(Interval, Start_Time) :-
    before_rel_chk('start', Interval, Start_Time),
    after_rel_chk('start', Interval, Start_Time),
    meets_rel_chk('start', Interval, Start_Time),
    met_by_rel_chk('start', Interval, Start_Time).

/* List of the needed checks for ending intervals */
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
   * If B starts - check if B starts after A.
   * If A ends - check if A ends before B starts. 
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
      A             B
|----------|	|----------|

Two options should be checked for this interval:
   * If B starts - check if B starts after A.
   * A ends - check if A ends before B.
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
   * If A ends - check if A starts before B and it ends after B ends.
   * If B ends - check if B ends before A ends and it starts after A starts.
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
   * If A ends - check if A starts before B and it ends after B ends.
   * If B ends - check if B ends before A ends and it starts after A starts.
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
   * A ends - check if A ends between B, and B started between A.
   * B ends - check if B starts between A, and A ended between B.
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
   * A ends - check if A ends between B, and B started between A.
   * B ends - check if B starts between A, and A ended between B.
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
   * A ends - check if A ends when B started.
   * B starts - check if B starts when A ended.
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
   * A ends - check if A ends when B starts.
   * B starts - check if B starts when A ended.
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
   * A ends - check if A starts when B starts, and A ends between B.
   * B ends - check if B starts when A starts, and A ends between B.
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
   * A ends - check if A starts when B starts, and A ends between B.
   * B ends - check if B starts when A starts, and A ends between B.
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
   * A ends - check if A ends when B ends, and A starts between B.
   * B ends - check if B ends when A ends, and A starts between B.
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
   * A ends - check if A ends when B ends, and A starts between B.
   * B ends - check if B ends when A ends, and A starts between B.
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
   * A end - check if A starts and end same as B.
   * B end - check if B starts and end same as A.
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










