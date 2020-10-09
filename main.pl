/* Defines the entry point */
:- initialization(profile(time(main))).

/* Defines the dynamic facts which inserted during execution */ 
:- dynamic i_start/2,
		   i_end/2,
		   satisfied/3.

:- multifile(spesification/0).

:-op(500, xfy, [before, after, 
				contains, during, 
				overlaps, overlapped_by, 
				meets, met_by,
				starts, started_by,
				finishes, finished_by,
				equals]).
				
:-op(700, xfy, [and, or]).


and(A, B) :- 
	A,
	B.
	
or(A, B) :-
	A;
	B.
	
/* Main which load automatically during the execution */
main(_) :-
	read_spec_from_file('specification.pl', Spec, Spec_Atoms),
	read_intervals_from_file('intervals.pl', Stream),
	(
		execute_intervals(Stream, Spec, Spec_Atoms)
		->
			close(Stream)	
		;
			close(Stream)
	).
	

/* Read file which contains the specification
as Prolog term */
read_spec_from_file(File, Spec, Spec_Atoms) :-
    open(File, read, Stream),
    read_term(Stream, Spec, []),
    close(Stream),
	split(Spec, Spec_Atoms).

	
/* Read file which contains Prolog term that 
defines the interval start/end operations */
read_intervals_from_file(File, Stream) :-
    open(File, read, Stream).
	

/*A recursive function that simulates loop which runs overall 
interval start/end operations and try to see if any violation is
occurring during the execution */
execute_intervals(Stream, Spec, Spec_Atoms) :-
  read_term(Stream, Interval_Operation, []),
  (
  	Interval_Operation == end_of_file 
  	-> 
  		!,
  		ansi_format([fg(red)], '[FAIL]~n', []),
  		fail
  	;     
		Interval_Operation =.. [_|[H|_]],
		not(member(H, Spec_Atoms))
			->
			execute_intervals(Stream, Spec, Spec_Atoms)
			;
			call(Interval_Operation),
			Spec
			->
			!,
			ansi_format([fg(blue)], '[DONE]~n', [])
			;
			execute_intervals(Stream, Spec, Spec_Atoms)).
  
   
/* Call the necessary check if the interval is 'start' type */
start(Interval, Start_Time) :-
	(
		i_start(Interval, Time)
		->
		ansi_format([fg(yellow)], '[WARNING]: ~w already started at ~w TIME.~n', [Interval, Time])
		;
		(							
			assert(i_start(Interval, Start_Time)))).
			% ansi_format([fg(green)], 
			% '[INFO]: ~w START at ~w TIME [~w, _)~n', 
			% [Interval, Start_Time, Start_Time]))).

/* Call the necessary check if the interval is 'end' type */
end(Interval, End_Time) :-
	(
	i_start(Interval, Start_Time)					
	->
		(
			Start_Time > End_Time
			->
			ansi_format([fg(yellow)], '[WARNING]: ~w ended before it started.~n', [Interval]),
			fail
			;								
			assert(i_end(Interval, End_Time))
			% ansi_format([fg(green)], 
			% '[INFO]: ~a END at ~a TIME [~w, ~w]~n', 
			% [Interval, End_Time, Start_Time, End_Time])
		)
	;											% Else (If it didn't start)
	ansi_format([fg(yellow)], '[WARNING]: ~w did not started yet.~n', [Interval]),
	fail).
	
	
/* Case: A BEFORE B.
     A               B
|----------|	|----------|

*/
before(A, B) :-
	(
		satisfied(A, "BEFORE", B)
		->
		continue
		;
		(
			i_start(B, B_START_TIME),
			i_end(A, A_END_TIME),
			A_END_TIME < B_START_TIME,
			assert(satisfied(A, "BEFORE", B)),
			ansi_format([fg(blue)], '[SATISFY]: ~w BEFORE ~w.~n', [A, B]))).
			
	
/* Case: A AFTER B.
      B             A
|----------|	|----------|

*/	
after(A, B) :- 
	(
		satisfied(A, "AFTER", B)
		->
		continue
		;
		(
			i_start(A, A_START_TIME),
			i_end(B, B_END_TIME),
			A_START_TIME > B_END_TIME,
			assert(satisfied(A, "AFTER", B)),
			ansi_format([fg(blue)], '[SATISFY]: ~w AFTER ~w.~n', [A, B]))).
	
/* Case: A CONTAINS B.
      A              
|------------|	
   |-----|
      B
      
*/
contains(A, B) :-
	(
		satisfied(A, "CONTAINS", B)
		->
		continue
		;
		(
			i_end(A, A_END_TIME),
			i_end(B, B_END_TIME),
			i_start(A, A_START_TIME),
			i_start(B, B_START_TIME),
			A_START_TIME < B_START_TIME,
			A_END_TIME > B_END_TIME,
			assert(satisfied(A, "CONTAINS", B)),
			ansi_format([fg(blue)], '[SATISFY]: ~w CONTAINS ~w.~n', [A, B]))).
	
/* Case: A DURING B.
      B              
|------------|	
   |-----|
      A
      
*/
during(A, B) :-
	(
		satisfied(A, "DURING", B)
			->
			continue
			;
			(
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				A_START_TIME > B_START_TIME,
				A_END_TIME < B_END_TIME,
				assert(satisfied(A, "DURING", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w DURING ~w.~n', [A, B]))).
	
/* Case: A OVERLAPS B.
      A              
|------------|	
        |------------|
              B

*/
overlaps(A, B) :-
	(
		satisfied(A, "OVERLAPS", B)
			->
			continue
			;
			(
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				is_between(A_START_TIME, A_END_TIME, B_START_TIME),
				is_between(B_START_TIME, B_END_TIME, A_END_TIME),
				assert(satisfied(A, "OVERLAPS", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w OVERLAPS ~w.~n', [A, B]))).
	
/* Case: A OVERLAPPED BY B.
      B              
|------------|	
        |------------|
              A

*/
overlapped_by(A, B) :-
	(
		satisfied(A, "OVERLAPPED BY", B)
			->
			continue
			;
			(
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				is_between(A_START_TIME, A_END_TIME, B_END_TIME),
				is_between(B_START_TIME, B_END_TIME, A_START_TIME),
				assert(satisfied(A, "OVERLAPPED BY", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w OVERLAPPED BY ~w.~n', [A, B]))).
	
/* Case: A MEETS B.
      A              
|------------|	
             |------------|
                    B
					
*/
meets(A, B) :-
	(
		satisfied(A, "MEETS", B)
			->
			continue
			;
			(
				i_end(A, A_END_TIME),
				i_start(B, B_START_TIME),
				A_END_TIME =:= B_START_TIME,
				assert(satisfied(A, "MEETS", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w MEETS ~w.~n', [A, B]))).
	
/* Case: B MET BY A.
      B              
|------------|	
             |------------|
                    A

*/
met_by(A, B) :-
	(
		satisfied(A, "MET BY", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_end(B, B_END_TIME),
				A_START_TIME =:= B_END_TIME,
				assert(satisfied(A, "MET BY", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w MET BY ~w.~n', [A, B]))).
	
/* Case: A STARTS B.
    A              
|-------|	
|-------------|
       B

*/
starts(A, B) :-
	(
		satisfied(A, "STARTS", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				A_START_TIME =:= B_START_TIME,
				is_between(B_START_TIME, B_END_TIME, A_END_TIME),
				assert(satisfied(A, "STARTS", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w STARTS ~w.~n', [A, B]))).
	
/* Case: A STARTED BY B.
    B              
|-------|	
|-------------|
       A

*/
started_by(A, B) :-
	(
		satisfied(A, "STARTED BY", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				A_START_TIME =:= B_START_TIME,
				is_between(A_START_TIME, A_END_TIME, B_END_TIME),
				assert(satisfied(A, "STARTED BY", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w STARTED BY ~w.~n', [A, B]))).
	
/* Case: A FINISHES B.
          A              
      |-------|	
|-------------|
       B

*/
finishes(A, B) :-
	(
		satisfied(A, "FINISHES", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				A_END_TIME =:= B_END_TIME,
				is_between(B_START_TIME, B_END_TIME, A_START_TIME),
				assert(satisfied(A, "FINISHES", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w FINISHES ~w.~n', [A, B]))).
	
/* Case: A FINISHED BY B.
          B              
      |-------|	
|-------------|
       A

*/
finished_by(A, B) :-
	(
		satisfied(A, "FINISHED BY", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				A_END_TIME =:= B_END_TIME,
				is_between(A_START_TIME, A_END_TIME, B_START_TIME),
				assert(satisfied(A, "FINISHED BY", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w FINISHED BY ~w.~n', [A, B]))).
	
/* Case: A EQUALS B.
       A              
|-------------|	
|-------------|
       B

*/
equals(A, B) :-
	(
		satisfied(A, "EQUALS", B)
			->
			continue
			;
			(
				i_start(A, A_START_TIME),
				i_start(B, B_START_TIME),
				i_end(A, A_END_TIME),
				i_end(B, B_END_TIME),
				A_START_TIME =:= B_START_TIME,
				A_END_TIME =:= B_END_TIME,
				assert(satisfied(A, "EQUALS", B)),
				ansi_format([fg(blue)], '[SATISFY]: ~w EQUALS ~w.~n', [A, B]))).
				

/* Continue same as true, for readability */
continue :- true.

/* Check if goal target is between 2 values */
is_between(Start, End, Target) :-
    Start < Target,
    Target < End.

/* Extract atoms from the specification file.*/	
split(Expression, Atomics) :-
    Expression =.. [Functor| Args],
    phrase(split_atomics(Args, Functor), Atomics).

split_atomics([], Atomic) -->
    [Atomic].
split_atomics([Head| Tail], _) -->
    split_list([Head| Tail]).

split_list([]) -->
    [].
split_list([Head| Tail]) -->
    {Head =.. [Functor| Args]},
    split_atomics(Args, Functor),
    split_list(Tail).	
