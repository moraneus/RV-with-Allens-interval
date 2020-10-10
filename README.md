# General Description:
This program was written in prolog, and it's compatible with the SWI-Prolog interpreter (Downloaded from https://www.swi-prolog.org). \
It implements a monitoring version of Allen's interval algebra. \
Let's assume we are looking at the relative order of 2 intervals. \
Each interval has a start time and end time:
1. Start new interval made by `start(some_interval, start_time)`.
2. End existing interval made by `end(some_interval, end_time)`.

Every new `start` or `end` fact should be checked against the specification. \
If new asserted fact satisfied the specification, a notification would be sent by the program, and it stops.

### Types of Allen's interval algebra:
![Image of Inervals](https://www.researchgate.net/profile/Ioannis_Tsamardinos/publication/230561978/figure/fig2/AS:646067146223617@1531045819115/1-The-13-relations-between-intervals-in-Allens-algebra-Interval-A-is-always-either-at.png)


The program finds the intervals and their relations, which are satisfied the specification (defined in 'specification.pl'). When it happened, notification send to the console. \
When the program is executed, it reads all the `start`/`end` fact from another separate file ('intervals.pl'), line by line (term by term), and checks them against the specification. \
The program runs automatically right after it executes.

## About the monitoring algorithem:
### Pseudo code:
````
1. Extract atoms from specification -> list_of_spec_atoms[] //Run once at the begin
2. For each interval from intervals.pl:
     1. If interval in list_of_spec_atoms[]:
          1. Assert interval
          2. If specification has been satisfied due to the new assertion:
               1. retun true
3. return false
````

The program implements 13 different intervals types as they described at Allen's interval algebra. \
This situation requires comparing times between those intervals.

Due to the unification property built-in prolog, the comparing is a relatively simple operation that checks only the matches intervals. 

# Running the program
## The program contains 3 files:
### 1. main.pl:
The main program which tries to see if the specification is satisfied.
### 2. specification.pl
It contains the specification should be satisfied. 

The specification contains two types of boolean operands: 
   1. Allen's Interval Algebra boolean operands (before, after, contains, during, overlaps, overlapped_by, meets, met_by, starts, started_by, finishes, finished_by, equals).  
   2. Logic operands (and, or, not).
   
   The priority of Allen's operands [1] is higher than the logic operands [2].
   An expression can be written in various ways (prefix, infix, or any combination).
   
   For example, let's take the expression `a before b and not(d after c)`; all the next are valid formats:
   1. `a before b and not(d after c)`
   2. `before(a, b) and not(d after c)`
   3. `before(a, b) and not(after(d, c))`
   4. `and(before(a, b), not(after(d, c)))`
### 3. intervals.pl:
Contains all interval operations (`starts` and `ends`) as prolog terms.

## Running sample:
### Specification:
```
(((before(a, b) or d after c) and (f overlaps g)) and (c after b and (e during d and finished_by(i ,k)))) and (((d contains e and g overlapped_by f) and (g meets h and i met_by g)) and ((h equals i and m starts i) and (h started_by m and finishes(k, i)))).
 ```
### 1. Failure example:
```
[SATISFY]: a BEFORE b.
[SATISFY]: d AFTER c.
[SATISFY]: f OVERLAPS g.
[SATISFY]: c AFTER b.
[SATISFY]: e DURING d.
[SATISFY]: i FINISHED BY k.
[SATISFY]: d CONTAINS e.
[SATISFY]: g OVERLAPPED BY f.
[SATISFY]: g MEETS h.
[SATISFY]: i MET BY g.
[SATISFY]: h EQUALS i.
[SATISFY]: m STARTS i.
[SATISFY]: h STARTED BY m.
[FAIL]

% 68,027,179 inferences, 11.656 CPU in 11.766 seconds (99% CPU, 583
```
### 2. Success example:
```
[SATISFY]: a BEFORE b.
[SATISFY]: d AFTER c.
[SATISFY]: f OVERLAPS g.
[SATISFY]: c AFTER b.
[SATISFY]: e DURING d.
[SATISFY]: i FINISHED BY k.
[SATISFY]: d CONTAINS e.
[SATISFY]: g OVERLAPPED BY f.
[SATISFY]: g MEETS h.
[SATISFY]: i MET BY g.
[SATISFY]: h EQUALS i.
[SATISFY]: m STARTS i.
[SATISFY]: h STARTED BY m.
[SATISFY]: k FINISHES i.
[DONE]

% 27,160 inferences, 0.000 CPU in 0.074 seconds (0% CPU, Infinite Lips)
```

Both was checked against ~2,000,000 new facts.
