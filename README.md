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


The program finds the interval and their relation, which are satisfied with the specification (defined in 'specification.pl'), and then it sends a notification to the console. \
When the program is executed, it reads all the `start`/`end` fact from another separate file ('intervals.pl'), line by line, and checks them against the specification. \
The program runs automatically right after it executes.


## About the monitoring algorithem:
### Pseudo code:
````
1. Extract atom from specification -> list_of_spec_atoms[] //Run once at the begin
2. for interval from intervals.pl:
     1. if interval in list_of_spec_atoms[]:
          1. assert interval
          2. if specification satisfyied:
               1. retun true
3. return false
````

The program implements 13 different intervals type as they described at Allen's interval algebra. \
This situation requires comparing times between those intervals. \
Due to the unification property built-in prolog, the comparing is a relatively simple operation that checks only the matches intervals with the limitations that apply to them. \

# Running the program
## The program contains 3 files:
### 1. main.pl:
The main program which tries to see if the specification is satisfied.
### 2. specification.pl
It contains the specification should be satisfied. \
The specification contains two types of boolean operands: \
   1. Allen's Interval Algebra boolean operands (before, after, contains, during, overlaps, 
      overlapped_by, meets, met_by, starts, started_by, finishes, finished_by, equals). \
   2. Logic operands (and, or, not). \
   
   The priority of Allen's operands [1] is higher than the logic operands [2]. \
   An expression can be written in various ways (prefix, infix, or any combination).  \
   For example, let's take the expression 'a before b and not(d after c)'; all the next are valid formats:
   1. 'a before b and not(d after c)'
   2. 'berfore(a, b) and not(d after c)'
   3. 'berfore(a, b) and not(after(d, c))'
   4. 'and(before(a, b), not(after(d, c)))'
### 3. intervals.pl:
Contains all interval operations (`starts` and `ends`) as prolog terms.
