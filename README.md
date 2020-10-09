# General Description:
This program was written in prolog, and it's compatible with the SWI-Prolog interpreter (Downloaded from https://www.swi-prolog.org). \
It implements a monitoring version of Allen's interval algebra. \
Let's assume we are looking at the relative order of 2 intervals. \
Each interval has a start time and end time:
1. Start new interval made by `start(some_interval, start_time)`.
2. End existing interval made by `end(some_interval, end_time)`.

Every new `start` or `end` fact should be checked against the specification. \
Let's say that if new asserted fact satisfied the specification, notification will send by the program and it stop.

### Types of Allen's interval algebra:
![Image of Inervals](https://www.researchgate.net/profile/Ioannis_Tsamardinos/publication/230561978/figure/fig2/AS:646067146223617@1531045819115/1-The-13-relations-between-intervals-in-Allens-algebra-Interval-A-is-always-either-at.png)


The program finds the interval and their relation which are satisfyied the specification (defined in 'specification.pl'), and then it sends a notification to the console. \
When the program is executed, it reads all the `start`/`end` fact from another separate file ('intervals.pl'), line by line, and it checks them againt the specification. \
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

The program implement 13 different intervals type as they described at Allen's interval algebra. \
This situation requires comparing times between those intervals. \
Due to the unification property built-in prolog, the comparing is a relatively simple operation that checks only the matches intervals with the limitations that apply to them. \

# Runing the program
## The program contains 3 file:
### 1. main.pl:
The main program which try to see if the specification is satisfyied.
### 2. specification.pl
Contains the specification should be satisfyied. \
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


All you need is to update limitations.pl and intervals.pl with yours and then execute main.pl.

# Output Example
```
[INFO]: y START at 14 TIME [14, _)
[INFO]: v START at 5 TIME [5, _)
[INFO]: a START at 10 TIME [10, _)
[INFO]: b START at 20 TIME [20, _)
[INFO]: a END at 19 TIME [10, 19]
[ERROR]: a BEFORE [b].
[ERROR]: [b] AFTER a.
[INFO]: c START at 28 TIME [28, _)
[ERROR]: [a] BEFORE c.
[ERROR]: c AFTER [a].
[INFO]: y END at 16 TIME [14, 16]
[ERROR]: y BEFORE [b].
[INFO]: c END at 40 TIME [28, 40]
[INFO]: v END at 10 TIME [5, 10]
[ERROR]: [b,y] AFTER v.
[INFO]: b END at 29 TIME [20, 29]
[INFO]: z START at 1 TIME [1, _)
[INFO]: d START at 20 TIME [20, _)
[ERROR]: d AFTER [a].
[INFO]: d END at 25 TIME [20, 25]
[INFO]: z END at 3 TIME [1, 3]
[ERROR]: [c] AFTER z.
[INFO]: f START at 10 TIME [10, _)
[INFO]: v START at 10 TIME [10, _)
[INFO]: v END at 25 TIME [5, 25]
[INFO]: f END at 25 TIME [10, 25]
[ERROR]: f EQUAL [v].
[ERROR]: [v] EQUAL f.
[INFO]: j START at 17 TIME [17, _)
[INFO]: n START at 15 TIME [15, _)
[INFO]: n END at 20 TIME [15, 20]
[INFO]: k START at 12 TIME [12, _)
[INFO]: m START at 10 TIME [10, _)
[INFO]: j END at 20 TIME [17, 20]
[INFO]: m END at 20 TIME [10, 20]
[ERROR]: [n] FINISHES m.
[ERROR]: m FINISHED_BY [n].
[INFO]: k END at 20 TIME [12, 20]
[ERROR]: [n,j] FINISHES k.
[INFO]: i START at 10 TIME [10, _)
[INFO]: i END at 45 TIME [10, 45]
[ERROR]: [a] STARTS i.
[ERROR]: i STARTED_BY [a].
[INFO]: l START at 10 TIME [10, _)
[INFO]: l END at 12.5 TIME [10, 12.5]
[ERROR]: l STARTS [i].
[ERROR]: [i] STARTED_BY l.
[INFO]: g START at 11 TIME [11, _)
[INFO]: g END at 14 TIME [11, 14]
[ERROR]: [f] CONTAINS g.
[INFO]: q START at 34 TIME [34, _)
[INFO]: r START at 40 TIME [40, _)
[INFO]: r END at 45 TIME [40, 45]
[INFO]: q END at 57 TIME [34, 57]
[ERROR]: [r] DURING q.
[INFO]: e START at 1 TIME [1, _)
[INFO]: e END at 14.1 TIME [1, 14.1]
[ERROR]: e CONTAINS [g].
[INFO]: w START at 57 TIME [57, _)
[ERROR]: [q] MEETS w.
[INFO]: u START at 58 TIME [58, _)
[INFO]: w END at 60 TIME [57, 60][INFO]: o START at 59 TIME [59, _)
[INFO]: o END at 62 TIME [59, 62]
[INFO]: u END at 61 TIME [58, 61]
[ERROR]: u OVERLAPS [o].
[ERROR]: u OVERLAPPED_BY [w].
[INFO]: o START at 59 TIME [59, _)
[INFO]: o END at 62 TIME [59, 62]
[INFO]: u END at 61 TIME [58, 61]
[ERROR]: u OVERLAPS [o].
[ERROR]: u OVERLAPPED_BY [w].
```

