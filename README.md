# General Description:
This program was written in prolog, and it's compatible with the SWI-Prolog interpreter (Downloaded from https://www.swi-prolog.org).
It implements a monitoring version of Allen's interval algebra.
Let's assume we are looking at the relative order of 2 intervals.
Each interval has a start time and end time.
Start new interval made by start(some_interval, start_time).
End existing interval made by enf(some_interval, end_time).
Every new start or end should be checked against the facts.
Let's say that if the new insertion satisfied one of those facts, 
that means it should be `FAILD`.

### Types of Allen's interval algebra:
![Image of Inervals](https://www.researchgate.net/profile/Ioannis_Tsamardinos/publication/230561978/figure/fig2/AS:646067146223617@1531045819115/1-The-13-relations-between-intervals-in-Allens-algebra-Interval-A-is-always-either-at.png)
The program finds the limitations which are defined in a separate file ('limitations.pl'), and then it sends a notification to the console.
When the program is executed, it reads all the start/end fact from another separate file ('intervals.pl'), line by line, and it checks if any of the limitations was apply due to the interval operation.
The program runs automatically right after it executes.


### About the monitor algorithem:
We are try to handle with finding limitations of 13 different intervals within every interval operation.
This situation requires comparing times between those intervals.
Due to the unification of prolog, the comparing is a simple operation who checks only the matches intervals with the limitions whom apply on them.
Yet, it steel need to run throught all types of intervals checks.
Those checks allow to locate violation even if the intervals were called out of order (except that end should be after start).
1. ##### Check if interval A before B 
```      A               B
|----------|	|----------|

Two options should be checked for this interval:
   * B start
   * A end
```
2. ##### Check if interval A after B
```
      A               B
|----------|	|----------|

Two options should be checked for this interval:
   * B start
   * A end
```
5. ##### Check if interval A meets interval B 
```prolog
a_meets_b(Interval_A_Name, Interval_B_Name).
```
6. ##### Check if interval A met by interval B 
```prolog
a_met_by_b(Interval_A_Name, Interval_B_Name).
```
7. ##### Check if interval A overlaps interval B 
```prolog
a_overlaps_b(Interval_A_Name, Interval_B_Name).
```
8. ##### Check if interval A overlapped by interval B 
```prolog
a_overlapped_by_b(Interval_A_Name, Interval_B_Name).
```
9. ##### Check if interval A starts interval B 
```prolog
a_starts_b(Interval_A_Name, Interval_B_Name).
```
10. ##### Check if interval A started by interval B 
```prolog
a_started_by_b(Interval_A_Name, Interval_B_Name).
```
11. ##### Check if interval A finishes interval B 
```prolog
a_finishes_b(Interval_A_Name, Interval_B_Name).
```
12. ##### Check if interval A finished by interval B 
```prolog
a_finished_by_b(Interval_A_Name, Interval_B_Name).
```
13. ##### Check if interval A equal to interval B 
```prolog
a_equal_b(Interval_A_Name, Interval_B_Name).
```

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

