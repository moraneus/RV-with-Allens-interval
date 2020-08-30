# Details:
It's just a basic program who gets as input, during execution, new facts of interval starting or ending.

I used SWI-Prolog interpreter (Downloaded from: https://www.swi-prolog.org).

### I also, implemnted those interval checks:
![Image of Inervals](https://www.researchgate.net/profile/Ioannis_Tsamardinos/publication/230561978/figure/fig2/AS:646067146223617@1531045819115/1-The-13-relations-between-intervals-in-Allens-algebra-Interval-A-is-always-either-at.png)

### Basic commands:
1. ##### Start new interval
(When new interval starts, it takes the current timestamp and use it as argument related to the `start` fact)
```prolog
start_interval(Interval_Name).
```
2. ##### Ending running interval
```prolog
end_interval(Interval_Name).
```
3. ##### Check if interval A before B 
```prolog
a_before_b(Interval_A_Name, Interval_B_Name).
```
4. ##### Check if interval A after B
```prolog
a_after_b(Interval_A_Name, Interval_B_Name).
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

# Run Example
```prolog
Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- start_interval('aaa').
aaa is starting
true.

?- start_interval('aaa').
aaa already started
false.

?- end_interval('aaa').
aaa is ended
true.

?- end_interval('aaa').
aaa is not running
false.

?- start_interval('aaa').
aaa already started
false.

?- start_interval('bbb').
bbb is starting
true.

?- start_interval('ccc').
ccc is starting
true.

?- end_interval('bbb').
bbb is ended
true.

?- end_interval('ccc').
ccc is ended
true.

?- listing(start).
:- dynamic start/2.

start(aaa, 1598779718.885983).
start(bbb, 1598779783.872037).
start(ccc, 1598779796.048538).

true.

?- listing(end).
:- dynamic end/2.

end(aaa, 1598779735.179297).
end(bbb, 1598779807.620872).
end(ccc, 1598779813.522073).

true.

?- a_before_b('aaa', 'bbb').
aaa before bbb
true.

?- a_before_b('bbb', 'aaa').
bbb don't before aaa
false.

?- a_after_b('bbb', 'aaa').
aaa before bbb
true.

?- a_after_b('aaa', 'bbb').
bbb don't before aaa
false.

?- a_overlaps_b('bbb', 'ccc').
bbb overlaps ccc
true.

?- a_overlaps_b('ccc', 'bbb').
ccc don't overlaps bbb
```
<span style="color:red">some **false.** text</span>

```prolog
?- a_overlapped_by_b('ccc', 'bbb').
bbb overlaps ccc
true.

?- a_overlapped_by_b('bbb', 'ccc').
ccc don't overlaps bbb
false.

?- start_interval('ddd').
ddd is starting
true.

?- start_interval('eee').
eee is starting
true.

?- end_interval('eee').
eee is ended
true.

?- end_interval('ddd').
ddd is ended
true.

?- a_during_b('eee', 'ddd').
eee during ddd
true.

?- a_during_b('ddd', 'eee').
ddd don't during eee
false.

?- a_contain_b('ddd', 'eee').
eee during ddd
true.

?- a_contain_b('eee', 'ddd').
ddd don't during eee
false.
```

