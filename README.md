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

