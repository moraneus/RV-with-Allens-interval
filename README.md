# Details:
This program gets as input, during execution, interval starting/ending.

I used SWI-Prolog interpreter (Downloaded from: https://www.swi-prolog.org).

![Image of Yaktocat](https://www.researchgate.net/profile/Ioannis_Tsamardinos/publication/230561978/figure/fig2/AS:646067146223617@1531045819115/1-The-13-relations-between-intervals-in-Allens-algebra-Interval-A-is-always-either-at.png)
## Basic commands:
1. Start new interval `start_interval(Interval_Name)` (When new interval starts, it's take the current timestamp and use it as argument related to the `start` fact)
2. Ending running interval `end_interval(Interval_Name)`
3. Check if interval A before B `a_before_b(Interval_A_Name, Interval_B_Name)'
4. Check if interval A after B `a_after_b(Interval_A_Name, Interval_B_Name)'
5. Check if interval A meets interval B `a_meets_b(Interval_A_Name, Interval_B_Name)'



# Run Example

