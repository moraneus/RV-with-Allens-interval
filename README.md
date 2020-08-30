# Details:
This program gets as input, during execution, interval starting/ending.
I used SWI-Prolog interpreter (Downloaded from: https://www.swi-prolog.org).
![Image of Allen's interval algebra] (https://raw.githubusercontent.com/moraneus/RV-with-Allens-interval/master/Interval%20types.png?token=ALS2F7N7KE4GSOSYXN7T4327JNV6I)
## Basic commands:
1. Start new interval `start_interval(Interval_Name)` (When new interval starts, it's take the current timestamp and use it as argument related to the `start` fact)
2. Ending running interval `end_interval(Interval_Name)`
3. Check if interval A before B `a_before_b(Interval_A_Name, Interval_B_Name)'
4. Check if interval A after B `a_after_b(Interval_A_Name, Interval_B_Name)'
5. Check if interval A meets interval B `a_meets_b(Interval_A_Name, Interval_B_Name)'



# Run Example

