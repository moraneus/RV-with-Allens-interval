/* Author: Moran Omer
   Description:
   The specification contains two types of boolean operands:
   1. Allen's Interval Algebra boolean operands (before, after, contains, during, overlaps, 
      overlapped_by, meets, met_by, starts, started_by, finishes, finished_by, equals)
   2. Logic operands (and, or, not).
   
   The priority of Allen's operands [1] is higher than the logic operands [2].
   An expression can be written in various ways (prefix, infix, or any combination). 
   For example, let's take the expression 'a before b and not(d after c)'; all the next are valid formats:
   1. 'a before b and not(d after c)'
   2. 'before(a, b) and not(d after c)'
   3. 'before(a, b) and not(after(d, c))'
   4. 'and(before(a, b), not(after(d, c)))'
*/
% specification :- 
(((before(a, b) or d after c) and (f overlaps g)) 
				 and (c after b and (e during d and finished_by(i ,k)))) and (((d contains e and g overlapped_by f) 
				 and (g meets h and i met_by g)) and ((h equals i and m starts i) and (h started_by m and finishes(k, i)))).
