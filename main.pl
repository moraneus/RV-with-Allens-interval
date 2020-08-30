:- dynamic start/2.
:- dynamic end/2.

start_interval(A) :- start(A, _), !, format("~a already started", [A]), fail.
start_interval(A) :- get_time(Now), assert(start(A, Now)), format("~a is starting", [A]).

end_interval(A) :- (start(A, _), not(end(A, _))), !, get_time(Now), assert(end(A, Now)), format("~a is ended", [A]).
end_interval(A) :-  format("~a is not running", [A]), fail.

a_before_b(A, B):- (end(A, _a_e), start(B, _b_s), _a_e < _b_s), !, format("~a before ~a", [A, B]).
a_before_b(A, B):- format("~a don't before ~a", [A, B]), fail.

a_after_b(A, B):- a_before_b(B, A).

a_meets_b(A, B):- (end(A, _a_e), start(B, _b_s), _a_e =:= _b_s), !, format("~a meets ~a", [A, B]).
a_meets_b(A, B):- format("~a don't meets ~a", [A, B]), fail.

a_met_by_b(A, B):- a_meets_b(B, A).

a_overlaps_b(A, B):- (start(A, _a_s), end(A, _a_e), start(B, _b_s), end(B, _b_e), _a_s < _b_s, _a_e < _b_e), !, format("~a overlaps ~a", [A, B]).
a_overlaps_b(A, B):- format("~a don't overlaps ~a", [A, B]), fail.

a_overlapped_by_b(A, B):- a_overlaps_b(B, A).

a_starts_b(A, B):- (start(A, _a_s), start(B, _b_s), _a_s =:= _b_s), !, format("~a starts ~a", [A, B]).
a_starts_b(A, B):- format("~a don't srarts ~a", [A, B]), fail.

a_started_by_b(A, B):- a_starts_b(B, A).

a_finishes_b(A, B):- (end(A, _a_e), end(B, _b_e), _a_e =:= _b_e), !, format("~a finishes ~a", [A, B]).
a_finishes_b(A, B):- format("~a don't finishes ~a", [A, B]), fail.

a_finished_by_b(A, B):- a_finishes_b(B, A).

a_during_b(A, B):- (start(A, _a_s), end(A, _a_e), start(B, _b_s), end(B, _b_e), _a_s > _b_s, _a_e < _b_e), !, format("~a during ~a", [A, B]).
a_during_b(A, B):- format("~a don't during ~a", [A, B]), fail.

a_contain_b(A, B):- a_during_b(B, A).

a_equal_b(A, B):- (start(A, _a_s), end(A, _a_e), start(B, _b_s), end(B, _b_e), _a_s =:= _b_s, _a_e =:= _b_e), !, format("~a equal ~a", [A, B]).
a_equal_b(A, B):- format("~a don't equal ~a", [A, B]), fail.
