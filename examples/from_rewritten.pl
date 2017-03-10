:- coinductive from1/1.
:- coinductive from2/1.

from1(k1(X)) :- from2(X).

% just a dumb redundant predicate
from2(k2(X)) :- from1(X).