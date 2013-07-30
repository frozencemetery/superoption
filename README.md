*superoption*

/"We've done a bad thing."/

Functional programmers are familiar with Option types (or `Maybe`s, in Haskell-speak).  This extends the idea of an option type from just `None` and `Sum` (in SML-speak) to `None`, `Some`, and `Lots`.  This allows programmers more fine-grained control over their types that can hold any number of values (ours are called "superoption").

One possible application of this demonstrated here is sorting algorithms.  Algorithms which have runtime O(n^2) can often be faster for small inputs than those of O(nlogn) or similar.  The programmer can use superoption types to determine which algorithm to use.

We further include the notion of `All` in our superoptions, which is for holding infinite numbers of elements.

Since this is already a bizarre and not particularly serious endeavor, we use GADTs to enforce various constraints.
