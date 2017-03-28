# *Mathematica* Streams

A simple, lightweight implementation of streams (lazily evaluated lists) in *Mathematica*. A stream is represented as a linked list whose tail is left unevaluated until the list is walked.

### Constructors and Methods

The function `makeStream` creates a stream.
```mathematica
integersFrom[n_] := makeStream[n, integersFrom[n + 1]];
sNaturals = integersFrom[1];
```
Upon evaluation the stream `sNaturals` is rendered in the notebook with the first two elements printed in curly braces along with an ellipsis: `{1, 2, ...}`.

By convention, stream-related functions begin with a lowercase S. Thus, the regular methods of functional programming `Map`, `Select`, `First` and `Rest` have stream analogues `sMap`, `sSelect`, `sFirst` and `sRest`. For example, the Sieve of Eratosthenes could be implemented
```mathematica
primeSieve[s_] := makeStream[sFirst@s,
  primeSieve[sSelect[streamRest@s, !(Divisible[#, streamFirst@s])&]]];
sPrimes = primeSieve[integersFrom@2];
```
where `sPrimes` evaluates to `{2, 3, ...}` in the notebook. The additional functions `sRef`, `streamToList`, `sRange` and `sTake` are also provided.

### Usage

As a simple example, let's consider finding the third prime number in the interval from ten thousand to one million. Using streams is much quicker than using a *Mathematica* list since the entire list needs to be created before the third element is taken. Thus, the evaluation times for
```mathematica
Select[Range[10^4, 10^6], PrimeQ[#]&][[3]]
```
and
```mathematica
sSelect[sRange[10^4, 10^6], PrimeQ@#&]//sRef[#,3]&
```
differ by four orders of magnitude.

### Todos

Currently the evaluation of the `sRest` of a stream does not memoize the `sFirst@sRest` of the stream. This leads to performance issues: in many cases the same elements of the stream will need to be accessed multiple times and in this case memoization provides a good optimization. 
