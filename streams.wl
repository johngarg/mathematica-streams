(* ::Package:: *)

Print["Mathematica-Streams: A simple, lightweight implementation of streams (lazily evaluated lists) in Mathematica."];


(* The implementation relies heavily on tail-call optimised recursion *)
$RecursionLimit = Infinity;
$IterationLimit = Infinity;


(* Delay the evaluation of the tail of the stream. *)
SetAttributes[Stream, {HoldRest}];
SetAttributes[makeStream, {HoldRest}];


(* Represent streams as linked lists whose tails are left unevaluated. Streams are constructed using makeStream and elements are accessed using sFirst and sRest. *)
makeStream[a_, b_] := Stream[a, b];
sFirst[s_] := s[[1]];
sRest[s_] := ReleaseHold@s[[2]];
streamQ[s_] := Head@s === Stream;
theEmptyStream = Null;
emptyStreamQ[s_] := s === Null;


(* Format streams as a list of the first two elements and an ellipsis indicating a promise for the rest. *)
Format[Stream[a_, b_]] := If[streamQ@b,
  "{" <> ToString@a <> "," <> ToString@sFirst@b <> ",...}",
  "{" <> ToString@a <> ",...}"]


(* Define a sRange function, like Range[] but for streams. *)
sRange[low_, high_] := If[low>high,
  theEmptyStream,
  makeStream[low, sRange[low+1, high]]];
sRange[high_] := sRange[1, high];


(* Use sRef to access the nth element of a stream. *)
sRef[s_, 1] := sFirst@s;
sRef[s_, n_]/;n>0 := sRef[sRest@s, n-1];


(* Like Map but for streams, implemented using recursion. *)
sMap[proc_, theEmptyStream] := theEmptyStream;
sMap[proc_, s_] := makeStream[proc[sFirst@s],
  sMap[proc, sRest@s]];


(* Select out the elements of stream subject to the condition pred. *)
sSelect[s_/;emptyStreamQ[s], pred_] := theEmptyStream;
sSelect[s_, pred_] := If[pred[sFirst@s],
  makeStream[sFirst@s, sSelect[sRest@s, pred]],
  sSelect[sRest@s, pred]];


(* Take the first n elements of the stream and output as a Mathematica list *)
sTake[theEmptyStream,l_] := l;
sTake[s_, l_, n_] := sTake[sRest@s, Append[l, sFirst@s], n-1];
sTake[s_, l_, 0] := l;
sTake[s_, n_] := sTake[s, {}, n];


(* Take the elements of the stream while pred is true and output as Mathematica list *)
sTakeWhile[pred_, s_] := If[pred[sFirst@s], 
  Prepend[sTakeWhile[pred, sRest@s], sFirst@s],
  {}];


(* Some useful streams. *)

(* The set of integers and natural numbers. *)
naturalGenerator[n_] := makeStream[n, naturalGenerator[n+1]];
sNaturals = naturalGenerator[1];

(* Fibonacci *)
fibGenerator[a_, b_] := makeStream[a, fibGenerator[b, a+b]];
sFibs = fibGenerator[0, 1];

(* A stream of Primes calculated using the Sieve of Eratosthenes *)
primeGenerator[s_] := makeStream[sFirst@s,
  primeGenerator[sSelect[sRest@s,!(Divisible[#, sFirst@s])&]]];
sPrimes = primeGenerator[naturalGenerator@2];
