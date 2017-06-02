(*
   Copyright (c) 2017 John Gargalionis.

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

Print["Mathematica-Streams: A simple, lightweight implementation of streams (lazily evaluated lists) in Mathematica."];

BeginPackage["Streams`"];

Stream::usage         = "A lazily evaluated list.";
makeStream::usage     = "Constructs a stream.";
sFirst::usage         = "Returns the first element of the stream.";
sRest::usage          = "Returns the rest of the stream.";
streamQ::usage        = "Returns the True if passed a stream, false otherwise.";
theEmptyStream::usage = "The empty stream.";
sRange::usage         = "Like Range but for streams.";
sRef::usage           = "Accesses the nth element of a stream.";
sMap::usage           = "Like Map but for streams.";
sSelect::usage        = "Like Select but for streams.";
sTake::usage          = "Take the first n elements of the stream and output as a Mathematica list.";
sTakeWhile::usage     = "Take the elements of the stream while the predicate is true and output as Mathematica list.";
sNaturals::usage      = "A stream of natural numbers starting from 1.";
sFibs::usage          = "A stream of Fibonacci numbers.";
sPrimes::usage        = "A stream of prime numbers.";

Begin["`Private`"]

Unprotect[Stream, makeStream, sFirst, sRest, streamQ, theEmptyStream, sRange,
          sRef, sMap, sSelect, sTake, sTakeWhile, sNaturals, sFibs, sPrimes];

(* The implementation relies heavily on tail-call optimised recursion *)
$RecursionLimit = Infinity;
$IterationLimit = Infinity;

(* Delay the evaluation of the tail of the stream. *)
SetAttributes[Stream, {HoldRest}];
SetAttributes[makeStream, {HoldRest}];

(* Represent streams as linked lists whose tails are left unevaluated. Streams are constructed using makeStream and elements are accessed using sFirst and sRest. *)
makeStream[a_, b_]     := Stream[a, b];
sFirst[s_Stream]       := s[[1]];
sRest[s_Stream]        := ReleaseHold@s[[2]];
streamQ[s_Stream]      := Head@s === Stream;
emptyStreamQ[s_Stream] := s === Null;
theEmptyStream          = Null;

(* Format streams as a list of the first two elements and an ellipsis indicating a promise for the rest. *)
Format[Stream[a_, b_]] := If[streamQ@b,
                             "{" <> ToString@a <> "," <> ToString@sFirst@b <> ",...}",
                             "{" <> ToString@a <> ",...}"]

(* Define a sRange function, like Range[] but for streams. *)
sRange[low_, high_] := If[low>high,
                          theEmptyStream,
                          makeStream[low, sRange[low+1, high]]];
sRange[high_]       := sRange[1, high];

(* Use sRef to access the nth element of a stream. *)
sRef[s_Stream, 1]       := sFirst@s;
sRef[s_Stream, n_]/;n>0 := sRef[sRest@s, n-1];

(* Like Map but for streams, implemented using recursion. *)
sMap[proc_, theEmptyStream] := theEmptyStream;
sMap[proc_, s_Stream]       := makeStream[proc[sFirst@s],
                                          sMap[proc, sRest@s]];

(* Select out the elements of stream subject to the condition pred. *)
sSelect[s_Stream/;emptyStreamQ[s], pred_] := theEmptyStream;
sSelect[s_Stream, pred_]                  := If[pred[sFirst@s],
                                                makeStream[sFirst@s, sSelect[sRest@s, pred]],
                                                sSelect[sRest@s, pred]];

(* Take the first n elements of the stream and output as a Mathematica list *)
sTake[theEmptyStream,l_] := l;
sTake[s_Stream, l_, n_]  := sTake[sRest@s, Append[l, sFirst@s], n-1];
sTake[s_Stream, l_, 0]   := l;
sTake[s_Stream, n_]      := sTake[s, {}, n];

(* Take the elements of the stream while pred is true and output as Mathematica list *)
sTakeWhile[pred_, s_Stream] := If[pred[sFirst@s],
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
primeGenerator[s_Stream] := makeStream[sFirst@s,
                                       primeGenerator[sSelect[sRest@s, !(Divisible[#, sFirst@s])&]]];
sPrimes = primeGenerator[naturalGenerator@2];

Protect[Stream, makeStream, sFirst, sRest, streamQ, theEmptyStream, sRange,
        sRef, sMap, sSelect, sTake, sTakeWhile, sNaturals, sFibs, sPrimes];

End[]

EndPackage[]
