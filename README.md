# Racket Projects

This is a collection of my side projects in Racket. I'll continue to add more, and polish the existing ones. Maybe this'll have some good examples for someone learning to code in a functional language.

## Polished
- Binary-Encrypt
  * Various conversions between strings, decimal integers, and binary integers
  * require, provide, foldl, map, lambda, char->integer, string->list, let, if, reverse, list-tail, append, list, integer->char, list->string

- DividePrecise
  * Divide real numbers with arbitrary precision
  * provide, define, foldl, string-append, cdr, member, reverse, string->list, number->string, map, exact->inexact, let, if, string->number, cons

- Hex-Encrypt
  * Various conversions between strings, decimal integers, hexidecimal integers
  * define, foldl, string-append, reverse, map, lambda, number->string, char->integer, string->list, let, if, cddr, cons, list->string, or, and, letrec, length, cond, else, string-append, char?, string-ref

- Pi
  * Caluclates pi with arbitrary precision
  * require, define, expt, let, if, denominator, numerator, substring, string-append

- Primes
  * Calculates prime numbers using the Sieve of Eratosthenes
  * provide, define, let, cddr, values, if, empty?, reverse, filter, lambda, not, modulo, car, cdr, cons

## Rough
- Alpha
  * Prints an random distribution of stars to the screen horizontally and vertically
  * define, let, if, letrec, cons, cdr, list-ref, string-append, when, begin, flatten, make-list, length
   
- Basics
  * A short walkthrough on the basics of Racket.
  * define, displayln, lambda, for-each, map, let
  
- Binary-Decimal
  * Conversions between decimal and binary representations of positive integers
  * provide, define, let, if, cons, ceiling, char->integer, reverse, string->list, not, empty?, flatten, append, lambda, map, list->string, cdr, make-string

- Draw Tree (GUI)
  * Draws a graphical tree of arbitrary depth, width, span, and color
  * define, new, frame%, send, horizontal-panel%, slider%, checkbox%, button%, make-object%, point%, color%, lambda, unless, when, random, if, let, canvas%

- FactorPrime
  * Finds the prime factors of integers

- Factorials
  * Basic calculation of factorials

- FibonacciApprox
  * Calculates the n'th Fibonacci number with various approximations

- GraphMe (GUI)
  * Graphically plots lists of integers or points

- Perfect
  * Calculates perfect numbers

- UnionIntersectExcept
  * Examples of set operations without using high level operations
 
- Wander (GUI)
  * Graphical worm of arbitrary speed and length wanders randomly around the screen

- Waterfall-Encrypt
  * Encryption algorithm that uses previous chunk of the message to encrypt the current chunk

- Xibonacci
  * Abstraction on the Fibonacci sequence to allow arbitrary starting values and number of factors. Look up "Tribonacci" for an example

## Broken
- Integration
  * Calculates approximate area under a function, within given bounds on the x-y plane

- Tau
  * Calculates pi or tau using a different heuristic
