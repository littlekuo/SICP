#lang racket

; the rewritten expmod generates a tree recursion

; T(n) = 2*T(n/2) + c