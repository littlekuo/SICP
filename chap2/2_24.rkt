#lang racket

; result
; (1 (2 (3 4)))

; tree
;(1 (2 (3 4)))
;    ^
;   /  \
;   1   ^ (2 (3 4))
;     /   \
;    2     ^ (3 4)
;        /   \
;       3     4


;box and pointer

;   +---+---+  +---+---+
;   | * | *-+->| * | / |
;   +-+-+---+  +-+-+---+
;     |          |   
 ;    V          V      
;   +---+      +---+---+  +---+---+
;   | 1 |      | * | *-+->| * | / |
;   +---+      +-+-+---+  +---+---+
;                |          |
;                V          V
;              +---+      +---+---+  +---+---+
;              | 2 |      | * | *-+->| * | / |
;              +---+      +-+-+---+  +-+-+---+
;                           |          |
;                           V          V
;                         +---+      +---+
;                         | 3 |      | 4 |
;                         +---+      +---+