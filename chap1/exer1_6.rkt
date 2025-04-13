#lang racket


Since new-if is a function, and not a special form, each parameter 
subexpression will be evaluated before the procedure is applied. 
It means that when executing the sqrt-iter function, (good-enough? guess x), 
guess and (sqrt-iter (improve guess x) x) will always be evaluated 
before new-if is applied.

Since the second alternative (sqrt-iter (improve guess x) x) is 
calling the function recursively, the function will be stuck in 
an infinite loop. In this case, it is interesting to note that 
new-if will never be executed.