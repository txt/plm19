#!/usr/bin/env gst

"things in quotes are comments"

Transcript nextPutAll: 1 printString; 
           cr " command are grouped by '!'"
!

! Object methods !
oo
  Transcript nextPutAll: self printString; 
             cr!
!  

| x y | "locals vars. live till next '!'"
x := 1.
y:=2.
(x+y) oo  "should print 3"
!

"now go to 
 https://people.eecs.berkeley.edu/~fateman/264/papers/smalltalk-tutorial.html 
 and try the examples there in section 1 and 2"

"e.g. 1.2"

'Hello, world' oo!
