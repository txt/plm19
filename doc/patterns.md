

<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>

# Patterns

Experts are experts since they've learned patterns of useful behavior (fyi: expert = 5 patterns per day * 10 years)

If we look at N concrete things,  and then abstract our thinking a little,
then we can find some repeated features that might even apply to future things.

For example...

## Patterns in chess:

- more space in the center
- open lines
- weak squares
- pawn majority, pawn minority
- two weak points to attack - the Principle of Two Weaknesses
- weak kingside
- weak queenside pawn structures
- inactive knight at the corner
- dead bishop
- weak isolated d-pawn
- insufficient piece coordination
- attacking mark
- free protected pawn
- bishop pair
- better minor piece in open position - bishop versus knight
- better minor piece in blocked position - knight versus bishop
- break through
- and others etc. 

e.g forking:

![](https://raw.githubusercontent.com/txt/seng18/master/img/chessfork1.png)

![](https://raw.githubusercontent.com/txt/seng18/master/img/chessfork2.png)

![](https://raw.githubusercontent.com/txt/seng18/master/img/chessfork3.png)





## Patterns in Object Design

<img width=500 src="https://raw.githubusercontent.com/txt/seng18/master/img/chbrowser.png">

<img width=500 src="https://raw.githubusercontent.com/txt/seng18/master/img/filebrowser.png">

<img width=500 src="https://raw.githubusercontent.com/txt/seng18/master/img/compositebrowser.png">

<img width=500 src="https://raw.githubusercontent.com/txt/seng18/master/img/compositepattern.png">




<a href="https://raw.githubusercontent.com/txt/seng18/master/img/gofpattern.jpg"><img width=500
src="https://raw.githubusercontent.com/txt/seng18/master/img/gofpattern.jpg"></a>





Gang of Four (1994

Erich Gamma, Richard Helm, Ralph Johnson and John Vlissides 

If you read this book, and you are not excited, then check your pulse. You're dead.

<img width=400 src="../img/gofbook.png">


Patterns are a religion

<img width=400 src="../img/onering.png">
 
# "Patterns" in function programming
 
Functional programmers (Haskell, Clojure, F#, Lisp, Skill, Racket,….) make up 10 patterns for breakfast.

Cause its so easy (say what? OO makes some easy things hard?)

In LISP, one (and only one) recursive data type, the list which contains either
atoms
or another list

e.g. visitor pattern in functional (5 lines)

```lisp
(defun visitr (things f)
  "visitor patterns in functional programming"
  (if (atom things)
      (funcall f things)   ; then
      (dolist (one things) ; else do for each
         (visitr one f))))

(defun demo (&aux all)
  (let ((nastyComplexThing
     '(a 
       (b 1) 
       c
       (d e 
          (f g 
         (2 3 4)
         )
          h)
       (i j)
       (k
        (l
         (m
          (n o p q r 
         (s 5 6 7)
         (t 8 9 )
         u v w x y z)))))))
    (visitr nastyComplexThing
        (lambda (x)
          (if (numberp x)
          (push x all))))
    all))

(print(demo))

; output
; (9 8 7 6 5 4 3 2 1)
```

## Documenting a Pattern



## Examples of Patterns
- [Patterns in Ruby](https://github.com/davidgf/design-patterns-in-ruby)
- [Patterns in Haskell](http://blog.ezyang.com/2010/05/design-patterns-in-haskel/)

