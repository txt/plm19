<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>

# HASKELL

Yet another another functional language. Like [Ocaml](ocaml.md) or
[LISP](lisp1.md) (but LISP prototypes the basics of functional languages, so it muddled up
many things... which modern functional lanauges like Haskell, Ocaml, etc have unmuddled).

To remind you:

- Procedural languages have procedures that do things and read/write results to some memory locations.
- Logic languages have predictates that do/don't match and as a side-effect update some bindings.
- Functional languages haves functions that convert inputs (of some types) to outputs (of another types)
      - If you call a function N times, with the same inputs, you will always get the same outputs
      - Unlike procedures.

### About Haskell


Oh so succict. Here's quicksort:

-  Like Prolog, [] = empty list. 
- Also x:xs is a list whos's car is x and cdr is xs.
- Finally "++" concatenates lists

```haskell
qsort [] = []
qsort (x:xs) = 
     qsort(filter (<=x) xs) 
     ++ [x] 
     ++ qsort(filter (>x) xs)
```

Lazy evaluation

- Infinite lists are cool!

Fully functional

```
Prelude> f x y = x < y
Prelude> f 2 3
True
Prelude> g = f 2
Prelude> g 10
True
Prelude> g 1
False
```

Vibrant community 

- 13,000+ packages available at [hackage](http://hackage.haskell.org/packages/browse). 
- Many, many  [recent Haskell books](https://www.amazon.com/s?k=Haskell&rh=n%253A3952&ref=nb_sb_noss) at Amazon
- Look at all  [the industrial applications](https://wiki.haskell.org/Haskell_in_industry) 


