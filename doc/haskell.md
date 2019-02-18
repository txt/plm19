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

## About Haskell

Vibrant community 

- 13,000+ packages available at [hackage](http://hackage.haskell.org/packages/browse). 
- Many, many  [recent Haskell books](https://www.amazon.com/s?k=Haskell&rh=n%253A3952&ref=nb_sb_noss) at Amazon
- Look at all  [the industrial applications](https://wiki.haskell.org/Haskell_in_industry) 

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

Lazy evaluation: Lazy" evaluation is performing operations when and as they are needed. It is useful when it is a feature of a programming language or library because it is generally harder to implement lazy evaluation on your own than simply to precalculate everything up front.
For example, the following sorts the list and then takes the
first element of the list. However, because of lazy evaluation,
only the head gets computed. For example, if we take the minimum
of the list [2, 1, 3,] `qsort` will first filter out all the
elements that are smaller than two. Then it does `qsort` on that
(returning the singleton list [1]) which is already enough. Because
of lazy evaluation, the rest is never sorted, saving a lot of
computational time.

```haskell
minimum xs = head (qsort xs)
```

Fully functional: functions can return functions. Note the following is called _currying_

```haskell
Prelude> f x y = x < y
Prelude> f 2 3
True
Prelude> g = f 2
Prelude> g 10
True
Prelude> g 1
False
```

- (Technical aside: Currying is when you break down a function that
takes multiple arguments into a series of functions that take part
of the arguments. FYI- all Haskell functions are "curried" down to
one; i.e.  every function in Haskell officially only takes one
parameter and if you need N arguments, Haskell builds a recursive
nesting of N argumnets. Which is why the above works so easily.
Currying provides the ability to partially apply one or more arguments to create more specific function, thus can be more effective in reuse code.
 This type of abstracting the common part as a high level function(with currying) can reduce your effect to write code, make test much easy , and reuse more code, which yields more elengant code.)

### Fun with Haskell

Lots of conventions for typing less

```haskell
fn x = ceiling (negate (tan (cos (max 50 x))))  
```
can also be defined as follows (thanks to currying):

```haskell
ghci> fn = ceiling . negate . tan . cos . max 50  
ghci> fn 60
2
```

`Filter` returns the items in a list that match some predicate p

```haskell
-- "_" is the annoymous variable (somethign to ignore).
-- "|" means a case statement

filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  

ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]  -- note the currying of "> 3 x" 
[5,6,4]

ghci> filter even [1..10]  
[2,4,6,8,10]  
```

Lambdas in Haskell are denoted with a "\\"

```Haskell
-- equivalent
map (+3) [1,6,3,2]  -- note the currying of "+ 3 x".
map (\x -> x + 3) [1,6,3,2]
```

Lambdas can use pattern matching:

```Haskell
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[3,8,9,8,7]  
```

`foldl` (left fold)  is applied between the starting value and the head of the list. That produces a new accumulator value and the binary function is called with that value and the next element, etc.

```haskell
total xs = foldl (\acc x -> acc + x) 0 xs  

ghci> total [3,5,2,1]  
11
```

Btw, the same can be done as follows (why?)

```haskell
total' = foldl (+) 0  

ghci> total' [3,5,2,1]  
11  

``` 

`foldl1` assumes that the first item in the list is the start value and the rest is list to be folded.

```haskell
most = foldl1 (\x acc -> if x > acc then x else acc)  

last' = foldl1 (\_ x -> x) -- wow. the function is anonymous,
```

## About Functional Languages

### What is a Functional Language?

A functional program consists of an expression E (representing both
the algorithm and the input). This expression E is subject to some
rewrite rules.

Reduction consists of replacing some part P of E by another expression
P' according to the given rewrite rules. ... This process of reduction
will be repeated until the resulting expression has no more parts that
can be rewritten.

Functional programming is characterized by the programming with
values, functions and functional forms.

Keywords and phrases: Lambda calculus, free and bound variables,
environment, functional programming, 
recursive functions, functional, curried function.

Functional programming languages are the result of both abstracting
and generalizing the data type of maps. Recall, the mapping m from
each element x of S (called the domain) to the corresponding element
m(x) of T (called the range) is written as:

```
m : S --> T
```

For example, the squaring function is a function of type:

```
sqr : Num --> Num
```

and may be defined as:

```
sqr where x |--> x*x
```

A linear function f of type

```
f : Num --> Num
```

may be defined as:

```
f where x |--> 3*x + 4
```

Functional programming is based on the mathematical concept of a function and functional programming languages include the following:

+ A set of primitive functions.
+ A set of functional forms.
+ The application operation.
+ A set of data objects and associated functions.
+ A mechanism for binding a name to a function.

LISP, FP, Scheme, ML, Miranda Ocaml,  Haskell are just some of the languages to implement this elegant computational paradigm.

The basic concepts of functional programming originated with LISP.

### Why Bother?

Functional programming languages are important for the following reasons.

+ Functional programming dispenses with the assignment command freeing the programmer from the rigidly sequential mode of thought required with the assignment command.
+ Functional programming encourages thinking at higher levels of abstraction by providing higher-order functions -- functions that modify and combine existing programs.
+ Functional programming has natural implementation in concurrent programming.
+ Functional programming has important application areas. Artificial intelligence programming is done in functional programming languages and the AI techniques migrate to real-world applications.
+ Functional programming has a close relationship to computer science theory. Functional programming is based on the lambda-calculus which in turn provides a framework for studying decidability questions of programming. The essence of denotational semantics is the translation of conventional programs into equivalent functional programs.
+ Functional programming is useful for developing executable specifications and prototype implementations.

```
n :: Int
n = 1000
main = do
print $ length [(a,b,c) | a<-[1..n],b<-[1..n],c<-[1..n],a^2+b^2==c^2]
```

and appropriate C version:

```  
  #include <stdio.h>
  
  int main(void)
  {
      int a,b,c, N=1000;
    int cnt = 0;
      
    for (a=1;a<=N;a++)
        for (b=1;b<=N;b++)
          for (c=1;c<=N;c++)
          if (a*a+b*b==c*c) cnt++;
      printf("%d\n", cnt);
    }
```

### What is functional programming good for?

Functional programming, like any good programming technique, is a
useful tool in your armory for solving some classes of problems. It's
very good for callbacks, which have multiple uses from GUIs through to
event-driven loops. It's great for expressing generic
algorithms. List.map is really a generic algorithm for applying
functions over any type of list. Similarly you can define generic
functions over trees. Certain types of numerical problems can be
solved more quickly with functional programming (for example,
numerically calculating the derivative of a mathematical function).

### Pure and impure functional programming

A pure function is one without any side-effects. A side-effect really
means that the function keeps some sort of hidden state inside
it. strlen is a good example of a pure function in C. If you call
strlen with the same string, it always returns the same length. The
output of strlen (the length) only depends on the inputs (the string)
and nothing else. Many functions in C are, unfortunately, impure. For
example, malloc - if you call it with the same number, it certainly
won't return the same pointer to you. malloc, of course, relies on a
huge amount of hidden internal state (objects allocated on the heap,
the allocation method in use, grabbing pages from the operating
system, etc.).

ML-derived languages like OCaml are "mostly pure". They allow
side-effects through things like references and arrays, but by and
large most of the code you'll write will be pure functional because
they encourage this thinking. Haskell, another functional language, is
pure functional. OCaml is therefore more practical because writing
impure functions is sometimes useful.

There are various theoretical advantages of having pure functions. One
advantage is that if a function is pure, then if it is called several
times with the same arguments, the compiler only needs to actually
call the function once. A good example in C is:

```
for (i = 0; i < strlen(s); ++i)
  {
      // Do something which doesn't affect s.
  }
```
    
If naively compiled, this loop is O(n^2) because strlen (s) is called
each time and strlen needs to iterate over the whole of s. If the
compiler is smart enough to work out that strlen is pure functional
and that s is not updated in the loop, then it can remove the
redundant extra calls to strlen and make the loop O(n). Do compilers
really do this? In the case of strlen yes, in other cases, probably
not.
    

