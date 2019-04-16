<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week10

## Kotlin

In LISP, write down code that is approximately
like the following Kotlin snippets:

1) Show that you know the difference
 between structural and referential equality.
```
val john1 = Person("John")
val john2 = Person("John")
john1 == john2    // true  (structural equality)
john1 === john2   // false (referential equality)
```

2) Show you know how to LISP handles defaults and types (hint
and its a bit of a trick question "how LISP handles types")

```
fun build(title: String="", width: Int = 800, height: Int = 600) {
    Frame(title, width, height)
}
```

3) Translate the following into LISP if(Test,THEN,ELSE)
calls.

```
when (x) {
    1 -> print("x is 1")
    2 -> print("x is 2")
    3, 4 -> print("x is 3 or 4")
    in 5..10 -> print("x is 5, 6, 7, 8, 9, or 10")
    else -> print("x is out of range")
}
```


4) suppose "map" was a list of lists of length 2. How 
to handle the following?

```
for ((key, value) in map) {
    print("Key: $key")
    print("Value: $value")
}
```


5) Do you know what null safety is?

```
val name = ship?.captain?.name ?: "unknown"
```

6) Do you know what lambdas are?


```
val sum = { x: Int, y: Int -> x + y }   
val res = sum(4,7)       
```


7) Can you walk a list, rejecting items?
Can you define, the carry around a function?

```
numbers.filter({ x -> x.isPrime() })
```

## ELM 

(Really, more about functional languages with with automatic
type inference.)

To study this section, see 
[here](http://unbox.org/doc/Seven%20More%20Languages%20in%20Seven%20Weeks.pdf).

Write down the following three functions in  LISP:

```
> first (head::tail) = head

> factorial x = \
| if | x==0->1\
|    | otherwise -> x * factorial (x - 1)

> count list = \
|   case list of \
|     [] -> 0 \
|     head::tail -> 1 + count tail
> count list = \
|   case list of \
|     [] -> 0 \
|     head::tail -> 1 + count tail
```

What is automatic type inferencing?
Use this ELM snippet to explain:

```
> [1, 2] ++ [3, 4]
[1,2,3,4] : [number]
```

Why do the following cause errors in ELM:

```
> [1, "2"]
> if x < 0 then "too small" else x
```

What is pattern matching? 
Use this ELM snippet to explain:

```
> list = [1, 2, 3]
[1,2,3] : [number]
> case list of \
| head::tail -> tail \ | []->[]
[2,3] : [number]
```


In the following, where is the constructor? What does it return?

```
> type Color = Black | White
> type Piece = Pawn | Knight | Bishop | Rook | Queen | King
> type ChessPiece = CP Color Piece
> piece = CP Black Queen
```


The following ELM snippet describes a familiar data structure.
What is it?

```
> type List = Nil | Cons Int List
```

Can the above be used to hold Strings? Why or Why not?
Write down the ELM recursive data type that can handle
arbitrary types:

```
XXXX?
```

What is currying? Give an example, using the following code:

```
> add x y = x + y
<function> : number -> number -> number
```

## Memory Management

Explain: in a stack-based language that only supports
local variables in functions, it is a relatively simple
matter to collect memory that is no longer used.

Explain: if we add an external memory heap to your
stack-based language, it becomes more complicated
to reclaim memory that is not longer used.

Conservation pointer recognition:

- How is it different to precise pointer recognition?
- Why not just use precise pointers all the time?
- Why is it that, often, we can get away with conservative pointer recognition?


Reference counting:

- What is it?
- When won't it work?
- Draw a diagram illustrating reference counting.  Label all parts.
  Show on the diagram how reference counting can fail.

Mark and sweep:

- What is it?
- How does it address the drawback of reference counting?
- Why don't we just mark and sweep all the time?
- Draw a diagram illustrating mark and sweep.  Label all parts.
- Explain: simple mark and sweep can lead to fragmentation and
  that is bad.

Compacting garbage collection:

- What is it?
- How does it address the drawback of mark and sweep?
- Explain: compacting collectors halve the available memory.

Generational garbage collection divides memory into generations
and new memory is allocated from a relatively small generation zero
(and longer living memory exists in a much larger generation 1 or 2).

- Explain: what heuristic does generational GC exploit?
- Explain:  what kind of GC would you recommended for the
  **first** generation? Why?
- Explain:  what kind of GC would you recommended for the
  **last** generation? Why?

