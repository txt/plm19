
<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>

# Memory Management

## Summary of this lecture


Memory management: don't do it yourself.
Some problems are just too hard.


## When Memory Management is Easy

Algol-like languages: local variables are easy.

- when memory stack pointer at position I,
- call a function:
- push items for its local variables to I+1, I+2, etc
- work the function
- execute the function, return a  value
- pop stack back to "I"
- place the return v alue at "I+1"


But then, there's the question of the heap (memory not in the function local variables)

```
Stack
.---------.    
|         |
.---------.    
|         |
.---------.    
|         |
.---------.    
| pointer | ----->  Heap {...}
.---------.
```

And that way, madness lies

<img width=800 src="https://blog.parasoft.com/hubfs/literal-leak-01.jpg">

## Words to know

- constructor : e.g. `malloc()`
- destructor; e.g. `free()`
- heap
- stack

Tradeoffs

- precise vs. conservative
- moving/compacting vs. non-moving
- stopping vs. incremental
- generational vs. non-generational

## Reading:

My notes below come from:

- https://www.iecc.com/gclist/GC-faq.html
- https://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec26-gc/lec26.html



## Slogans

C programmers think memory management is too important to be left to the computer.

- Because with C, programmers manager more with `malloc()` and `free()`

Lisp programmers think memory management is too important to be left to the user.

- Because with Lisp (and nearly every "modern" language), memory management is built in
- But it took _decades_ to get it right


## Examples:

Garbage = allocated space that is no longer usable by the program. For example, consider this code. e.g. OCAML:

	let x = [[1; 2; 3]; [4]] in
	let y = [2] :: List.tl x in
	    y

- Here the variable x is bound to a list of two elements, each of which is itself a list. 
- Then the variable y is bound to a list that drops the first element of x and adds a different first element and the function returns this new list. 
- Since x is never used again that first list is inaccessible, or garbage.

C language (I.e what can go wrong with manual memory management):

- Using stack-allocated structures beyond their lifetimes (i.e. use after free);
     - Under manual memory management, this usually occurs when one part of a program decides it has finished using a memory block, and is unaware that another part of the program is still using it. This is rare under automatic memory management.
- Using heap-allocated structures after freeing them (i.e. use after free);
- Neglecting to free heap-allocated objects when they are no longer required (i.e. memory leak);
- Failing to allocate memory for a pointer before using it;
- Allocating insufficient memory for the intended contents;
- Loading from allocated memory before storing into it;
- Dereferencing non-pointers as if they were pointers.
- And more!

C++:

- All the problems of C and..
- Excessive copying by copy constructors (1);
- Unexpected sharing due to insufficient copying by copy constructors;


## What is garbage collection?

- Garbage collection is a part of a language's runtime system, or an add-on library, perhaps assisted by the compiler, the hardware, the OS, or any combination of the three, that automatically determines what memory a program is no longer using, and recycles it for other use. It is also known as _automatic storage (or memory) reclamation_.

Why is it good?

- Manual memory management is (programmer-)time consuming, and error prone. Most programs still contain leaks. This is all doubly true with programs using exception-handling and/or threads.
- A second benefit of garbage collection, less obvious to people who haven't used it, is that relying on garbage collection to manage memory simplifies the interfaces between components (subroutines, libraries, modules, classes) that no longer need expose memory management details ("who is responsible for recycling this memory").

Is garbage collection slow?

- Not necessarily.  A variety of algorithms allow garbage collection to proceed concurrently, incrementally, and (for some definitions of the term) in "real time". There are incremental garbage collectors that work with C and C++, for instance.
- Also,
memory has become so cheap that garbage collectors have been applied to very-large heaps. For very many applications modern garbage collectors provide pause times that are completely compatible with human interaction (pause times less than a fraction of second).
- Also, also, if memory leaks cause crash then _not_ doing garbage collection seems infinitely slower.

Can I use garbage collection with C or C++?

- Probably. Modern (well-tested, efficient, non-pausing) garbage collectors are available that work with all but the most pathological C and C++ programs, including legacy code. 

## Manual vs Automatic Memory Management

Manual storage management by the programmer:

- Often not so good (see examples of above of C and C++)

Languages like Java and OCaml 

- The system automatically identifies blocks of memory that can never be used again by the program and reclaims their space for later use.
- Programmer does not have to worry about when to deallocate a given block of memory. 
- Helps modular programming, because two modules can share a value without having to agree on which module is responsible for deallocating it. The details of how boxed values will be managed does not pollute the interfaces in the system.



A good automatic garbage collector:

- Identifies most garbage.
- Anything it identifies as garbage must be garbage.
- It should impose a low added time overhead.
- During garbage collection, the program may be paused, but these pauses should be short.


## Myths

Folk myths:


- GC is necessarily slower than manual memory management.
- GC will necessarily make my program pause.
- Manual memory management won't cause pauses.
- GC is incompatible with C and C++.

Folk truths:

- Most allocated objects are dynamically referenced by a very small number of pointers. The most important small number is ONE.
- Most allocated objects have short lifetimes.
- Allocation patterns (size distributions, lifetime distributions) are bursty, not uniform.
- VM behavior matters.
- Cache behavior matters.
- "Optimal" strategies can fail miserably (so need to adjust locally).

## Garbage Collection

Memory heap = directed graph in which the nodes are blocks of memory and the edges are the pointers between these blocks. 

Garbage collection = reclaim unused memory

- i.e. blocks not reachable from root (some _main_ function)

When to trigger?

- Not when low on memory 
	- Usually means memory is full of garbage
	- Much to fix up
	- Leads to long pauses
- Better to trigger after some magic number of allocations (that is proportional to amount of non-garbage found after last collection)



## Precise vs Conservative


Reachability means "find the pointers"

- a word of memory cells is just a sequence of bits:
	-   how can the garbage collector tell apart a pointer from an integer? 
- Why not reserve a bit in every word to indicate whether the
   value in that word is a pointer or not? 
	- This tag bit uses up about 3% of memory, which may be acceptable. 
	- It also limits the range of integers (and pointers) that can be used. On a 32-bit machines, using a single tag bit means that integers can go up to about 1 billion, and that the machine can address about 2GB instead of the 4GB that would otherwise be possible. 
- Adding tag bits also introduces
a small run-time cost that is incurred during arithmetic or when
dereferencing a pointer.  

A different solution:

- compiler record information that the garbage collector can query
at run time to find out the types of the various locations on the
stack. 
Avoids the need
for tag bits 
- Substantially more complicated because the
garbage collector and the compiler become more tightly coupled.

Or, be _conservative_

- if the collector encounters something that looks like it might
be a pointer, it treats it as if it is one, and the memory block
it points to is treated as reachable. 
- Memory is considered unreachable
only if there is nothing that looks like it might be a pointer to
it. 
	- May fail to collect some garbage
	-   but it won't deallocate anything but garbage. 
- In practice it works pretty well 
	- most integers are small 
	- most pointers look like large integers. 
	- relatively few cases in which the collector is not sure whether a block of memory is garbage.

## Reference Counting


- Each block of memory knows how many pointers there are incoming to that block. 
- When the count goes to zero, the block is unreachable and can be deallocated.
- Problems1: It imposes a lot of run-time overhead, 
	-  each time a pointer is updated, the reference counts of two blocks of memory must be updated (one incremented, one decremented). 
	- This cost can be reduced by doing compile-time analysis to determine which increments and decrements are really needed.
- Problem2: It can take a long time,
	- deallocating one object can cause a cascade of other objects to be deallocated at the same time. 
	- The solution to this problem is to put objects to be deallocated onto a queue. When an allocation for n bytes is performed, objects taking up space totaling at least n bytes are dequeued and deallocated, possibly causing more objects to lose all their references and be enqueued.
- Problems3 (and this is the big one): cannot collect garbage that lies on a cycle in the heap graph
	-  because the reference counts will never go down to zero. 
	- Cyclical data structures are common, for instance with many representations of directed graphs.

Since simple reference counting is not so good, we need mark and sweep.

## Mark and Sweep

- A mark phase in which all reachable memory is marked as reachable, 
- A sweep phase in which all memory that has not been marked is deallocated. 
- Requires that every block of memory have a bit reserved in it to indicate whether it has been marked.
- Marking for reachability is essentially a graph traversal; 
	- depth-first or a breadth-first traversal. 
- Problem:
	- Marking is that graph traversal takes O(n) space where n is the number of nodes. 
	- However, not as bad as the graph traversal needs only a single bit per node in the graph
	- But, if garbage collection is being performed because the system is low on memory, there may not be enough added space to do the marking traversal itself


Mark and sweep can an take a ling time, during which time, whole program/computer freezes. 
We need generational Garbage Collection

## Generational Garbage Collection

- Heuristic: trash dies early
- Place new objects in eden where fast garbage collectors run, very often.
	- See compacting garbage collection , below
- Objects that survive N collections get promoted to next generation
	- That has better, but slower, collectors that are  run less often.

## Compacting

- Deallocating garbage is nice, but the space that it creates may be scattered among many small blocks of memory. 
- This external fragmentation may prevent the space from being used effectively. 
- A compacting (or copying) collector is one that tries to move the blocks of allocated memory together, compacting them so that there is no unused space between them. 
- Compacting collectors tend to cause caches to become more effective, improving run-time performance after collection.
- Compacting collectors are difficult to implement because they change the locations of the objects in the heap. 
	- This means that all pointers to moved objects must also be updated. This extra work can be expensive in time and storage.
- Some compacting collectors work by using an object table containing pointers to all allocated objects. 
	- Objects themselves only contain pointers into (or indices of) the object table. 
	- This solution makes it possible to move all allocated objects around because there is only one pointer to each object. 
	- However, it doubles the cost of following a pointer.

## Why Not Compact and Garbage Collect at the Same time.

A copying collector

- starts from a set of roots 
- traverse all of the reachable memory-allocated objects, copying them from one half of memory into the other half.  
- The area of memory that we copy from is called old space (or from-space) and the area of memory that we copy to is called new space (or to-space).  
- When we copy the reachable data, we compact it so that it is in a contiguous chunk.  
	- squeeze out the holes in memory that the garbage data occupied.  
- After the copy and compaction, we end up with
	- a compacted copy of the data in new space data and 
	- a (hopefully) large, contiguous area of memory in new space in which we can quickly and easily allocate new objects.  
- The next time we do garbage collection, the roles of old space and new space will be reversed.

Example: Here "|" equals half way point::

	Obj1    ,  Obj2    ,  Obj3    ,  Obj4    ,  Obj5 |    , , , , ,                        

- At this point, we've filled up half of memory, so we initiate a collection.  
- Old space is on the left and new space on the right.  

Suppose further only objects 2 and 4 are reachable from the stack.  After copying and compacting, we would have a picture like this:

	Obj1    ,  Obj2    ,  Obj3    ,  Obj4    ,  Obj5 |    Obj2'  ,  Obj4', , , ,             

 Notice that we copied the live data (the red and light-blue objects) into new space, but left the unreachable data in the first half.  Now we can "throw away" the first half of memory (this doesn't really require any work):

	, , , , ,                                        |    Obj2  ,  Obj4, , , ,             

Now computational restarts again, this time allocating to the right hand side and, in the figure, collection to the left.

And so on.

Pros:
	- Compacts memory, and hence avoids any fragmentation. 
	- Running time is proportional to the amount of live memory, not the size of the heap. 

Cons:
	 Since a copying collector inherently moves objects, need precise points since impossible to update ambiguous pointer. 



