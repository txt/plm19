<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week6

Haskell

- what is the differente betweena  function, a predicate and a function?
- using examples, explain the role of functional  in:
     - optimization
     - formal reasoning abiut a prorgram (to prove, e.g., safety properties)
- what is `x:xs`
- can you write down `below` that accepts 2 args and returns true if the first is less than the second?
- can you curry that function?
- can you use that in a (filer xxx [10,30,50,10]) list to find all items under 35?
- can you explain the quicksort code from the lecture?
- can you explain the role of lazy evaluation in `min xs = head (qsort xs)` ?
- can you explain the role of list comprehensions in 

```haskell
n :: Int
n = 1000
main = do
print $ length [(a,b,c) | a<-[1..n],b<-[1..n],c<-[1..n],a^2+b^2==c^2]
```
