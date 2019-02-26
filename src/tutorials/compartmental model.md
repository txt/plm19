# Building a Compartmental Model

## 1. Components of a compartmental model

In software engineering, a compartment model is a type of mathematical model used for describing the way information is 
transmitted among the compartments of a system.

To do this, we require 3 primary entities:
    
+ `Stock`s store quantities;
+ `Flow`s dribble quanities between `Stock`s;
+ `Aux`illary variabes contain business logic.

For our purposes, all of these are nearly the same so we represent
them all as `Thing`s.

### 1.1 `Things`

A `Thing` is defined as follows:

```python
class Thing:
  def __init__(i,txt='thing',init=None,lo=0,hi=100):
    i.init = init if init != None else lo
    i.lo,i.hi = lo,hi
    i.lo = i.lo if i.lo < i.init else i.init
    i.hi = i.hi if i.init < i.hi else i.init*2
    i.txt = 0,txt

  def restrain(i,x):
    return max(i.lo, min(i.hi, x))

  def rank(i):
    raise NotImplementedError(
             '"rank" must be implemented in subclass')
```


`Stock, Flow, Aux` are subclasses of `Thing`.
All these `Thing`s have different `rank`s (so when we print a
list of `Thing`s, we can sort them out into all the `Stock`s, 
before all the `Aux`s before all the `Flow`s.)

```python
class Percent(Thing) : 
  def rank(i): return 4
class Flow(Thing) : 
  def rank(i): return 3
class Stock(Thing): 
  def rank(i): return 1
class Aux(Thing)  : 
  def rank(i): return 2
```

Also, `Thing`s *restrain* themselves to always be between 
a `lo` and `hi` value.

### 1.2 `Things`
`Things` stores out meta-knowledge about variables in a model.
The call:

     Things(C = S(100), D = S(0),
            q = F(0),  r = F(8), s = F(0))

creates `Stock`s, `Flow`s (no `Aux`illary variables)
with names `C,D,q,r,s`. A `Things` also holds `order`
which is all the things sorted by their rank. The
method `asList` prints out values in their `order`.

`Things` know how to generate `payloads`; i.e. `o` instances
whose fields contain (initially) all the init values
of our `Thing`s.  These `payloads` can be updated
with current contents of the working memory.

```
class Things:
  def __init__(i,**things):
    i.things = things
    i.order = [k for k in sorted(i.things.keys(), 
               key=lambda z:i.things[z].rank())]
    for k in i.order:
      i.things[k].name = k
  def payload(i,old=None):
    out = o(**{k:(i.things[k].init) for k in i.order})
    if old:
      for k,v in old.items():
        out[k] = i.things[k].restrain(v)
    return out
  def asList(i,d):
    return [d[k] for k in i.order]
```

### 1.3 Model

In order to "run" a model for `tmax` iterations, we define a class called `Model`

Essentially, `Model`s run `Things` from time 0 to `tmax` (defaults to 30).
At each step

- We build a new payload which is updated
with the current contents of working memory.
- We print the current contents of working memory, in the
  right order.
  
 ```python
 class Model:
  def __init__(i, params):
    i.params = params

  def step(i):
    raise NotImplementedError(
             '"step" must be implemented in subclass')
  def have(i):
    raise NotImplementedError(
             '"have" must be implemented in subclass')
  def run(i, dt=1, tmax=30, print_head=True, verbose=False):
    have = i.have()
    t,b4  = 0, i.have().payload()
    head = ['?t']  
    for col in i.have().order:
      if col == "d":
        head += [">d"]
      elif col == 'ep' or col == "np":
        head += ["<"+col]
      else:
        head += ["$"+col]

    # Print the title of the table
    if print_head:
      say(head)

    while t < tmax:
      now = i.have().payload(b4)
      i.step(dt,t,b4,now)
      vals = [t] + i.have().asList(now)
      t += dt
      b4 = now
      if verbose:
        say(list(map(lambda x: round(x, 2), vals)))
      # Print the last evaluated column after running
      # the model
      
      if t == tmax:
        say(list(map(lambda x: round(x, 2), vals)))
  ```
