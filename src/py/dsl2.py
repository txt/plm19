#!/usr/bin/env python3

"""
## Utilities

Printing stuff.
"""

def say(lst):
  print(', '.join([str(x) for x in lst]))

"""
### Associative Arrays

Associative arrays in Python. After creating an `o` with

    x=o(bb=2, aa=0, _cc=2)

then `x`'s fields are avaialble via the _dot_ notation

    print(x.aa) # --> 0
    x.aa += 10  # adds 10
    print(x.aa) # --> 10

Also, since this class includes magic methods
for `__setitem__` and `__getitem__`, we can use
symbolic access:

    print(x["aa"])
    x["aa"] *= 10
    print(x.aa) # --> 100

Also `o` objects know how to print themselves in 
alphabetical order of their keys.  Further, they don;t
print "secret" keys (those that start with "`_`".; e.g
`_cc` in the above). For example, continuing with
the above value of `x`:

    print(x) ==> o(aa=100, bb=2)

"""

class o:
  def __init__(i,**d): 
    i.__dict__.update(d)
  def items(i): 
    return i.__dict__.items()
  def keys(i): 
    return sorted(list(i.__dict__.keys()))
  def __setitem__(i,k,v):
    i.__dict__[k] = v
  def __getitem__(i,k): 
    return i.__dict__[k]
  def __repr__(i): 
    tmp = ['%s=%s' % (k, i[k]) 
           for k in i.keys() if k[0]  != "_"]
    return 'o('+', '.join(tmp) + ')'

"""
## `Stock`s, `Flow`, `Aux`illary Variables

In compartmental modelling:
    
- `Stock`s store quantities;
- `Flow`s dribble quanities between `Stock`s;
- `Aux`illary variabes contain business logic.

For our purposes, all of these are nearly the same so we represent
them all as `Thing`s. `Stock, Flow, Aux` are subclasses of `Thing`.
All these `Thing`s have different `rank`s (so when we print a
list of `Thing`s, we can sort them out
into all the `Stock`s, before all the `Aux`s before all the 
`Flow`s.

Also, `Things` restrain themselves to always be between 
a `lo` and `hi` value.

"""

class Thing:
  def __init__(i,init=None,lo=0,hi=100):
    i.init = init if init != None else lo
    i.lo,i.hi = lo,hi
    i.name = 0,''
  def restrain(i,x):
    return max(i.lo, min(i.hi, x))
  def rank(i):
    raise NotImplementedError(
             '"rank" must be implemented in subclass')

class Flow(Thing) : 
  def rank(i): return 3
class Stock(Thing): 
  def rank(i): return 1
class Aux(Thing)  : 
  def rank(i): return 2

# Here some short hand for quickly specifying `Stock`s,`Aux`s, `Flow`s.

S,A,F = Stock,Aux,Flow

"""
`Things` stores out meta-knowledge about variables in a model.
The call
Have(C = S(100), D = S(0),
                q = F(0),  r = F(8), s = F(0))

"""
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

class Model:
  def step(i):
    raise NotImplementedError(
             '"step" must be implemented in subclass')
  def have(i):
    raise NotImplementedError(
             '"have" must be implemented in subclass')
  def run(i,dt=1,tmax=30):
    have = i.have()
    t,b4  = 0, i.have().payload()
    say( ['t'] + i.have().order )
    while t < tmax:
      now = i.have().payload(b4)
      i.step(dt,t,b4,now)
      say( [t] + i.have().asList(now) )
      t += dt
      b4 = now

class Diapers(Model):
  def have(i):
    return Things(C = S(100), D = S(0),
                  q = F(0),  r = F(8), s = F(0))
  def step(i,dt,t,u,v):
    def saturday(x): return int(x) % 7 == 6
    v.C +=  dt*(u.q - u.r)
    v.D +=  dt*(u.r - u.s)
    v.q  =  70  if saturday(t) else 0 
    v.s  =  u.D if saturday(t) else 0
    if t == 27: # special case (the day i forget)
      v.s = 0

if __name__ == "__main__":
    Diapers().run()

