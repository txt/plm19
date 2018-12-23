#from __future__ import print_function, division
import random
import sys
import traceback

def ok(*lst):
  print("### ",lst[0].__name__)
  for one in lst: unittest(one)
  return one

class unittest:
  tries = fails = 0  #  tracks the record so far
  @staticmethod
  def score():
    t = unittest.tries
    f = unittest.fails
    return "# TRIES= %s FAIL= %s %%PASS = %s%%"  % (
      t,f,int(round(t*100/(t+f+0.001))))
  def __init__(i,test):
    unittest.tries += 1
    try:
      test()
    except Exception as e:
      unittest.fails += 1
      i.report(test)
  def report(i,test):
    print(traceback.format_exc())
    print(unittest.score(),':',test.__name__)

r   = random.random
isa = isinstance
sys.dont_write_bytecode = True
same = lambda x:x

def order(x,key=lambda z:z):
  sorted(x, key=key)
  return x

class o:
  """Emulate Javascript's uber simple objects."""
#  def has(i)             : return i.__dict__
#  def keys(i)            : return i.has().keys()
#  def items(i)           : return i.has().items()
#  def __init__(i,**d)    : i.has().update(d)
#  def __setitem__(i,k,v) : i.has()[k] = v
#  def __getitem__(i,k)   : return i.has()[k]
#  def __repr__(i)        : return 'o'+str(i.has())
#
#
  def items(i)           : return i.__dict__.items()
  def keys(i)            : return sorted([k for k in order(i.__dict__.keys())])
  def __init__(i,**d)    : i.__dict__.update(d)
  def __setitem__(i,k,v) : print(k); i.__dict__[k] = v; print(20)
  def __getitem__(i,k)   : return i.__dict__[k]
  def __repr__(i)        : return i.__class__.__name__+str(
                                  [i[k] for k in i.keys() if not k[0] == "_"])

#class payload(o):
#  def __init__(i,**d):
#    i.__dict__.update(d)
#    for k,v in d.items(): 
#      v.name = k
#  def keys(i):
#    return [k for k in sorted(i.__dict__.keys(), key=lambda z:i[z].rank())]
#  def slots(i,old=dict()):
#    out = o({k:i[k].init for k in i.keys()})
#    for k,v in old.items():
#        out[k] = v
#    return out
#
#vrry round and old and enw item

# we coudl the meta knowledge
# need to keys in sorted rank order
# need values
class Have:
  def __init__(i,**d):
    i.__dict__.update(d)
    i._keys = [k for k in sorted(d.keys(), key=lambda z:d[z].rank())]
    for pos,k in enumerate(i.keys()):
        d[k].pos = pos
        d[k].value= d[k].init
  def keys(i)  : return i._keys
  def values(i): return i._values
  def clone(i,inits=None):
    out = Have({k:v.clone() for k,v in i.items})
    for k,v in inits.items():
        out[k] = v # but not reallly

  def __setitem__(i,k,v): 
     i[k].value = i[k].restrain(v)

class Has:
  def __init__(i,init=None,lo=0,hi=100):
    i.init = init if init != None else lo
    i.lo,i.hi = lo,hi
    i.pos=0
  def restrain(i,x):
    return max(i.lo, min(i.hi, x))
  def rank(i): 
    return 0
  def clone(i):
    return i.__class__(init=i.init,lo=i.lo,hi=i.hi)
  def __repr__(i):
    return str(dict(what=i.__class__.__name__,
                    name= i.name,init= i.init,
                    lo  = i.lo,  hi  = i.hi))

class Flow(Has) : 
  def rank(i): return 3
class Stock(Has): 
  def rank(i): return 1
class Aux(Has)  : 
  def rank(i): return 2

S,A,F = Stock,Aux,Flow

@ok
def _has():
  g=Have(dd=F(100),cc=F(20), aa=S(10), bb=A(100))
  print(g.keys())
  print(g.values())

sys.exit()

class Model:
  def step(i):
    raise NotImplementedError('"step" must be implemented in subclass')
  def have(i):
    raise NotImplementedError('"have" must be implemented in subclass')
  def run(i,dt=1,tmax=30):
    have = i.have()
    t,b4  = 0, i.values()
    print(have.keys())
    while t < tmax:
      now = i.have.slots(b4)
      i.step(dt,t,b4,now)
      print(now)
      t += dt
      b4 = now

class Diapers(Model):
  def have(i):
    return payload(C = S(100), D = S(0),
                  q = F(0),  r = F(8), s = F(0))
  def step(i,dt,t,u,v):
    def saturday(x): return int(x) % 7 == 6
    v.C +=  dt*(u.q - u.r)
    v.D +=  dt*(u.r - u.s)
    v.q  =  70  if saturday(t) else 0 
    v.s  =  u.D if saturday(t) else 0
    if t == 27: # special case (the day i forget)
      v.s = 0

@ok
def _diapers1():
  printm(Diapers().run())

