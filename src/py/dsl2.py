#from __future__ import print_function, division
import random
import sys
import traceback

r   = random.random
isa = isinstance
sys.dont_write_bytecode = True
same = lambda x:x

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
    except Exception,e:
      unittest.fails += 1
      i.report(test)
  def report(i,test):
    print(traceback.format_exc())
    print(unittest.score(),':',test.__name__)

class o:
  """Emulate Javascript's uber simple objects."""
  def has(i)             : return i.__dict__
  def keys(i)            : return i.has().keys().sort(key=i._order)
  def items(i)           : return i.has().items()
  def __init__(i,_order=same,**d)    : i._order=_order; i.has().update(d)
  def __setitem__(i,k,v) : i.has()[k] = v
  def __getitem__(i,k)   : return i.has()[k]
  def __repr__(i)        : return 'o{'+str(i.asList())+'}'
  def copy(i): 
      j = o()
      for k in i.has(): j[k] = i[k]
      return j
  def asList(i):
    return [i[k] for k in i.keys() if not k[0] =="_"]

@ok
def _o():
  g=o(a=20,z=10,c=100)
  print(g.keys())

class Has:
  def __init__(i,init,lo=0,hi=100):
    i.init,i.lo,i.hi = init,lo,hi
  def restrain(i,x):
    return max(i.lo, min(i.hi, x))
  def rank(i): 
    "Trick to sort together columns of the same type."
    return 0
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

class Model:
  def __init__(i,dt=1,tmax=30):
    i.dt, i.tmax = dt,tmax
    i.b4,i.now = o(),o()
  def step(i):
    raise NotImplementedError('"step" must be implemented in subclass')
  def have(i):
    raise NotImplementedError('"have" must be implemented in subclass')
  def state(i):
    """To create a state vector, we create 
    one slot for each name in 'have'."""
    tmp=i.have()
    for k,v in tmp.has().items():
      v.name = k
    return tmp  
  def run(i,dt=1,tmax=30):
    """For time up to 'tmax', increment 't' 
       by 'dt' and 'step' the model."""
    t,b4 = 0, o()
    keep = []    ## 1
    state = i.state()
    for k,a in state.items(): 
      b4[k] = a.init
    keys  = sorted(state.keys(),  ## 3
                   key=lambda z: state[z].rank())
    keep = [["t"] +  keys,
            [0] + b4.asList(keys)]
    while t < tmax:
      now = b4.copy()
      i.step(dt,t,b4,now)
      for k in state.keys(): 
        now[k] = state[k].restrain(now[k]) ## 4
      keep += [[t] + now.asList(keys)] ## 2
      t += dt
      b4 = now
    return keep

class Diapers(Model):
  def have(i):
    return o(C = S(100), D = S(0),
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

