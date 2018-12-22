from __future__ import division,print_function
import sys
sys.dont_write_bytecode = True

class tub:
  id=0
  def __init__(i,name,lo=-10**32,hi=10**32,init=0):
    i.name,i.lo,i.hi,i.init = name,lo,hi,init
    i.log={}
    i.latest=None
  def __iadd__(i,x,t):
    i.latest = i.log[t] = x 
  @staticmethod
  def next():
    tub.id = tub.id + 1
    return tub.id
  def __repr__(i):
    return '1'#'T{%s = %s}' % (i.name,i.latest)
  
def stock(name,hi=10**32,init=2):
  return tub(name,lo=0,hi=max,init=init)

def source(name=None):
  return tub(name or ('source %s' % tub.next()))

def sink(name=None):
  return tub(name or ('sink %s' % tub.next()))

def flow(name=None,init=0):
  return tub(name or ('flow %s' % tub.next()),
             init=init)

class model:
  def __init__(i,**parts):
    for k,v in parts.items():
      if callable(v):
        parts[k] = v(k)
    i.parts=parts

class dot(object):
  @staticmethod
  def set(inst,key,value):
    super(dot, inst).__setattr__(key,value)
  @staticmethod
  def get(inst,key):
    return super(dot,inst).__getattribute__(key)
  def __init__(i,model,t,t1):
    dot.set(i,"model",model)
    dot.set(i,"t",    t)
    dot.set(i,"t1",   t1)
  def __getattribute__(i,key):
    m = dot.get(i, "model")
    t = dot.get(i, "t")
    part = m.parts[key]
    return part.log.get(t,
                        part.init)
  def __setattr__(i,key,val):
    m  = dot.get(i, "model")
    t1 = dot.get(i, "t1")
    part = m.parts[key]
    part.log[t1] = val + part.log.get(t1,
                                      part.init)

print(model(fred=stock, jane=sink).parts)

def steps(m,n=100,inc=1,t=0):
  for _ in xrange(n):
    t1 = t+inc
    yield t,inc,dot(m,t,t1)
    t  = t1

def _main():
  m = model(a=stock,b=stock)
  for t,_,z in steps(m):
    z.a = z.a * z.b
    print(t,z.a)

_main()
