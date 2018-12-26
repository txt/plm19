#!/usr/bin/env python3

from  dsl2 import Model,Things,S,A,F

class Diapers(Model):
  def have(i):
    return Things(C = S('clean diapers',100), 
                  D = S('dirty diapers', 0),
                  q = F('cleaning rate',0),  
                  r = F('poop rate',8), 
                  s = F('resupply',0))
  def step(i,dt,t,u,v):
    def saturday(x): return int(x) % 7 == 6
    v.C +=  dt*(u.q - u.r)
    v.D +=  dt*(u.r - u.s)
    v.q  =  70  if saturday(t) else 0 
    v.s  =  u.D if saturday(t) else 0
    if t == 27: # special case (the day i forget)
      v.s = 0

if __name__ == "__main__":
    Diapers().run(tmax=70)

