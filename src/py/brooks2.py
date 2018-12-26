#!/usr/bin/env python3

from  dsl2 import Model,Things,S,A,F, Stock,Flow,Percent,Aux

class BrooksLaw(Model):
  def have(i): return Things(
    aR    = Flow( "assimilationRate"),
    co    = Percent(  "communicationOverhead"),
    d     = Stock("developedSoftware",0),
    ep    = Stock("experiencedPeople",30),
    ept   = Aux(  "experiencedPeopleNeeded2Train"),
    nprod = Aux(  "nominalProductity",0.1),
    np    = Stock("newPersonnel",0),
    paR   = Flow( "personnelAllocationRate"),
    ps    = Aux(  "plannedSoftware"),
    sdR   = Flow( "softwareDevelopmentRate"),
    ts    = Aux("teamSize",5),
    to    = Percent( "trainingOverhead",25), # one-quarter of an experienced
                                          # person's time is needed to
                                          # train a new person until
                                          # he/she is fully assimilated.
    r     = Stock("requirements",500))

  def step(self,dt,t,i,j):
    def _co(x):
      myTeam = i.ts - 1   # talk to everyone in my team
      others = x/i.ts - 1 # talk to every other team
      return 0.06*(myTeam**2 + others**2)
    j.aR  = i.np/20
    j.ps  = 2.5*t
    j.co  = _co(i.ep + i.np)
    j.paR = 6 if  (i.d - i.ps) < 75 and t < 60 else 0
    j.sdR = i.nprod*(1-i.co/100)*(0.74*i.np+1.28*(i.ep - i.ept))
    j.ept = i.np*i.to /100
    j.ep += i.aR*dt
    j.np += (i.paR - i.aR)*dt
    j.d  += i.sdR*dt
    j.r  +=  - i.sdR*dt

if __name__ == "__main__":
    BrooksLaw().run(tmax=70)
