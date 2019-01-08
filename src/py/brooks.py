from __future__ import division,print_function
import sys
sys.dont_write_bytecode = True

from flow import *

def brookesLaw():
  m = model(requirements=stock,software=stock,
            newbies=stock,guru=stock,
            devrate=flow)
  assimimationRate = 1.2
  normalProducitivity = 1.3
  for t,dt,x in steps(m):
    x.devrate = normProductivity * (1 - communicationOverhead/100) * (0.8*x.newbies + 1.2*(gurus - trainers)
    x.software = x.software + x.devrate * x.dt
    x.gurus = x.gurus + x.assimilationRateationRage
                                                                        
    print t,x.a

brookesLaw()
