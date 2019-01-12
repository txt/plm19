| c b|
App reset.
c := Stagger new: #(#see #jane #run #past #the #ball).

3 timesRepeat: [
	10 timesRepeat: [' ' o. c next o].
	'' oo
].
b := Bag new.
10000 timesRepeat: [ b add: c next].
b oo.
!
