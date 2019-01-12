! Manager methods !
init: seed0
	seed 	:= seed0.
	roles   := IdentityDictionary new.
	workers := OrderedCollection new.
	rand 	:= Random seed: seed
!
worker: aWorker
	"Tell this manager/worker that they work together."
	workers add: aWorker.
	aWorker manager: self.
!
workers 	^workers !
reset 		self init: seed.  !
rand 		^rand next !
!

! Manager class methods !
last
	^Last
!
new 			
	^self new: Random next 
!
new: seed 		
	Last := super new init: seed  .
	^Last
!!
