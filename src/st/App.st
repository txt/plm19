"=============================================="
! App class methodsFor: 'variables' !
asynchronousOn
    Asynchronousp  := true
!
asynchronousOff
    Asynchronousp  := false
!
clock
	^Clock
!
dawnOfTime
	Clock := 0
!
impatient
	^(Patience := Patience - 1 ) < 0  
!
nextId
	Ids ifNil:[ Ids := 0].
	Ids := Ids + 1.
	^Ids.
!
patience: anInteger
	Patience := anInteger
!
random
	Rand ifNil: [ Rand := self seed].
	^Rand
!  
seed
	^self seed: 1
!
seed: seed
	^(Rand := Random new seed: seed )
!
silent
	Trace := false 
!
tick
	^Clock := Clock + 1
!
tooManyLoops
	^TooManyLoops
!
tooManyLoops: anInteger
	^TooManyLoops := anInteger.
!
verbose
	Trace := true 
!!

"=============================================="
! App class methodsFor: 'creation' !
ready
	^self reset.
!
reset
	^self reset: 1 maxLoops: 250
!
resetAny
	^self reset: (Random next*100000) asInteger maxLoops: 250
!
reset: seed maxLoops: n
	self 	dawnOfTime;
			tooManyLoops: n;
			seed: seed;
			asynchronousOff;
			patience: 50;
			silent 
!!

"=============================================="
! App class methodsFor: 'printing' !
say: message
	Trace ifTrue: [('Actory> ' s, message ) oo] 
!!
