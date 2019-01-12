! Transition methodsFor: 'creation'!
init
	id  := App nextId.
	if   := [true].
	then := [true].
!!

"========================================"
! Transition methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: #Transition;
			nextPut: $(;
			nextPutAll: 'from: '.
	state printOn: aStream.
    aStream	nextPutAll: ' to: '.
	next printOn: aStream.
	aStream nextPut: $)
!!


"========================================"
! Transition methodsFor: 'simulation'!
fire
	then value.
	^next
!
ready
	^if value.
!
start
	state onEntry
!
staysAt: here
	state := here.
	next := here.
!!

"========================================"
! Transition methodsFor: 'accessing'!
machine
	^container
!
factory
	^container factory
!!

"========================================"
! Transition methodsFor: 'storing'!
machine: aMachine 
	container := aMachine.
	state := aMachine addState: state.
	next  := aMachine addState: next.
	state oneMoreFrom.
	next  oneMoreTo.
!!

