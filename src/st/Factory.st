! Factory class methodsFor: 'creation' !
new
	self error: 'should not create Factories without nameStrings'
!
new: nameString
	^super new init: nameString
!!

"==============================================="
! Factory methodsFor: 'creation' !
  
init: aString 
	machines := Stagger new.
	name := aString
!
go: state1 to: state2                          
	^self go: state1 to: state2 if: [true] then: [true] 
!
go: state1 to: state2 if: gaurd                
	^self go: state1 to: state2 if: gaurd  then: [true] 
!
go: state1 to: state2 then: sideEffect         
	^self go: state1 to: state2 if: [true] then: sideEffect 
!
go: state1 to: state2 then: sideEffect if: gaurd 
	^self go: state1 to: state2 if: gaurd then: sideEffect 
!
go: state1 to: state2 if: gaurd then: sideEffect
	self transition:
		(Transition new
			state:  state1 state;
			if:     gaurd;   
			then:   sideEffect;
			next:   state2 state)
!! 

"==============================================="
! Factory methodsFor: 'storing' !
machine: machine
	current := machine.
	machines add: machine.
	machine factory: self.
!
newMachine: name
	self machine: (Machine new: name).
!
transition: trans
	current add: trans
!!

"==============================================="
! Factory methodsFor: 'printing' !
printOn: aStream
	super printOn: aStream.
	name printOn: aStream
!!

"==============================================="
! Factory methodsFor: 'enumerating' !
do: aBlock
	machines do: [:machine| aBlock value: machine]
!!

"==============================================="
! Factory methodsFor: 'accessing' !
machines
	^machines
!!

"==============================================="
! Factory methodsFor: 'simulation' !
reset
	machines contents do:[:machine | machine reset]
!
report
	'' loo.
	('Terminated at tick ', Clock s) oo.
	('Final patience ', Patience s) oo.
!
run
	self reset; start; walk; report.
!
start
	machines contents do: [:machine| machine start].
!
step
	| more |
	App tick.
	more := false.
	self do: [:machine|
	  machine step 
		ifTrue: [
			more := true.
            Asynchronousp 
		   	   ifTrue: [^true]]].
	^more
!
walk
	App patience: self patience.
	[true] whileTrue: [
		App clock > self tooMuch 
			ifTrue:[ ^true].
		self step 
			ifTrue:  [App patience: self patience]
			ifFalse: [App impatient 
						ifTrue: [^true]]].
!!

"==============================================="
! Factory methodsFor: 'constants' !
patience
	^50
!
tooMuch
	^100
!!
