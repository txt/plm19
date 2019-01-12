! Stagger class methodsFor: 'creation' !
new
	^super new init: OrderedCollection new.
!
new: list
	^super new init: list 
!!

"========================================"
! Stagger methodsFor: 'creation' !
init: aList 
	contents := aList asOrderedCollection.
	contents shuffle.
 	pointer := 1	
!!

"========================================"
! Stagger methodsFor: 'accessing' !
contents
	^contents
!!

"========================================"
! Stagger methodsFor: 'storing' !
add: item
	contents add: item
!!

"========================================"
! Stagger methodsFor: 'enumeration' !
do: aBlock
	contents size timesRepeat: [ 
		aBlock value: self next ].
!
once: aBlock
	aBlock value: self next.
!
next
	pointer >= contents size 
		ifTrue: [ self reset ].
	pointer := pointer + 1.
	^contents at: pointer.
!
reset
	pointer := 0.
	contents shuffle.
!!
