! State class methodsFor: 'coercing' !
from: aSymbol
	^(self nameToClass: aSymbol) new: aSymbol.
!
nameToClass: aSymbol
	aSymbol s ~ 'X$' ifTrue: [^ErrorState].
	aSymbol s ~ 'L$' ifTrue: [^LoopState].
	aSymbol s ~ 'F$' ifTrue: [^EndState].
	aSymbol s ~ 'HaHa$' ifTrue: [^Happy].
	^NormalState
!!

"============================================"
! State class methodsFor: 'creation' !
new: aSymbol
	^super new init: aSymbol
!!

"============================================"
! State methodsFor: 'creation' !
init: aSymbol
	name := aSymbol.
	self reset.
	id := App nextId.
	froms := tos := 0
!
oneMoreFrom
	froms := froms + 1.
!
oneMoreTo
	tos := tos + 1.
!!

"============================================"
! State methodsFor: 'comparing' !
= aState
	^name = aState name
!!

"============================================"
! State methodsFor: 'accessing' !
factory
	^container factory
!
machine: aMachine
	container := aMachine
!
machine
	^container
!!

"============================================"
! State methodsFor: 'simulation' !
onEntry
	visits := visits + 1.
	self tooManyLoops
!
onExit
	^nil
!
onStay
	^nil
!
reset
	visits := 0
!
start
	self onEntry.
!
stopped
	^false
!	
tooManyLoops
	visits > App tooManyLoops
		 ifTrue: [self error: 'too many loops']
!!

"============================================"
! State methodsFor: 'printing' !
printOn: aStream
	aStream nextPutAll: self class name;
			nextPut: $(.
	aStream	nextPutAll: name;
			nextPut: $#.
	id printOn: aStream. 
	aStream	nextPut: $)
!

"============================================"
! LoopState methods !
tooManyLoops
	^false
!!

! EndState methods !
stopped
	^true
!
onEntry
	'that''s all folks!' oo
!!

! Happy methods !
onEntry
	'ha ha ha ha ha' noo.
!
onStay
	'still laughing' oo.	
!
onExit
	'the fun stops here' oo.
!!
