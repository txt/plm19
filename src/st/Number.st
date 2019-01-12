! Number methodsFor: 'printing' !
chars: c
	|s|
	s := WriteStream on: ''.
	self chars: c on: s.
	^s contents
!
chars: c on: aStream
	self timesRepeat: [aStream nextPut: c].
!!
