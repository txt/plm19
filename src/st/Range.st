"= Range.st =

A range is a summary of a stream of numbers. After reading N numbers,
a range maintains the min/max values seen in those numbers.

== Class Methods =="

!! Range class methods !
new
  "auto-initialize new Ranges"
  ^super new init
!!

"== Instance methods == 
=== Prining ==="

! Range methods !
printOn: aStream
	aStream nextPut: $[.
  	min printOn: aStream.
	aStream nextPutAll: '..'.
	max printOn: aStream.
	aStream nextPut: $].
!
"Min's first value is very large. Max's first value is min * -1"
init
  	min := FloatE fmax.
	max := min negated.
!
add: aNumber
  "take a number, reset max and min if needed"
	self 	min: aNumber; 
			max: aNumber	
!
min: aNumber
  	"take a number, reset min if needed"
	min := min min: aNumber
!
max: aNumber
  	"take a number, reset max if needed"
	max:= max max: aNumber
!
scale: aNumber
  	"scale a number according to where it falls in the range min..max"
  	^(aNumber - min)/(max - min)
!!
