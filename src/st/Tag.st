"XXX find an implement any empty methods in the following"
!Tag class methods!
handles
^'someImpossibBle STRing'
!
handlerOf: line
self allSubclasses do: [:class|
line ~ class handles ifTrue: [^class]].
^nil "XXX once TagError is running, get rid of this return"
!!
!Tag class methods !
handles
^'^\s*\[.+\]\s*$'
!
line: string for: this at: line
this block: (Behavior evaluate: string )
!!
! TagWidth class methods !
handles
^'^\s*width\s+(\d+)\s*$'
!!

! TagWidth methods !
line: string for: graph at: line
graph width: string asWords second asInteger.
"if we expect a float, use Float readFrom: self"
! !


