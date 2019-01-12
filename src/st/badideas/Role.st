! Role class methods !
new		^super new init !!

! Role methods !
init
	name := 'name'.
	eman := 'eman'.
	fromCard := toCard := 0.
	froms := OrderedCollection new.
	tos   := OrderedCollection new.
!
uses: from and: to
	froms addLast: from.
	tos   addLast: to.
!!
