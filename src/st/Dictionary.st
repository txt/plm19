! Dictionary methodsFor: 'accessing' !
pairs: aBlock
	self associationsDo: [:assoc|
		aBlock value: assoc key value: assoc value]
!!
