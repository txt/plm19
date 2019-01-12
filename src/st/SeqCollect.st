! SequenceableCollection methodsFor: 'operations' !
shuffle
	^self shuffle: App random
!
shuffle: r
	|n j k tmp| 
	n := self size.
	1 to: n  do: [:i|
		k := r between: 0 and: (n-i).
		j := i + k .
		tmp := self at: j.
		self at: j put: (self at: i).
		self at: i put: tmp 
	]
!!

