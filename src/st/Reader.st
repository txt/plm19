!Reader class methods !
new 
    ^super new init  
!!
! Reader  methods !
init
	line := 0.
	self handler: Tag;
   	     commentChar: $#.
!
cantDo: this
	^self error: ('line ',line s,') cannot handle [',this,']')
!
commentChar: aChar
	commentChar := aChar
!
handler: aRootClass
	 handler := aRootClass
!
handlerOf: aString 
    ^(handler handlerOf: aString) ifNil: [^self cantDo: aString]
!
line: string for: this
    |klass|
    klass := self handlerOf: string.
    ^klass new  line: string for: this at: line
!
lines: aFileName for: this
	line := 0.
	aFileName fileLinesDo: [:words|
		line := line + 1.
		words size > 1 ifTrue: [ 
			self line: words for: this]] !
!!
