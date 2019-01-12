";;;;set global flags !"
ObjectMemory gcMessage: false 
!

";;;; convenint get/set creation "
! Class methods !
inits
	self class compile: 'new [^super new init]'
!
public
	|getter setter|
	self instanceVariableString asWords do: [:what |
		getter := '%1 [
			"Answer the receiver''s %1."
			^%1 ]' % {what} .
	    setter := '%1: aValue [
			"Set the receiver''s %1."
			%1 := aValue ]' % {what} .
		self compile: getter; 
             compile: setter.
	]
!!

";;;; convenient sub-classing"
! Symbol methods !
isa:  dad
     ^dad subclass: self       instanceVariableNames: ''
          classVariableNames: '' poolDictionaries: ''
          category: nil !
isa: dad with: vars
     ^dad subclass: self       instanceVariableNames: vars
          classVariableNames: '' poolDictionaries: ''
          category: nil !
isa: dad  shares: classVars
     ^dad subclass: self
          instanceVariableNames: '' 
		  classVariableNames: classVars
          poolDictionaries: ''        
	      category: nil 
!
isa: dad with: vars shares: classVars
     ^dad subclass: self
          instanceVariableNames: vars classVariableNames: classVars
          poolDictionaries: ''        category: nil 
!
asObject
	^Smalltalk at: self !!
";;;; convenient printing"
! Object methods ! 
o       self s display   !
no      '' oo. self s display !
oo      self s displayNl !
noo     '' oo. self s displayNl !
loo     '' oo. (60 chars:$_) oo. self s displayNl !
s       ^self displayString !!
 
";;;; collection tricks"
! SequenceableCollection methods !
first: first then: then
         | atFirst |
         atFirst := true .
         self do: [:elm |
               atFirst ifFalse: [then  value: elm]
                       ifTrue:  [first value: elm].
               atFirst := false] !!
 
! Array methods !
printOn: aStream
	 aStream nextPut: $(.
         self first: [:elm| elm printOn: aStream]
              then:  [:elm| aStream space.
                            elm printOn: aStream].
         aStream nextPut: $) !!

";;;; string ticks"
! String methods ! 
asWords  
      ^(self tokenize: '\s+') asOrderedCollection 
!
fileLinesDo: aBlock
    |f last|         
    f := File name: self.
    f readStream linesDo: [:line|
        last := aBlock value: line].
	f readStream close.
    ^last 
!!

";;;; nil tricks"
! UndefinedObject methods !
ifNil:  aBlock 		
	^aBlock value 
!
notNil: aBlock 		
	^self 
!!
! Object methods !
ifNil:  aBlock      
	^self 
!
notNil: aBlock 		
	^aBlock value 
!!
