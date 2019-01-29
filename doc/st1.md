
<a href="http://tiny.cc/plm19">home</a> ::
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> ::
<a href="https://github.com/txt/plm19/tree/master/src">src</a> ::
<a href="http://tiny.cc/plm19give">submit</a> ::
<a href="https://plm19.slack.com/">chat</a> ::
<a href="https://github.com/txt/plm19/blob/master/license.md">&copy;</a> 2019, <a href="http://menzies.us">timm</a>
<br>
<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>

# Smalltalk1


"Smalltalk's the source. It'll change your life. Swear to God" --Point Break (sort of)

- [Smalltalk-80](https://www.youtube.com/watch?v=AuXCc7WSczM)
- [Scientific American, 1977](http://dreammachin.es/Kay_SciAm_77.pdf)

Tips: 

1. Try completing all of   https://people.eecs.berkeley.edu/~fateman/264/papers/smalltalk-tutorial.html 
2. Use me as a method lookup (I'm expecting lots of texts in our `how2smalltalk`
   channel).
3. Browse the source code 
      - cd /usr/share/gnu-smalltalk/kernel/
      - methods are indendefed four characters from start of line `grep '^     x:' *`      
      - but do NOT rewrite that code

## Basics

- self  =pointer to self
- super =pointer to superclass
- true  =Only intance of class True
- false  =Only intance of class False
- nil    =Only instance of class UndefinedObject
- Smalltalk =global, holds all globals; e.g. classes
- #abc =symbol
- $x =character
- "  =comments
- ' =strings
     - BTW, strings are Collections of characters
- . =end statement
- ^ =return
- := =assignment
- blocks (of code)
     - [] =block
     - [:x| ... ] =block with one argument
     - basis of all control, loops
     - [e.g whileTrue:](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/BlkClosure.st#L346-L354)
- #() =array
- : =keyword argument
    - `5 between: 3 and: 8`  = selector `between:and:`

## Moving on

- Novel precedence: unary, binary, keywords, brackets
    - _Binary are one,two letter selectors for maths,
      equality, etc.
    - So `1 + 8/4` is... surprising
- Postfix messages
    -  1 negated
    - -1 negated
    -  false not
    -  false not not
    -  -1 abs
    -  1 abs
    -  10 factorial
    -  10 factorial sqrt
    -  5 sqrt
    -  1 isNumber
    -  $a isNumber
    -  $a isNumber not
    -  1 isCharacter
    -  $a isCharacter
    -  'someString' first
    -  'hello world' size
    -  'hello world' asUppercase
    -  'hello world' copy
    -  'hello world' copy sort
    -  #( 17 99 1 57 13) copy sort
    -  1 class name
    -  1 class name asUppercase


## Blocks

- Blocks. `aBlock value: x`.
     - e.g. `1 to: 10 by:2 do: [:x| x oo]`
     - [Number.st](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/Number.st#L982-L999)


## Polymorphism

- Polymorphism: decentralized control
    - e.g. [ifTrue:](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/True.st#L60-L71)

E.g.

    $ cd /usr/share/gnu-smalltalk/kernel/
    $ grep  '^    =='  *.st
    Object.st:    == arg [
    SmallInt.st:    == arg [
    
    $ grep '^    = ' *.st
    AnsiDates.st:    = aDateTime [
    Association.st:    = anAssociation [
    Bag.st:    = aBag [
    BindingDict.st:    = arg [
    ByteArray.st:    = aCollection [
    Character.st:    = char [
    CharArray.st:    = aString [
    Class.st:    = aClass [
    CObject.st:    = anObject [
    CompildCode.st:    = aMethod [
    CompildMeth.st:    = aMethod [
    CompiledBlk.st:    = aMethod [
    CType.st:    = anObject [
    CType.st:    = anObject [
    CType.st:    = anObject [
    Date.st:    = aDate [
    Delay.st:    = aDelay [
    Dictionary.st:    = aDictionary [
    ExcHandling.st:    = anObject [
    FileSegment.st:    = aFileSegment [
    File.st:    = aFile [
    FloatD.st:    = arg [
    FloatE.st:    = arg [
    FloatQ.st:    = arg [
    Fraction.st:    = arg [
    HashedColl.st:    = aHashedCollection [
    Interval.st:    = anInterval [
    LargeInt.st:    = aNumber [
    LookupKey.st:    = aLookupKey [
    Magnitude.st:    = aMagnitude [
    MethodInfo.st:    = aMethodInfo [
    Object.st:    = arg [
    OtherArrays.st:    = anObject [
    OtherArrays.st:    = aLargeArray [
    Point.st:    = aPoint [
    Rectangle.st:    = aRectangle [
    RunArray.st:    = anObject [
    ScaledDec.st:    = arg [
    SeqCollect.st:    = aCollection [
    SmallInt.st:    = arg [
    String.st:    = aCollection [
    Symbol.st:    = aSymbol [
    Time.st:    = aTime [
    URL.st:    = anURL [
    VFS.st:    = aFile [
    VFS.st:    = aFile [


E.g. [Point =](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/Point.st#L198-L203) 

## Everything is an object, even a class.

- Point is an instance of `Class`
- Point class methods are managers of Point instances
    - e.g. Point `x: 10 y:10`
    - class message to Point that returns an instance
      with `x=10,y= 20`

After that, you got to understand the class Hierarchy.
Literally, everything is an object.
    
    Object
        Behavior
          ClassDescription
            Class -- and Class is instance of Metaclass
            Metaclass
        BlockClosure -- []
        Boolean
          False -- true
          True  -- false
        CObject
          -- C stuff
        Collection
          Bag
          MappedCollection
          SequenceableCollection
            ArrayedCollection
              Array
              Interval
              CharacterArray
                String
                  Symbol
            LinkedList
              Semaphore
            OrderedCollection
              SortedCollection
          HashedCollection
            Dictionary
              IdentityDictionary
              RootNamespace
                SystemDictionary
            Set
              IdentitySet
        File
          Directory
        Magnitude
          Association
          Character
          Date
          Number
            Float
            Fraction
            Integer
              SmallInteger
          Time
        Message
          DirectedMessage
        Point
        Rectangle
        Signal -- exception handling. see on:do:
          Exception
            Error
              Halt
                ArithmeticError
                  ZeroDivide
                MessageNotUnderstood
              UserBreak
            Notification
              Warning
        Stream
          PositionableStream
            ReadStream
            WriteStream
              ReadWriteStream -- why not under ReadStream?
                ByteStream
                  FileStream
          Random
        UndefinedObject -- nil
   
\* [on:do:](https://www.gnu.org/software/smalltalk/manual/html_node/Handling-exceptions.html#Handling-exceptions)
 
