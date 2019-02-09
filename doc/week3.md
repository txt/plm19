


<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week3

## Things to know

- "CoffeeScript is a transpiler language." Explain.

- List 2 advantages and disadvantages of writing languages using the following. For each item
write at least 5 lines describe the thing before talking of its advantages and disadvantages
     - precedence-drive grammars
     - interpreters in Prolog
     - macros
     - transpilers
 
- LLVM: what are two examples (each) for the following LLVM passes. For each example,
  describe what it is and why it might be useful
     - analysis
     - transformations
     - utilities

- If I set any of the Smalltalk homework questions, could you answer them in an exam?
- Write a Smalltalk object method for collecting every second item in a `Collection`.
     - Should that method be in `Collection`? Discuss.
- The following question using the Smalltalk hierarchy, described below:
     - Define a String method for testing if a `String` is a palindrome (i.e. you get
       the same string if you write it backwards and forwards).
     - Discuss the merits, if any of moving that method down into `Symbol`.
     - Discuss the merits, if any of moving that method down to `Bag`.
     - Discuss the merits, if any of moving that method down to `Collection`.
     - Overall, where do you think that method should live?

```
Object
    Behavior
      ClassDescription
        Class -- and Class is instance of Metaclass
        Metaclass
    BlockClosure -- []
    Boolean
      False -- false
      True  -- true
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
            SystemDictionary -- Smalltalk
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
```

