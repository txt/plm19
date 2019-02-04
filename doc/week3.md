
[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
<a href="http://tiny.cc/plm19"><img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>
<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)

# Review, week3

## Things to know

- "CoffeeScript is a transpiler language." Explain.

- List 2 advantages and disadavtanges of writing languages using the following. For each item
write at leat 5 lines describe the thing before talking of its advantas and disavanatgages
     - precdence-drive gramamers
     - interpreters in Prolog
     - macros
     - transpilers
 
- LLVM: what are two examples (each) for the following LLVM passes. For each example,
  describe what it is and why it might be useful
     - analysis
     - transformations
     - utilities

- If I set any of the Smalltalk homework questions, could you answer them in an exam?
- Write a Smalltalk object method for collecting every second item in a `Colelction`.
     - Should that method be in `Collection`? Discuss.
- The following question using the Smalltalk hierarchy, described below:
     - Define a String method for testing if a `String` is a palindrome (i.e. you get
       the same string if you write it backwards and forwards).
     - Discuss the merits, if any of moving that method down into `Symbol`.
     - Discuss the merits, if any of moving that method down to `Bag`.
     - Discuss the merits, if any of moving that method down to `Collection`.
     - OVerall, where do you think that method should live?

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

