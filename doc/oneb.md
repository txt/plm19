# 1b (Smalltalk).

Here are four marks of homework. 
You only need to get 3 marks for full amrks (so 1 is extra).

You will need [my Smalltalk tools](my.st).

## 1b1. Number collector (0.5 marks)
    
Write a `Magic` subclass called `Num` that is
equivalent to the following.

     function num(txt)  
         return {n=0, mu=0, m2=0, sd=0, id = id(), 
                 lo=10^32, hi=-1*10^32, txt=txt,
                 w=1}
     end

     function numInc(t,x,    d) 
       if x == "?" then return x end
       t.n  = t.n + 1
       d    = x - t.mu
       t.mu = t.mu + d/t.n
       t.m2 = t.m2 + d*(x - t.mu)
       if x > t.hi then t.hi = x end
       if x < t.lo then t.lo = x end
       if (t.n>=2) then 
         t.sd = (t.m2/(t.n - 1 + 10^-32))^0.5 end
       return x  
     end
 
     function numDec(t,x,    d) 
       if (x == "?") then return x end
       if (t.n == 1) then return x end
       t.n  = t.n - 1
       d    = x - t.mu
       t.mu = t.mu - d/t.n
       t.m2 = t.m2 - d*(x- t.mu)
       if (t.n>=2) then
         t.sd = (t.m2/(t.n - 1 + 10^-32))^0.5 end
       return x
     end
 
When that works, it should do the following:

     FileStream fileIn: 'my.st' "mytricks" !
     FileStream fileIn: 'num.st' "yourcode" !

     | num |
     num := Num new.
     num nextPutAll: #( 2 3 4 4 4 4 5 5 6 7 7
                      8 9 9 9 9 10 11 12 12).
     num sd oo. "==> 3.06"
     num mu oo. "==> 7"
     num n  oo. "==> 20" !

## 1b2. Iterators (0.5 marks)

Go to /usr/share/gnu-smalltak/kernel and browse the
`Collection.st` class. Reflect on the methods
`reject:` and `select:` methods. 

Write your own `reject:`
method called `eject:` that does the same thing as 
`reject:` but do so calling `select`.

e.g.

     #(1 2 3) eject: [:x | x > 1.5]

     (1)

IMPORTANT: do not edit anything in that kernel directory.

HINT: the solution is two lines long.

## 1b3. Iterators (0.5 marks)

Write an iterator `b4Now:` that pass the i-th 
and (i+1)-th item to a alist:

e.g.

    FileStream fileIn: 'my.st' !

    #(10 21 32 43 54) b4Now: [:b4 :now|
         ((now-b4)/b4) asFloat oo] !x!
    
    1.1
    0.5238095238095238
    0.34375
    0.2558139534883721

## 1b4. Polymorphism (1.5 marks)

My `Magic` class has a method called `visit` that walks
a block across all instance variables. Write more
methods (not in `Magic`) such that you can write a generic
visit that walks across anything in Smalltalk (exception,
the `C*` classes that talk to ``C`` structs.

    Magic sub: #Employee has: 'name age shoesize'
    
    ! Employee methods !
    init
      self name: 'freda';
           age:  21;
           shoesize:  0 !
    
    printOn: aStream
      aStream 
         nextPutAll: 'Emp(';
         nextPutAll: ':name ',name s;
         nextPutAll: ' :age ',age s;
         nextPutAll: ' :shoesize ',shoesize s;
         nextPutAll: ')' !
    !
    
    ! Magic methods !
    visit: aBlock
      "To heck with encapulation. Walk over the instance vars."
      | num |
      num := self class instSize + self basicSize.
      1 to: num do: [:i | 
         (self instVarAt: i) visit: aBlock ] !
    !
    
    ! Object class methodsFor: 'testing' !
    goodVisit
        |x y z w|
        x := (Employee new)  name: 'tammy'.
        y := (Employee new)  name: 'tammy'.
        z := (Employee new)  name: 'Huy'; age: 18.
        w := {1. 2. #abc z. {x. x. x. {y.}.}. 4. {{{5.}.}.}.}.
        w visit:[:a| a oo] !
    !

Output:

    1
    2
    #abc
    'Huy'
    18
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    'tammy'
    21
    0
    4
    5    
        
Important: make sure `#abc` is printed as `#abc` and not:

    $a
    $b
    $c

To test this one, run

    FileStream fileIn: 'my.st'.
    FileStream fileIn: 'yourcode.st'.
    Object goodVisit.

Then you should see something like the above (and don't worry
if you see stuff that is a little difference).

## 1b5.  1 marks.

This is the extra mark one. Its a little tricky since you'll need
to hunt around looking for some magic methods.

For each of the following, include a litte test script showing off what can be done.


My file my.st hows an exampe of class methods contain tests in the
category 'testing'.  Write some test cases for your classes using the
same category (and call all those methods `goodX`, just like I did). Write
Smalltalk code to

- iterate through all classes (hint `allSubclasses`)
- iterate through all methods of class methods categories called 'testing' 
- run all class methods in category `testing` (hint `methodDictionary` and `perform:`)
  count how pass,fails you get
- run methods such that if any one test method fails, the result gets recorded as "fail"
  and the test carries on to the next method. 
- Hint: see [Gnu Smaltalk manual](https://www.gnu.org/software/smalltalk/manual/html_node/Handling-exceptions.html)
      and `on:do:`.
