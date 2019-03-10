<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>
'

# Notes on Project 2

Your mission is to replace N part of my data mining
pipeline with machines of your own making.

For each part replaced, the score will be:

- 10 marks (if there is an initial planning doc, a working
  implementation, and a final epilogue added to the doc)
- plus, at max up to 2 marks bonus for coding the "hard" filter
- plus, at max up to 2 marks bonus for coding in "hard" languages
- plus, at max up to 2 marks bonus for coding in "hard" abstractions


## Language Bonus Marks

Using any one of the following to replace any one of
the elements in my pipe will earn you extra marks.

### One star languages (zero bomus marks)

\* D  
\* Dart  
\* Go  
\* Groovy  
\* Java  
\* JavaScript  
\* Lua  
\* Objective-C  
\* PHP  
\* Perl  
\* Python  

### Two star languages (one bonus mark)

\*\* Awk/gawk  
\*\* Bash  
\*\* Fortran  
\*\* Smalltalk (gnu smalltalk, accept no other)
\*\* Kotlin  
\*\* Ruby  
\*\* Rust  
\*\* Swift  

### Three star languages (two bonus marks)

\*\*\* Clojure  
\*\*\* CoffeeScript  
\*\*\* Elixir  
\*\*\* Elm   
\*\*\* Erlang  
\*\*\* Forth  
\*\*\* Haskell  
\*\*\* Julia  
\*\*\* Lisp  
\*\*\* ML  
\*\*\* Prolog  
\*\*\* Scala  
\*\*\* Scheme  
\*\*\* TypeScript  

## Abstraction bonus marks

Using any one of the following to replace any one of
the elements in my pipe will earn you extra marks.


Making extensive use of any one following will earn you **zero**
extra mark:

**Zero** bonus marks:

- Inheritance
- Polymorphism

**One** bonus mark:

- Layers (need at least 3 to qualify)
- Pattern matching (replacing large hunks of procedural code
  with declarative pattern matching)
- Pipe and Filter (not my code but some elaborate filter 
  system of your own creation)
- State machines
- Compartmental models
- 

**Two** bonus marks:

- Macros (in Lisp or Julia or Elixr or some other language
  that I must pre-approve) to write your won domain specific
  language.
- Interpreter (i.e. right your own specialized domain specific
  language for that task)
- Lambda calculus (so not just using LISP but going nuts
  on set operatiors implemeted as lambdas mapped over lists)
- Rule-based programming (lots of match-select-act)
- Blackboards (agents watching a shared space, trigger on
  conclusions of lower-level agents, writing results to
  higher levels)


- 2a : Given a data mining pipeline (from the lecturer), replace any one parts of the pipe using
	  a different programming language. 
     - FIRST, write a reporting ranking ten abstractions you are thinking
	  of trying for that code (where  list includes a short description of each AND a tiny example where that 
	  abstraction might be useful).  For a list of abstractions, see below.
     - SECOND, write working code that replicates the i/o of that part of pipe.
	  That code should include your attempt to use your top three ranked abstractions (and it is expected
	  that you some of your planned abstractions will prove useless). 
     - THIRD add to the report an epilogue
	  describing your experience with the abstractions AND your recommendations to other people about
	  when to use/to avoid those abstractions. 
     - FOURTH add an end section describing what maximum grades you expect for this section (see below _bonus marks_).
     - FIFTH in some public Github repo (not from NCSU) write
	  a sub-directory called "_2a_". Add to that directory your report in pdf format  (I expect 5 pages (no less or more),
	  [2 column conference format](https://www.overleaf.com/gallery/tagged/acm-official#.WOuOk2e1taQ) AND
	  your working code AND a file canned "run" (that graders will run) AND a text file "run.out" showing the input and output when you run the code.
- 2b : as per 2a but use a different language, and a different part of the pipe and store the outputs to
         a directory "_2b".
- 2c : Grading someone else's group 2 code (not the written report, just checking it works as advertised).
- 2d : bonus marks. as per 2c. Only if allowed by professor. And
   this bonus mark is due same time as 2ab.

List of abstractions

1. See class
2. See the design patters list
3. From the Lopez book
4. From the Fowler book.


