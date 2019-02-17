<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>

<img width=900 src="http://www.ecorestrading.com/underconstruction.png">

# Review, week5

NOT DONE YET. Needs

- [Proxy](https://sourcemaking.com/design_patterns/proxy)
- [Subject-observor](https://sourcemaking.com/design_patterns/observer)
- Model-View-controller (a variant on subject-observer)

## Patterns

### Proxy
### Layers

_**Intention**_ Decompose requirements into highly cohesive, loosely coupled services
at different levels of abstract.

_**Structure**_

- Some encapsulation mechanism (e.g. classes with lots of nested structure)
- (Maybe) some global blackboard (see below)

_**Examples**_

- Data-Model-Dialog: classic 3-tiered architecture for relational data base applications
   
- Model-View-Controller: A pattern
- LAMP= Linux, Apache, mysql, (php|perl|python). Officially, can switch out layers for another
  structure but I've  never actually seen that done.
- MEAN= layers of Javascript for web-apps. Since its all in one language, more "leakage" of information
  across the layers (makes e.g. debugging or optimizing easier). Mean comprises
      - M = MongoDB, a popular NoSQL database
      - E = ExpressJS : make results to db and return response
      - A = AngularJS: request/display results for end user
      - N = NodeJSL massive set of JavaScript tools
- Blackboards: used in AI. See below.
- OSI 7-layer Model:
     - Application  Layer 7  Provides misc protocols for common activities (ftp, telnet, http, etc)
     - Presentation  Layer 6  Structures information and attaches semantics
     - Session  Layer 5  Provides dialog control and synchronization facilities
     - Transport  Layer 4  Breaks messages into packets and guarantee delivery
     - Network  Layer 3  Select a route from sender to receiver
     - Data Link  Layer 2  Detects and corrects errors in bit sequences
     - Physical  Layer 1  Transmits bits: velocity, bit-code, connection, etc

_**Rules of Thumb**_

- Sometimes, 2 layers are enough: see the Observer pattern that just does Subject-Observer
- Ideally, each level should be highly cohesive but loosely coupled to to the others.
- Ideally, each layer i should only know about layers i+1, i-1 (but see optimization, below).
- The more layers, the lower the efficiency
 - When optimizing, ignore the layers. Jump   long way around the code.
- If you can write all the layers in the same language, that has maintenance and system advantages
     - e.g. MEAN, written in Javascript
- Cost model for traditional 3-tied Data-Model-Dialog applications:
     - Cost model: each class in the "model" layer needs 1 data, 1 dialog, + 0.5 helper classes
     - Classes = 20 methods. Methods = 5 lines (median)
     - So each business concept class requires 350 lines of code (ish)
- Useful for each layer to be hidden with  a `Facade`
     - The pubic interface to all.
     - Typically, does no work itself but redirects messages to different parts of its internal
       structure.
- If the same information needed in different layers, the create `Proxy`s that can be created
  in a central location, and passed out to where they are needed
     - e.g. in Data-Model-Dialog, the range for `age` might be needed in Dialog (to help uses
       enter correct values) and in Data (to define the relational database fields)
     - So let the Model birth a proxy that is passed up to Dialog and down to Data.
     - Note in functional languages, the `Proxy`s can be lambda bodies paired to specific
       events in other classes. So all the other layer need to know is that on a certain event
       (e.g. button pressed) that the lambda is evaluated.

## Blackboard

_**Intent**_ Build a (very) loosely coupled system, perhaps using different programming languages,
where agents react to observations by making conclusions that define higher-level observations.
Good when

- A complete search is not feasible
- Need to experiment with different algorithms in the same sub-task
- Using disjoint algorithms perhaps exploiting  parallelism

_**Structure**_ 

- A multi-layered global space (the blackboard)
- Some networking architecture where the global space can be shared across multiple executables, machines,
  cities, countries.
- Agents that read from layer i and write to layer i+1
- Events, divided into event types
- A dispatch mechanism whereby agents can register that they only care about events of a certain
  type seen at a certain layer.

_**Rules of thumb**_

- Useful in poorly-structured, or simply new and immature domains
- When the application domain matures, perhaps abandon the Blackboard architecture and develop architectures that support closed solution approaches
- Use production systems as subroutines
     -.e.g. at each level, match-selects on layer i then acts on layer i+1
- Use match-select-act as the main controller (so only one agent can update the blackboard at a time)
     - Match= ask what agents think they have something to contribute
     - Select= find which agent has most definite conclusions
     - Act= let that agent update the blackboard
- Great for maintenance of a large community contributing to a knowledge-based system.
- Drawbacks:
     - Difficult to testing
     - No good solution is guaranteed
     - Good Control is hard to build
     - Low efficiency (cause of layers, cause of society of agents all bickering what to do next).
    
_**Examples**_

- Used [a lot in AI](https://aaai.org/ojs/index.php/aimagazine/article/view/537)
   to co-ordinate many researchers working on the same problem.
     - Think large DOD grants, many institutions, many grad students.
- e.g. speech recognition. layers are:
     - Phrases
     - Words
     - Segments
     - Waveform (lowest level)
- e.g. find a koala:
     - edge detection agents that input edges and output shapes
     - limb detection gents that input shapes and output limbs
     - species detection gent that input limbs and output different animal types
     - decision agents that input different animal types, reflect on their differences, and
    
### Model View controller

### Subject Observer

### Blackboard

Layers + Subject-Observer

## Unification

A wide range of tasks typically assigned to complex theorem proving algorithms can be implemented 
by a single _unification_ algorithm.

- Enter Logic Programming.

Some history

- 1950s: lets build parse trees!
- 1960, LISP, computation= rewrite parse trees from bottom to top

```
; input
(+ 
  (* 2 
     (/ 4 5))
  (+ 10
     1))

; first rewrite
(+ 
  (* 2 
     0.8)
  (+ 10
     1))

; second rewrite
(+ 
  1.6
  (+ 10
     1))

; third rewrite
(+ 
  1.6
  11)

; fourth rewrite

12.6
```

- 1970s: Prolog: computation = find bindings that let you match trees

Unification means 

- compare 2 trees
- find substitutions in one that make it equal to the other.

Given trees expressed as  lists containing

- nest lists, recursively
- atoms; i.e. strings, symbols, numbers, or
  variables (perhaps denotes with a leading question mark `?x`)

Then, to unify two lists

- Case1: Seek bindings that let you unify the head
- Case2: Using those binding, try to unify the tail

Also, to unify two atoms

- Case3: if they are equal, then they unify.
      - Detail: in this case, things will unify without extending the bindings
- Case4: else if one has a `known` binding, the see if the binding unifies to the other
- Case5: else if the other has a `known` binding, the see if the other binding unifies to it
- Case6: else if one is a variable, then extend the bindings by binding it to the other
- Case7: else if the other  is a variable, then extend the bindings by binding the other to it

Question: what do these cases fall in the following code:

```lisp
(defun unify (x y &optional binds)
  (cond 
    ((eql x y)        (values binds t))
    ((assoc x binds)  (unify (known x binds) y binds))
    ((assoc y binds)  (unify x (known y binds) binds))
    ((var? x)         (values (cons (cons x y) binds) t))
    ((var? y)         (values (cons (cons y x) binds) t))
    (t
      (when (and (consp x) (consp y))
        (multiple-value-bind (b2 yes) 
          (unify (car x) (car y) binds)
          (and yes (unify (cdr x) (cdr y) b2)))))))
```

Note that, because of the detail above, unification can return t without extending the
bindings.

- Which means if the initial bindings are nil then the new binding will be nil
- So unify has to return two values
    - One being the extension to the bindings
    - The other being a flag true/nil saying if unification was successful

We say that the first symbol in a list is its predicate. A logic program
is a set of nested lists, each of which has an outer most predicate.
If deep within one list, we find a predicate symbol, then we  jump over to
another list to handle some recursive unification.

The following examples use the syntax of the class Prolog-in-Lisp example:

````lisp
(<- (parent donald nancy)) 
(<- (parent donald sally)) 
(<- (child ?x ?y) (parent ?y ?x))
```

Question: what are the predicates?

Question: During recrusive unification (e.g. `parent` in `child` over to the real
parent lists) we give the symbols in the recursive list new values. Why?

Question: what bindings and "success flag" is returned by the following examples:

```lisp
;1
(unify '(p  a  b c  a) 
       '(p ?x ?y c ?x)) 

;2
(unify '(p  a  b c  20) 
       '(p ?x ?y c ?x)) 

;3 
(unify '(p ?x b ?y a) 
       '(p ?y b  c a))

;4 
(unify '(p ?x b ?y a) 
       '(p ?y b  c a))

;5
(unify '(a b c) 
       '(a a a)) 
```


