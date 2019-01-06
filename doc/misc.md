# misc stuff (ignore)

nice tour of scale

https://docs.scala-lang.org/tour/tour-of-scala.html

proj

1. OO abstractions 
     - use (inheritance, polymorphsim, design patterns)
     - use better. succinct shorthands. checks at load time
2. other abstractions
     - use other abstractions (and there are many)
     - use better. succinct shorthands. checks at load time

intro abstarction:

- the "o" object
- then go to frames, slots, ifneeded if changed etc
- polymorphism eg (sandbox/py/cocomo)

todo

- week1: python exercises rom data ming from sctach
- week2: higher level functions 
       - isEmpty  size  includes:  occurrencesOf: do:  select:  detect:[ifNone:]  reject:  collect:  inject:into:   
       - example of a spec language where the horrow is in superclass and simplicity is in subclass
- week3: compartmental models + frame langauge
- FSMs, rules, roles (from taql)


its all about the trees

- operator associativity (prolog lab)
- first there were stacks
- then shunting yard (implict trees)
       - code to got infix ==> rpn; rpn ==> trees
- then ress trees trees trees trees (c, lex, yacc)
- or comptuation = tree rewrite (lambdas)
- or comptuation = tree matching (logic)

create a class called compiled with thhe filled in string as an inst var and a __call__ method that runs the passed in lambda. so swe can acces sthe string and do lint checks if we want

list of properties for my ideal language

- succinct
- declarative
- is error seeking

use the yaml1.py code. needs a safe parser for ands ors nots add in class reference. see

- http://pyparsing.wikispaces.com/file/view/simpleBool.py/451074414/simpleBool.py
- http://pyparsing.wikispaces.com/file/view/eval_arith.py/68273277/eval_arith.py
- http://www.ptmcg.com/geo/python/confs/pyCon2006_pres2.html
       - http://www.ptmcg.com/geo/python/confs/adventureEngine.py.txt
       - http://www.ptmcg.com/geo/python/confs/pyparsing_adventure.pdf
- or, to keep it simple: https://stackoverflow.com/a/24721686/4234106

or use yaml indents to denote and/ors/nots. and have an "include" command to grab yaml from other files.
add a stripper to get rid of bad characters

- a key concent here will be the payload you carry around
- good place to introduce environments. and local subevironments you can enter, then exit and forget
- block structure variable scpre


rules:
- write condition and action as seperte items
- where its are rules

```
class rule():
  def __init__(i):
     i.priority=1
  def when(i, *l,**kw): return True
  def act(i,  *l,**kw): return True

class fred(rule):
   def when(i,x):
     for one in x:
      if one == 1:
        yield one
   def act(i,y):
      print(y)

class rules:
  def __init__(i,lst=[]):
    i._rules=lst
  def run(i,*l,**kw):
    for rule in i._rules:
      for out in rule.when(*l,**kw):
        tmp += [(rule.priority, out,rule.act)]
    sorted(tmp)
    for (_,out,act) in tmp:
      act(out,*l,**kw)
```   
 
 Examples on internal DSLs
 
 - web site generation
 

