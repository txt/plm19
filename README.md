# plm19

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
 
       
