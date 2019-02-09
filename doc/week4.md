<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/license.md">&copy;</a> 2019, <a href="http://menzies.us">timm</a>

# Review, week4

## Things to Know


## Environment Pattern

_**Intent**_ : Avoiding globals, share some updatable states

_**Structure**_:

- Some dictionary-like structure based around from place to place
  (e.g. carried inside a visitor).

_**Examples**_:

For nested structure: if you dont have it, pull it from the outer

```python
class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms,args))
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."
        return self if var in self else self.outer.find(var)
```

For much simpler flat data (no recursion):

```python
class Struct:
    "A structure that can have any fields defined."
    def __init__(self, **entries): 
        self.__dict__.update(entries)

    def __repr__(self):
        args = ['%s=%s' % (k, repr(v)) for (k,v) in vars(self).items()]
        return 'Struct(%s)' % ', '.join(args)
```
Example:

```python
     >>> options = Struct(answer=42, linelen=80, font='courier')

     >>> options.answer
     42

     >>> options.answer = 'plastics'

     >>> vars(options)
     {'answer': 'plastics', 'font': 'courier', 'linelen': 80}

     >>> options
     Struct(answer='plastics', font='courier', linelen=80)
```

## State Machines


<img align=right width=450 src="https://www.w3.org/2005/Talks/0621-dsr-mmi/watch-hsc.png">

_**Kind of**_ : Interpreter

_**Intent**_  : Seperate logic from the program, expressing that logic
very simply.

_**Problem**_ : Some ways to express knowledge are hard to understand,
complex to implement. Enter state machines.

_**Structure**_ :

- `MACHINE`
     - A place to store lists of `STATE`s and `TRANSITION`s.
     - Holds a local variable pointing to the `start` state 
- `TRANSISTION`s
     - Usually a 3-part or 4-part tupe
     - `from` the "from" state
     - `to` the "to" state
     - `gaurd` a "test" to see if we should use this transition
     - `side-effect`: an action to take if the GAURD is satisfied
       (e.g. if the `gaurd` == some button is pushed then
       the `side-effect` could be write some record to a log).
- `STATE`s
     - Which can be simple boolean tags (e.g. state1=entry; state2=exit)
     - Which, for more complex modeling, can be instances of a class,
       with specialized sub-classes (e.g. entry can be sub-classes
       to store specializations for start up). Also, it is useful
       to have an abstract "exit" class from which you can
       specialize different exit conditions.
     - Holds a list of out-transistion from each state.
- `NESTEDMACHINE`
     - One special kind of `STATE` can be a `MACHINE`. Which means
       machines can be nested inside each other.

_**Example**_ :

```c
/*
http://stackoverflow.com/questions/1371460/state-machines-tutorials/1371654#1371654
State machines are very simple in C if you use function pointers.
Basically you need 2 arrays - one for state function pointers and one for state 
transition rules. Every state function returns the code, you lookup state 
transition table by state and return code to find the next state and then 
just execute it.
*/

int entry_state(void);
int foo_state(void);
int bar_state(void);
int exit_state(void);

/* array and enum below must be in sync! */
int (* state)(void)[] = { entry_state, foo_state, bar_state, exit_state};
enum state_codes { entry, foo, bar, end};

enum ret_codes { ok, fail, repeat};
struct transition {
    enum state_codes src_state;
    enum ret_codes   ret_code;
    enum state_codes dst_state;
};
/* transitions from end state aren't needed */
struct transition state_transitions[] = {
    {entry, ok,     foo},
    {entry, fail,   end},
    {foo,   ok,     bar},
    {foo,   fail,   end},
    {foo,   repeat, foo},
    {bar,   ok,     end},
    {bar,   fail,   end},
    {bar,   repeat, foo}};

#define EXIT_STATE end
#define ENTRY_STATE entry

int main(int argc, char *argv[]) {
    enum state_codes cur_state = ENTRY_STATE;
    enum ret_codes rc;
    int (* state_fun)(void);

    for (;;) {
        state_fun = state[cur_state];
        rc = state_fun();
        if (EXIT_STATE == cur_state)
            break;
        cur_state = lookup_transitions(cur_state, rc);
    }

    return EXIT_SUCCESS;
}

/*
I don't put lookup_transition() function as it is trivial.
That's the way I do state machines for years.
*/
```

_**Rules of thumb**_ :

- When writing an interpreter for some domain-specic language, favor a target language with
      - A high-level visual notation
      - A simple impementation
      - An underlying mathematical formulation
      - Two such target languages are state machines and compartmental models.
- Useful for description of high-level logic for a system
- State machines are usually used for discrete variables.
  Use compartmental models for contionous variables.
- Once systems are specified as state machines, formal
  methods can be applied to check from (e.g.)  infinite loops
  or unreacahble states.
- Don't try to specify large systems, just with state machines.
  Better to divide the world up into classes, then use
  state machines to spec the internal discrete logic of some classes.
  small areas.
- Has some advantages over production rules (instead of 
  spending 60% to 80% of the time in "match", then we only
  need to match across the out transitions from the current state).

_**See Also**_ :

- Interpter
- Compartmental models
- Composite pattern: Machines can contain States or nested Machines.
- Visitor pattern: Used to walk around the machines, transistions,
  states and nexted machines.

## Compartmental Models

_**Kind of**_ : Interpreter

_**Intent**_ :
Seperate logic from the program, expressing that logic
very simply.

_**Problem**_ : 
Some ways to express knowledge are hard to understand,
complex to implement. Enter  machines.

_**Structre**_ :

- `payload0`: 
     - a `STRUCT` to hold the state of the model at the last time tick.
- `payload1`: 
     - a STRUCT to hold the state of the model at
  the next time tick (initialized to be a copy of
  `payload0`, then updated by the `step` function)
- `STOCK`s: 
     - a variable is measured at one specific time. Holds numeric
  variables. May be held inside `payload`s.
- `FLOW`s: 
     - roughly analogous to rate or speed 
- `AUX`s: 
     - auxillary variables. Constants that effect flows.
  May be held inside `payload`s.
- `MODEL`: 
     - a container class holding <`STOCK`s, `FLOW`s, `AUX`s>
     - an `have` method that creates <`STOCK`s, `FLOW`s, `AUX`s>
     - an `step` method that generates `payload1` from `payload0`

_**Rules of Thumb**_ :

- Don't try to specify large systems, just with compartmental models.
  Better to divide the world up into classes, then use
  compartmetanl machines to spec the internal numeic logic of some classes.
  small areas.
- Compartmental machines are usually used for numeric  variables.
- Compartmental models can be written by reading a flow diagram and
  the writing down

      payload1.x = payload1.x + 
                   dt(in1 + in2 + ... - out1 - out2 - ...)

_**Example**_ :


```
 q   +-----+  r  +-----+
---->|  C  |---->|  D  |--> s
 ^   +-----+     +-+---+
 |                 |
 +-----------------+ 
```

- `C` = stock of clean diapers
- `D` = stock of dirty diapers
- `q` = inflow of clean diapers
- `r` = flow of clean diapers to dirty diapers
- `s` = out-flow of dirty diapers

This is modeled as one have methods that initializes:

- `C`,`D` as a Stock with initial levels 100,0;
- `q`,`r`,`s` as a Flow with initial rates of 0,8,0

and as a step method that takes a payload `u`
from time before  and computes a new payload  `v` at time `t+dt`.

```python
class Diapers(Model):

  def have(i):
    return o(C = S(100), D = S(0),
             q = F(0),  r = F(8), s = F(0))

  def step(i,dt,t,u,v):
    def saturday(x): return int(x) % 7 == 6
    v.C +=  dt*(u.q - u.r)
    v.D +=  dt*(u.r - u.s)
    v.q  =  70  if saturday(t) else 0 
    v.s  =  u.D if saturday(t) else 0
    if t == 27: # special case (the day i forget)
      v.s = 0
```

_**See Also**_ :

Some ways to express knowledge are hard to understand,
complex to implement. Enter state machines.


