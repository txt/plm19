<a href="http://tiny.cc/plm19">home</a> ::
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> ::
<a href="https://github.com/txt/plm19/tree/master/src">src</a> ::
<a href="http://tiny.cc/plm19give">submit</a> ::
<a href="https://plm19.slack.com/">chat</a> ::
<a href="https://github.com/txt/plm19/blob/master/license.md">&copy;</a> 2019, <a href="http://menzies.us">timm</a>
<br>
<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>

# Review, week4

## Environment Pattern

_**Intent**_ : 

## State Machines

_**Intent**_  : Seperate logic from the program, expressing that logic
very simpley.

_**Problem**_ : Some ways to express knowledge are hard to understand,
complex to implement. Enter state machines.

_**Structure**_ :

- Machine
     - A place to store lists of "States" and "Transitions".
     - Holds a local variable pointing to the "start" state 
- Transistions
     - Usually a 3-part or 4-part tupe
     - FROM: the "from" state
     - TO: the "to" state
     - GAURD: a "test" to see if we should use this transition
     - SIDE-EFFECT: an action to take if the GAURD is satisfied
       (e.g. if the GAURD == some button is pushed then
       the SIDE-EFFECT could be write some record to a log).
- States
     - Which can be simple boolean tags (e.g. state1=entry; state2=exit)
     - Which, for more complex modeling, can be instances of a class,
       with specialized sub-classes (e.g. entry can be sub-classes
       to store specializations for start up). Also, it is useful
       to have an abstract "exit" class from which you can
       specialize different exit conditions.
     - Holds a list of out-transistion from each state.
- Nested Machine
     - One special kind of state can be a Machine. Which means
       machines can be nested inside each other.

<img width=500 src="https://www.w3.org/2005/Talks/0621-dsr-mmi/watch-hsc.png">

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

- Useful for description of high-level logic for a system
- State machines are usually used for discrete variables.
  Use compartmental models for contionous variables.
- Once systems are specified as state machines, formal
  methods can be applied to check from (e.g.)  infinite loops
  or unreacahble states.
- Don't try to specify large systems, just with state machines.
  Better to divide the world up into classes, then use
  state machines to spec the internal logic of some classes.
  small areas.
- Has some advantages over production rules (instead of 
  spending 60% to 80% of the time in "match", then we only
  need to match across the out transitions from the current state).

_**See Also**_ :

- Compartmental models
- Composite pattern: Machines can contain States or nested Machines.
- Visitor pattern: Used to walk around the machines, transistions,
  states and nexted machines.

## Compartmental Models
