# Brook's Law

## 1. Introduction

In the software engineering classic *The Mythical Man-Month*, Fred Brooks stated, “Adding manpower to a late software project makes it later” 
His explanation for the law was the additional linear overhead needed for training new people and the nonlinear communication overhead (a function
of the square of the number of people). These effects have been widely accepted and observed by others. 

The model is conceived around the following basic assumptions:
 * New personnel require training by experienced personnel to come up to speed.
 * More people on a project entail more communication overhead.
 * Experienced personnel are more productive than new personnel, on average.
 
Let's try and define with a compartmental model. It would look as follows:

![image](https://user-images.githubusercontent.com/1433964/53421272-651b7580-39ab-11e9-8128-9021a0ca37d9.png)

* It is built on two connected flow chains representing software development and personnel. 
* The software development chain assumes a level of requirements that needs to be implemented (see the upper left box in the above Figure). 
* The requirements are transformed into developed software at the software development rate (rates are shown as the circular pipe-valve symbols).
    - The level of developed software represents progress made on implementing the requirements. Project completion is when
developed software equals the initial requirements.
* The software development rate is determined by the levels of personnel in the system: new project personnel who come onto the project at the personnel allocation rate,
and experienced personnel who have been assimilated (trained) into the project at the
assimilation rate. 

Note: For the notations in the above figure, see [this]():

## 2. Model equations

In [compartmental model](), we saw how to design a compartmental model in terms of `Stocks`, `Flows`, and `Aux`illary variables. 
We can use to that build a model for Brook's Law.

Let's first initialize some of the variables in the above figure

```python
class BrooksLaw(Model):
    def __init__(self, params):
        super(BrooksLaw, self).__init__(params)
        self.params = params
```

Here, 
1. We create a new `Model` called the `BrooksLaw`
2. Seed it with some initial parameters `params`
    - Note: Let's not worry about how the `params` class is defined 
    right now. We'll get back to that later.

Next, lets create `Stocks`, `Flows`, and `Aux`s

```python
class BrooksLaw(Model):
    def __init__(self, params):
        super(BrooksLaw, self).__init__(params)
        self.params = params

    def have(i): return Things(
        aR    = Flow( "assimilationRate"),
        co    = Percent(  "communicationOverhead"),
        d     = Stock("developedSoftware",i.params.d),
        ep    = Stock("experiencedPeople",int(i.params.ep)),
        ept   = Aux(  "experiencedPeopleNeeded2Train"),
        nprod = Aux(  "nominalProductity",i.params.nprod),
        np    = Stock("newPersonnel",int(i.params.np)),
        paR   = Flow( "personnelAllocationRate"),
        ps    = Aux(  "plannedSoftware"),
        sdR   = Flow( "softwareDevelopmentRate"),
        ts    = Aux("teamSize",i.params.ts),
        to    = Percent( "trainingOverhead",i.params.to), # one-quarter of an experienced
                                              # person's time is needed to
                                              # train a new person until
                                              # he/she is fully assimilated.
        r     = Stock("requirements",i.params.r))
```

Note that all these variables will appear in the above [figure](https://user-images.githubusercontent.com/1433964/53421272-651b7580-39ab-11e9-8128-9021a0ca37d9.png).

Having set this up, now let's define some equations to run this model.

1. **`Communication Overhead`**
The fraction of time one spends communicating with other team members and one's own team members as a function of team size.
We use the `n^2` law to determine this.

In code, this will look as follows:
```python
  def _co(x):
    "Communication overhead"
    myTeam = i.ts - 1   # talk to everyone in my team
    others = x/i.ts - 1 # talk to every other team
    return pomposity*(myTeam**2 + others**2) # pomposity
```

2. **`Assimilation Rate`**



