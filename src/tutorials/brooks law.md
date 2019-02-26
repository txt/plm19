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
  def _co(total_personnel):
    "Communication overhead"
    myTeam = i.ts - 1   # talk to everyone in my team
    others = total_personnel/i.ts - 1 # talk to every other team
    return pomposity*(myTeam**2 + others**2) # pomposity
```

With that definition, at every time step, this will change as follows:
```
comm_overhead = _co(new_personnel + experienced_personnel)
```

2. **`Assimilation Rate`**
How long it takes to assimilate new personal. If it takes `N` days for a new personnel to learn a new tool (let's call this learning rate), then the assimilation rate will be `(no. of. new personnel) / (learning rate)` per day.

In code,
```python
assimilation_rate  = new_personnel/learning_rate
```

3. **`Planned Software`**
Assuming a productivity of `P` function points a day, the planned software that will be completed in `t` days is `P*t`

```python
planned_software = productivity * time
```

4. **`Personnel Allocation Rate`**
This defines the rate at which new personnel are allocated to an on-going project. Let's assume that the management utilizes feedback from the actual work accomplished to determine the number of personnel allocated as follows:
  * 6 personnel are allocated if 
    - The amount of developed software is less than X% of the planned software, and 
    - If T% of the estimated time to complete project is remaining.
  * 0 personnel otherwise.

For example, a policy may state that 6 personnel are allocated if less 75% of the project is completed and if the current time is less than 80% of the total time alloted for a project. If *more* than 75% is done, or if *more* than 80% of the time to complete the project is done, then *no* new personnel will be added.

In, code
```python
if planned_software - developed_software < X and t < T * t_max:
    personnel_allocation_rate = 6
else:
    personnel_allocation_rate = 0
```

5. **`Software Developemnt Rate`**
The software development rate represents the productivity adjusted for communication overhead, weighting factors for varying mix of personnel, and the effective number of experienced personnel.

It depends on factors such as:
  - Nominal productivity: `nominal_productivity`
  - Communication overhead: `comm_overhead`
  - Number of new personnel: `new_personnel`
  - Productivity of new personnel: `productivity_new`
  - Number of experience personnel that are able to develop (i.e., not busy with training other personnel):
      `available_experienced_personnel = experience_personnel - experienced_personnel_busy_training`
  - Productivity of experienced personnel: `productivity_experienced`
In code, 
```python
available_experienced_personnel = experience_personnel - experienced_personnel_busy_training
software_dev_rate = nominal_productivity * (1 - comm_overhead/100) * (productivity_new * new_personnel + productivity_experienced * available_experienced_personnel)
```
