----
### Pipe
$ [monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

### Help
+ [Table Format](table.md)
+ [Tips on debugging](debug.md)
+ [Codeanywhere](codeanywhere.md)
----

# Monte Carlo Simulation

The goal of monte-carlo simulation is to randomly generate a set of samples to run an algorithm over. Monte Carlo methods vary, but tend to follow a particular pattern:

1. Define a domain of possible inputs
2. Generate inputs randomly from a probability distribution over the domain
3. Perform a deterministic computation on the inputs
4. Aggregate the results

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Pi_30K.gif/220px-Pi_30K.gif" align=right>
For example, consider a quadrant (circular sector) inscribed in a unit square. Given that the ratio of their areas is 
`π/4`, the value of π can be approximated using a Monte Carlo method:

 - Draw a square, then inscribe a quadrant within it
 - Uniformly scatter a given number of points over the square
 - Count the number of points inside the quadrant, i.e. having a distance from the origin of less than 1
 - The ratio of the inside-count and the total-sample-count is an estimate of the ratio of the two areas, `π/4`. 
 - Multiply the result by 4 to estimate π.

In this procedure the domain of inputs is the square that circumscribes the quadrant. We generate random inputs by scattering grains over the square then perform a computation on each input (test whether it falls within the quadrant). Aggregating the results yields our final result, the approximation of π.

There are two important points:

- If the points are not uniformly distributed, then the approximation will be poor.
- There are a large number of points. The approximation is generally poor if only a few points are randomly placed in the whole square. On average, the approximation improves as more points are placed.

# For our pipeline

First we define all the variables than can be changed. We need the name, the `min` and `max` values that it can take.

Then we use a uniform distribution to generate random samples.

It will look as follows:

```
from random import seed, uniform

class MonteCarlo:
    def __init__(self):
        self.attr = {
            "pomposity" : {
                "min": 0, 
                "max": 1  
                },
            "learning_curve" : {
                "min": 1, 
                "max": 100
                },
            "optimism" : {
                "min": 0.1, 
                "max": 10},
            "atleast" : {
                "min": 0, 
                "max": 100
                },
            "done_percent" : {
                "min": 0, 
                "max": 100
                },
            "sDR_param1" : {
                "min": 0, 
                "max": 1 
                },
            "sDR_param2" : {
                "min": 1, 
                "max": 10
                },
            "sDR_param2" : {
                "min": 1, 
                "max": 10
                },
            "d" : {
                "min": 0, 
                "max": 90
                },
            "ep" : {
                "min": 1, 
                "max": 30
                },
            "nprod" : {
                "min": 0.1, 
                "max": 1
                },
            "np" : {
                "min": 1, 
                "max": 30
                },
            "ts" : {
                "min": 1, 
                "max": 10
                },
            "to" : {
                "min": 1, 
                "max": 100
                },
            "r" : {
                "min": 100, 
                "max": 1000
                },
        }

    def generator(self, iters=1):
        for i in range(int(iters)):
            row = {key: round(uniform(item["min"], item["max"]),2) for key, item in self.attr.items()}
            print(row)
```

Once we have defined this class, we to take input parameters that can read (a) `-n` the number of samples and (b) `-s` a random number seed.

We can use argparse for this

```
import argparse

def monte_carlo_main(iters, seed_val):
    mc = MonteCarlo()
    seed(seed_val)
    mc.generator(iters)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Monte Carlo generator.')
    parser.add_argument('-n', '--num-repeats', metavar='rep', type=int, default=1, help='Number of repeats')
    parser.add_argument('-s', '--seed', metavar='seed', type=int, default=1, help='Random number seed')	
    args = parser.parse_args()
    monte_carlo_main(iters=args.num_repeats, seed_val=args.seed)
```
