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
