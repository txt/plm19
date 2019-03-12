[monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

----

# Rank

In which we slice and dice our data such that only the most important ideas survive.

# Goal

We want to know which of the features we developed appear in the best/rest. 

+ The more often it appears in best and rest, the more discriminatory it is in determining what is best and what are the rest.

So, we do a [super](super.md) (supervised split) of each of the columns with respect to the [dom](dom.md) score. That is, for every column `c` is sorted then explored for a `cut` where the expected value of the variance after cutting is minimized. 

Note that this code endorses a cut only if:

1. _Both_ the expected value of the standard deviation of `c` and the goal column `dom` are  minimized

2. The minimization is larger than some trivially small change (defaults to 5%, see `Lean.super.margin`)

3. The number of items in each cut is greater than some magic constant `enough` (which defaults to the square root of the number of rows, see `Lean.super.enough`)

4. After finding a cut, this code explores both sides of the `cut` for recursive cuts.

The primary goal of rank is to basically 

1. See which column `c` appears most often.
2. Determine the range of values it takes 
3. And, for each range, count the number of times it appears in `best` and `rest`

# Sample output

```
  Total      Column Name    Range         Best  Rest
1 45    productivity_exp    ..6.26          62    25
2 33            optimism    77.64..         46    19
3 28             atleast    7.34..10.04     33     6
4 18             atleast    ..7.23          29    18
5 16            optimism    ..15.68         25    15
6 9     productivity_new    28..            12     6
```

So, we see here that the productivity of experienced personnel `productivity_exp` is the most important discriminating feature to improve the amount of developed software.

## In code

```lua
function  main(file,   all,both,best,rest,nb,nr,order,b,r,s)
  all,both  = klasses(file)
  best,rest = bestRest(both)
  nb        = #both[best].rows
  nr        = #both[rest].rows
  order={}
  for c,_ in pairs(all.name) do -- for all columns
    if indep(all,c) then        -- for all independent columns
      for x,_ in pairs(all.syms[c].counts) do -- for all symbols in column
        b = look2(both[best],c,x,0.001)
        r = look2(both[rest],c,x,0.001)
        b = b/(nb + 0.00001)
        r = r/(nr + 0.00001)
        if b > r then
          order[#order+1] = {-1*b^2/(b+r), c,x,b,r} end end end end
  ksort(1,order)
  s = function (x) return int(100*x) end
  for i,t in pairs(order) do
    print(i,s(-1*t[1]), 
            all.name[t[2]],t[3],s(t[4]),s(t[5]))
  end
end
```

