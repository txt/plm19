[monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

----

# Dom
Here, we'll learn how to summarize our problems into one large problem. We'll see why that's useful.


## What is domination?

> "Give me the fruitful error any time, full of seeds, bursting with its own corrections. You can keep your sterile truth for yourself."    
― Vilfredo Pareto


> "For many events, roughly 80% of the effects come from 20% of the causes."   
― Vilfredo Pareto

Which house do you want:

![](https://txt.github.io/fss18/img/build.png)

## Domination measures:


### Binary Domination

- X domiantes Y if better on at least one, and worse on none.
- Returns a boolean.
- Widely used. Often stops distinguishing  things after 3+ goals.
- Insenstive to the size of the domnation

![](http://cms.horus.be/files/99936/MediaArchive/pictures/Pareto_Dominated.png)

### Indicator Domiantion

From [Zitler 2004](https://pdfs.semanticscholar.org/8ed1/04b1d836930e88cc048e55bc9b985b05542f.pdf):

- X dominates Y if moving X to Y _loses less_ than moving Y to X.
    - To make the signal stronger, we _shout_ the difference (raise it to
   some expendial power).
    - To gauge the overall signal, we _average_ across all goals
    - To handle goal we want to minimize, maximize, we add a "_w_"
      constant (-1,1).
- Returns a number.
- Better at [distinguishing higher problems with 3+ goals](https://fada.birzeit.edu/jspui/bitstream/20.500.11889/4528/1/dcb6eddbdac1c26b605ce3dff62e27167848.pdf), see Table8.

- To rank _rowX_, I
    - N=100 times (say), pick _rowY_ at random.
        - Q: Why not comapre to all rows?
    - If _indicatorDomiantes( X,Y )_ then +1/N to _rowX_
    - Then I add a new `dom` column to the data, wheich we want to maximize.
    - see [dom.html](http://menzies.us/lean/dom.html)

For example, here are some rows
of [auto.csv](https://github.com/timm/lean/blob/master/data/autoUnsuper.csv):

      %cylinders   displacement         horsepower   <weight   >acceltn   model   origin   >mpg
      8           340:360              150:160      3609      8          70:70   1        10
      6           198:225              83:86        2587      16         70:70   1        20
      8           383:455              165:230      4425      10         70:70   1        10
      8           302:305              120:140      3449      10.5       70:70   1        20
      6           198:225              92:97        2833      15.5       70:70   1        20
      8           383:455              165:230      3850      8.5        70:70   1        20
      4           104:114              92:97        2372      15         70:70   3        20
      4           104:114              92:97        2375      17.5       70:70   2        30
      8           340:360              165:230      3693      11.5       70:70   1        20
      ....

From the first row,  we see we want to minimize weight and maximize acceleration and mpg.
From the other rows, we see that some discretizer has gotten to the _displacement_ and _horsepower_
values are replaced them with some string describing a range e.g._lo:hi_ =  _340:360_.

Here's the same data, with `dom` score added. Shown here are the 5 best and worst rows.


    %cylinders  displacement  horsepower  <weight  >acceltn  model  origin  >mpg  >dom
    8           383:455       165:230     4746     12        71:71  1       10    0
    8           383:455       165:230     4951     11        72:73  1       10    0
    8           383:455       165:230     4952     11.5      72:73  1       10    0
    8           383:455       165:230     4955     11.5      71:71  1       10    0
    8           383:455       165:230     5140     12        71:71  1       10    0
    8           383:455       165:230     4354     9         70:70  1       10    0.01
    8           383:455       165:230     4425     10        70:70  1       10    0.01
    8           383:455       165:230     4464     11.5      71:71  1       10    0.01
    8           383:455       165:230     4735     11        72:73  1       10    0.01
    8           383:455       165:230     4906     12.5      72:73  1       10    0.01
    ..          ...           ...         ...      ..        ...    ...     ...   ...
    4           85:91         69:72       2070     18.6      78:78  3       40    0.98
    4           85:91         ?           1835     17.3      80:80  2       40    0.98
    4           68:85         46:65       1825     18.6      77:77  2       40    0.99
    4           68:85         69:72       1613     18        71:71  3       40    0.99
    4           85:91         46:65       2335     23.7      80:80  2       40    0.99
    4           85:91         46:65       1968     18.8      80:80  3       40    1.0
    4           85:91         46:65       1975     19.4      81:81  3       40    1.0
    4           85:91         46:65       1985     21.5      78:78  2       40    1.0
    4           85:91         46:65       2085     21.7      80:80  2       40    1.0
    4           96:97         46:65       2130     24.6      82:82  2       40    1.0

Observe that the _highest_ dom scores are assocaited wiht rows with least weight, most acceleration
and most mpg (and the _lowest_ dom scores are associated with the reverse).


## In Code

First we want to compute the dom score for pairs of rows:
```lua
function dom(t, row1, row2, n, a0, a, b0, b, s1, s2)
  s1,s2,n = 0,0, 0
  for _ in pairs(t.w) do n=n+1 end
  for c,w in pairs(t.w) do
    a0 = row1[c]
    b0 = row2[c]
    a  = numNorm( t.nums[c], a0)
    b  = numNorm( t.nums[c], b0)
    s1 = s1 - 10^(w * (a-b)/n)
    s2 = s2 - 10^(w * (b-a)/n)
  end
  return s1/n < s2/n
end
```

Now we can append a column to the original table with the dom score
``` lua
function doms(t,  n,c,row1,row2,s)
  n= Lean.dom.samples
  c= #t.name + 1
  print(cat(t.name,",") .. ",>dom")
  for r1=1,#t.rows do
    row1 = t.rows[r1]
    row1[c] = 0
    for s=1,n do
     row2 = another(r1,t.rows)
     s = dom(t,row1,row2) and 1/n or 0
     row1[c] = row1[c] + s end end
  dump(t.rows)
end
```



