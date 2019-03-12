[monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

----

# Super= supervised descretization

In which we learn that we don't care about numbers themselves, we only care when those numbers change something else.

## Previously, in [BestRest](bestrest.md)...

We saw how we can split a row into two parts by minimizing the expected value of the standard deviation. 

Now, we can extend that further. We can use the same principle (expected value of one column to find a split in another column).

Take the following for example:
```
A    Klass
1    0
1    0
2    0
2    0
2    1
6    1
7    1
8    1
9    1
9    1
```

Here we can try and find a split in A such that the expected value of the standard deviation of the `Klass` column is minimized after the split.

This looks like a good split

```
A    Klass
-    -----
1    0    |
1    0    |
2    0    |    ---> 0.2
2    0    |
2    1    |
===========
6    1    |
7    1    |
8    1    |    ---> 0
9    1    |
9    1    |
```

So, the new table will look as follows

```
A    Klass
-    -----
1..6    0    |
1..6    0    |
1..6    0    |    ---> 0.2
1..6    0    |
1..6    1    |
===========
6..9    1    |
6..9    1    |
6..9    1    |    ---> 0
6..9    1    |
6..9    1    |
```
