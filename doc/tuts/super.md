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

This is only one iteration, we may recurse on each split and redo the process to get greater granularity.

## In code
```lua
-- This code rewrites the contents of
-- the numeric independent variables as ranges (e.g. 23:45).
-- Such columns `c` are sorted then explored for a `cut` where
-- the expected value of the variance after cutting is 
-- minimized. Note that this code endorses a cut only if:
--
-- - _Both_ the expected value of
--   the standard deviation of `c` and the goal column
--   `goal` are  minimized
-- - The minimization is create than some trivially
--   small change (defaults to 5%, see `Lean.super.margin`);
-- - The number of items in each cut is greater than 
--   some magic constant `enough` (which defaults to
--   the square root of the number of rows, see
--   `Lean.super.enough`)
--
-- After finding a cut, this code explores both 
-- sides of the cut for recursive cuts.
-- 
-- Important note: this code **rewrites** the table
-- so the only thing to do when it terminates is
-- dump the new table and quit.

function super(data,goal,enough,       rows,most,cohen)
  rows   = data.rows
  goal   = goal or #(rows[1])
  enough = enough or (#rows)^Lean.super.enough 

-- -------------------------------------------
-- This generates a print string for a band
-- that streches from `lo` to `hi`.


  local function band(c,lo,hi)
    if lo==1 then
      return "..".. rows[hi][c]
    elseif hi == most then
      return rows[lo][c]..".."
    else
      return rows[lo][c]..".."..rows[hi][c] end
  end

-- Find one best cut, as follows.
--
-- - First all everything to a _right_ counter
--   (abbreviated here as `r`).
-- - Then work from `lo` to `hi` taking away
--   values from the _right_ and adding them
--   to a _left_ counter (abbreviated here as `l`).
-- - Using the information in these _right_ and
--   _left_ counters, work out where the best `cut` is.
-- - If no such `cut` found, return `nil`.
--
-- Tehcnical note: actually, we run two _right_
-- and two _left_ counters:
--
-- - two for the independent column (`xl` and `xr`)
-- - and two for the goal column  (`yl` and `yr`)

  local function argmin(c,lo,hi, cohen,    
                          x,nl,nr,bestx,tmpx,
                          y,sl,sr,
                          cut) 
    nl,nr = num(), num()
    sl,sr = sym(), sym()
    for i=lo,hi do 
      numInc(nr, rows[i][c]) 
      symInc(sr, rows[i][goal]) end
    bestx = symEnt(sr)
    if (hi - lo > 2*enough) then
      for i=lo,hi do
        x = rows[i][c]
        y = rows[i][goal]
        numInc(nl, x); numDec(nr, x) 
        symInc(sl, y); symDec(sr, y) 
        if nl.n >= enough and nr.n >= enough then
          if nl.hi - nl.lo > cohen then
            if nr.hi - nr.lo > cohen then
              tmpx = symXpect(sl,sr) * Lean.super.margin
              if tmpx < bestx then
                cut,bestx = i, tmpx end end end end end end 
    return cut
  end

-- If we can find one good cut:
--
-- - Then recurse to, maybe, find other cuts. 
-- - Else, rewrite all values in `lo` to `hi` to
--   the same string `s` representing the range..

  local function cuts(c,lo,hi,pre,cohen,       cut,txt,s,mu)
    txt = pre..rows[lo][c]..".."..rows[hi][c]
    cut,mu = argmin(c,lo,hi,cohen)
    if cut then
      fyi(txt)
      cuts(c,lo,   cut, pre.."|.. ",cohen)
      cuts(c,cut+1, hi, pre.."|.. ",cohen)
    else
      s = band(c,lo,hi)
      fyi(txt)
      for r=lo,hi do
        rows[r][c]=s end end
  end

-- Our data sorts such that all the "?" unknown values
-- are pushed to the end. This function tells us
-- where to stop so we don't run into those values.

  function stop(c,t)
    for i=#t,1,-1 do if t[i][c] ~= "?" then return i end end
    return 0
  end

-- For all numeric indpendent columns, sort it and 
-- try to cut it. Then `dump` the results to standard output.

  for _,c  in pairs(data.indeps) do
    if data.nums[c] then
      ksort(c,rows) -- sorts the rows on column `c`.
      most = stop(c,rows)
      local all = num()
      for i=1,#data.rows  do numInc(all, rows[i][c]) end
      fyi("\n-- ".. data.name[c] .. " ----------")
      cuts(c,1,most,"|.. ",all.sd*Lean.super.cohen) end end
  print(gsub( cat(data.name,", "), 
              "%$","")) -- dump dollars since no more nums
  dump(rows)
end
```
