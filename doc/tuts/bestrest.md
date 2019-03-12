[monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

----

# BestRest

In which we realize that it doesn't matter
if you win or lose, it's all about the journey. 

We jest of course, it __*only*__ matters if you win.



## How to find a split

Let's use an example to illustrate how to we'd go about doing this:

```
arr = [1,1,1,1,3,4,4,3,3,5,3,2,4,3,2,6]
```

We have 16 numbers above. 


We break them apart as follows:

1. Find a split index, `i=1,..., N-2`
2. Split the data into `left=arr[:i]` and `right=arr[i:]`
3. Is there less than `enough` number of elements on each split (`left` and `right`)? 

    3.1 If yes, go to step 1.
    
    3.2 If no, then go to step 4.

    Note: We initialize this with `enough=√N` (So, in our case, this will be `enough=√16=4`)

4. Does the current split minimize the expected value of the $standard$ $deviation$ of both `left` and `right`. 
    4.1 If yes, save the current  split point
    4.2 If no, go to step 1.

So, at `i=4`

```
arr = [1,1,1,1,3,4,4,3,3,5,3,2,4,3,2,6]
              ^
            i = 4

left = [1,1,1,1]
right = [3,4,4,3,3,5,3,2,4,3,2,6]

exp_std_left = len(left) * stdev(left) / len(arr)
exp_std_right = len(right) * stdev(right) / len(arr)

exp_std = exp_std_left + exp_std_right
```

Once we have found the split, add a column to the original dataset denoting which split a row falls into.


## In code

```lua
function label(data,  enough,rows, most,cohen)
  rows = data.rows
  enough = (#rows)^Lean.label.enough

  local function band(c,lo,hi)
    if lo==1 then
      return "..".. rows[hi][c]
    elseif hi == most then
      return rows[lo][c]..".."
    else
      return rows[lo][c]..".."..rows[hi][c] end
  end

  local function argmin(c,lo,hi,     l,r,cut,best ,tmp,x)
    if (hi - lo > 2*enough) then
      l,r = num(), num()
      for i=lo,hi do numInc(r, rows[i][c]) end
      best = r.sd
      for i=lo,hi do
        x = rows[i][c]
        numInc(l, x)
        numDec(r, x)
        if l.n >= enough and r.n >= enough then
          if l.hi - l.lo > cohen then
            if r.hi - r.lo > cohen then
            tmp = numXpect(l,r) * Lean.label.margin
            if tmp < best then
               cut,best = i, tmp end end end end end end
    return cut
  end

  local function mark(c, lo,hi,   b)
    b= band(c,lo,hi)
    for r=lo,hi do rows[r][c+1]=b end
  end

  local function cuts(c,lo,hi,pre,       cut,txt,b)
    fyi(pre .. rows[lo][c])
    if hi - lo > enough then
      cut = argmin(c,lo,hi,cohen)
      if cut then
        return cuts(c,cut+1,hi, pre.."|..") end end
    mark(c,1,lo-1)
    mark(c,lo,hi)
  end

  local c=#data.name
  ksort(c,rows)
  local all = num()
  for i=1,#data.rows  do numInc(all, rows[i][c]) end
  cohen = all.sd*Lean.label.cohen
  fyi("\n-- ".. data.name[c] .. "----------")
  cuts(c,1,#data.rows,"|.. ")
  print(cat(data.name,", ") .. ",!klass" )
  dump(rows)
end
  ```
