----
### Pipe
$ [monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

### Help
+ [Table Format](table.md)
+ [Tips on debugging](debug.md)
+ [Codeanywhere](codeanywhere.md)
----

# Table Format

Let's look at an example of tables we would be handling

```
 ?id, outlook, $temp, >humid, wind, !play
 1, over,	64,	65,	TRUE,	yes
 2, over,	64,	65,	TRUE,	yes
 3, over,	72,	90,	TRUE,	yes
 4, over,	83,	86,	FALSE,	yes
 5, sunny,	69,	70,	FALSE,	yes
 6, sunny,	69,	70,	FALSE,	yes
 7, rainy,	65,	70,	TRUE,	no
 8, sunny,	75,	70,	TRUE,	yes
 9, sunny,	75,	70,	TRUE,	yes
 10, sunny,	85,	85,	FALSE,	no
 11, rainy,	71,	91,	TRUE,	no
 12, rainy,	70,	96,	FALSE,	yes
 13, rainy,	70,	96,	FALSE,	yes
```

Note that first row describes each columns. The special sympols `$<>!` will be defined below.

## 1. Header

In general,
- Tables have one column per feature and one row per example.
- The columns may be numeric (have numbers) or symbolic (contain discrete values).
- Also, some columns are goals (things we want to predict using the other columns).

To denote these we use the following prefixs with the name of the columns to determine 

 - `'<'` is a dependent goal to be maximized (it is also numeric);
 - `'>'` is a dependent goal to be minimized (it is also numeric);
 - `'$'` is an independent  numeric column;
 - `'!'` is a class column (and is not numeric).
 - ` ` is a independent symbolic column
 - `?` is a column that has to be ignored
 
 So in the table example above, 
 
 - `outlook` is a *symbolic* independent variable
 - `$temp` is a *numeric* independent variable
 - `>humid` is a *numeric* goal variable
 - `wind` is a *symbolic* independent variable
 - `!play` is a *non-numeric* class column
 - `?id` is the row number that has to be *ignored*

## 2. Rows

Once we have determined the header, we can now add the rows according the definition

We need methods for `nums`, `syms`. Once we have these, we can start adding rows using the following steps

1. Read every column header
2. If the prefix is a `$`, `>`, or `<` then add them as a number
3. Else, if it is `?` then skip that column
4. Finally, add it as a `sym`

In code, 
```lua
function row(t,cells,     x,r)
  r= #t.rows+1
  t.rows[r] = {}
  for c,c0 in pairs(t._use) do
    x = cells[c0]
    if x ~= "?" then
      if t.nums[c] then 
	      x = tonumber(x)
        numInc(t.nums[c], x)
      else
	      symInc(t.syms[c], x)
    end end
    t.rows[r][c] = x  end
  return t
end  
```
