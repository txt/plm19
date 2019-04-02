<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week9

## Rudy

Write the following in Smalltalk, then LISP:

```ruby
(1..5).each do |counter|
  puts "iteration #{counter}"
end
```

```ruby
array = [1,2,3,4,5]
doubled = array.map do |element|
  element * 2
end
puts doubled
```

In the following, 

- list the instance variables, the class variables, the _defined_ methods and the _called_ methods.
- Exactly when is `@@instance` created?

```Rudy
class SimpleLogger

  # Lots of code deleted...
  @@instance = SimpleLogger.new

  def self.instance
    return @@instance
  end

end
```

Rewrite the following in Python. Hint, `@level` is instance variable you will need to
create in an `init` method and you'll have to add in some `self` references. 

```ruby
class SimpleLogger
  def warning(msg)
    @log.puts(msg) if @level >= WARNING
    @log.flush
  end

  def info(msg)
    @log.puts(msg) if @level >= INFO
    @log.flush
  end
end
```



## Patterns

- Q:"Expert are experts sicne they see patterns that novices do not". Explain.
- Q: "Patterns + macros = new language". Discuss
- Q: Discuss the merits of designning using the Composite pattern for a class hierarchy browser (single-parent) and disk file browser .
       - In your answer, make sure you define what is a composite pattern
       - What else could a composite be used for?
- Q:For Abstract Factory : [learn more](https://refactoring.guru/design-patterns/abstract-factory)
       - Is it creational, strutural or behavioural? Justify your answer.
- Q: Consider the Signelton Pattern: [learn more](https://github.com/davidgf/design-patterns-in-ruby/blob/master/singleton.md)
       - What invariant does Singleton maintain?
       - Singletons have recenly inspred much strong hatred. What do some people as the problem with Singletons?
       - How does the following Ruby code maintain the Singleton invariant?

Code:
```ruby
class SimpleLogger

  # Lots of code deleted...
  @@instance = SimpleLogger.new

  def self.instance
    return @@instance
  end

  private_class_method :new
end
```

- Q: Consider the Adapter Pattern:
	- The following code allows for a uniform access protocol to strings and files. Define the methods that make up that uniform access protocol.
	- With this protocol in place, and the following code, what can you now do with a string that is the same as a file.

```ruby
class Encrypter
  def initialize(key)
    @key = key
  end

  def encrypt(reader, writer)
    key_index = 0
    while not reader.eof?
      clear_char = reader.getc
      encrypted_char = clear_char ^ @key[key_index]
      writer.putc(encrypted_char)
      key_index = (key_index + 1) % @key.size
    end
  end
end

class StringIOAdapter
  def initialize(string)
    @string = string
    @position = 0
  end:w


  def getc
    if @position >= @string.length
      raise EOFError
    end
    ch = @string[@position]
    @position += 1
    return ch
  end

  def eof?
    return @position >= @string.length
  end
end

encrypter = Encrypter.new('XYZZY')
reader= StringIOAdapter.new('We attack at dawn')
writer=File.open('out.txt', 'w')
encrypter.encrypt(reader, writer)
```

## The Pipeline

### MonteCarlo

Q: What is the purpose of the Monte Carlo function?    
A: Provide a range of poissibiltiies, selected at random, for control parameters of a simulation?


- Q:What is the input to the `monte\_carlo`  carlo function?  (Hint: `n` and `s`).
- Q:What is the output?
- Q:What is a psuedo-number generator and why is useful for debugging?
- Q:What is the seed of the psuedo-number generator?


### Compartmental Model

Q:What is the puprose of the compartmental model `brooks2` code in _the pipe_?

In the following:

- Q:What are the _payloads_?
- Q:What are the `stocks` in the following?
- Q:What is invariant about `i` and `j`? What is different about `i` and `j`?
- Q:When this runs, what does it output to the next step in the pipe?
- Q:What is its input (from _the pipe_).

```
 def step(self, dt, t, i, j):
        def _co(x):
            "Communication overhead"
            myTeam = i.ts - 1   # talk to everyone in my team
            others = x / i.ts - 1  # talk to every other team
            return self.params.pomposity * (myTeam**2 + others**2)  # pomposity
        j.aR = i.np / self.params.learning_curve  # 20 = Learning curve
        j.ps = self.params.optimism * t  # Optimism
        j.co = _co(i.ep + i.np)
        # Don't touch 6 and zero.
        j.paR = 6 if (
            i.ps - i.d) < self.params.atleast and t < int(self.params.done_percent * t / 100) else 0
        j.sdR = i.nprod * (1 - i.co / 100) * (self.params.productivity_new
                                              * i.np + self.params.productivity_exp * (i.ep - i.ept))
        j.ept = i.np * i.to / 100
        j.ep += i.aR * dt
        j.np += (i.paR - i.aR) * dt
        j.d += i.sdR * dt
        j.r -= i.sdR * dt
        return j
```

In the code that calls `step`, there is some additional functionality.
Why is there a `restrain` method?

```python
    def restrain(i, x):
        return max(i.lo, min(i.hi, x))
```

### Rows

One abstraction used to connect all the filters in _the pipe_ are `tables`.
Tables are read from a `csv` file. 

- Q:What is different about the first row and all the other rows?
- Q:What are the special characters in that first row? (Hint _><$!_)
- Q:Using that first row, how to distinguish a dependent from an independent column?
- Q:Using that first row, how to distinguish a numeric from a symbolic column?

Here is the code inside the `Num` header. What does it do? When is it called?
What is the point of the test for _?_ in the first line?

```lua
function numInc(t,x,    d) 
  if x == "?" then return x end
  t.n  = t.n + 1
  d    = x - t.mu
  t.mu = t.mu + d/t.n
  t.m2 = t.m2 + d*(x - t.mu)
  if x > t.hi then t.hi = x end
  if x < t.lo then t.lo = x end
  if (t.n>=2) then 
    t.sd = (t.m2/(t.n - 1 + 10^-32))^0.5 end
  return x  
end
```

### DOM

- Q:What is the purpose of the _dom_ part of the pipleine?
- Q:Imagine the dependent variables in two rows of data. 
We say these variables have a _weight_ of -1 or 1. What does -1 indicate?
- Q:Here is the dom function for those two rows. What is _a,b_ normalized before we look at their difference?

```lua
function dom(t,row1,row2,     n,a0,a,b0,b,s1,s2)
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

Using this function, we can compute the `dom` for a row by checking how often it dominates other rows.  
In my dom.lua, I don't compare it each row to all others. Instead I only comapre it to `Lean.dom.samples` others

- Q:How are those other samples selected?
- Q:Why don't I comapre against all?
- Q:My _Dom_ score is `1/n` where `n=Lean.dom.samples`. Why?

### BestRest

Q: What is the purpose of `bestrest`?
A: Replace all numbers in the goal column with two synbols: best or rest

Q:What  are the inputs, outputs of the BestRest part of the pipe?

- A: Input, a table with all numerics
- A: Output, same table, with the last column split into best and rest using the following rules.

Note that the output has no numerics in the goal column.

About Discretization:

- Q:What is discretization?
      - Why not discretize with (e.g.) `(max-min)/10`?
- Q:If we have 900 numbers and we declare that `enough` best examples is square root of the number of rows, who many bests and rests exampels are there?
      - If that number is too small, there are two ways to increase it. Name them.

Aside: time for some theory. To split a list of numbers into a set of bins:

- Before doing anything else,  determine:
      - _width_ = Minimum bin size. My code assumes sqrt(examples)
      - _height_ = Minimum acceptable difference between bin start and bin end. My code sets hieght to 0.3*standardDeviation of examples
      - what a measure of diversity? My code uses standard deviation.
      - what is a trivial difference in diversity? My code says les than 1%.
- Then, first
      - Sort the numbers
      - set width, height
- Second,
      - We say a _cut_ is a split of those _n_ numbers into  bins of size _n1,n2_ with diversity _d1,d2_
        and expected value of the diversity after the cut of `n1/n*d1 + n2/n*d2`.
      - We say a  _good cut_ is a split where _n1 > width_ and _n2 > width_ and the (max-min) value of each cut is over _height_..
      - Find the _good cut_ that minimizies the expected diverity (and we say that two expected diversities are the same if they
        are only trivially different).
      - If it exists, take each half and recurse (start this second step again)

The resulting devisions are then labelled:

- ..m for the bottom cut
- m.. for the top cut
- m..n for the in-between cuts 

### Super

Q: What is the purpose of `super`?
A: Replace all numbers in the goal column with two synbols: best or rest


- Q: What is the purpose of the _super_  part of the pipeline?
- Q: What are its inputs and outputs:
     - A: Input, a table with independent numerics
     - A: Output, same table, independent numerics discretize such that each bin minimizes the diversity of that attribute (providing that split
       also reduces the diversity of the goal column).
     - This is called _supervised_ discretization sicne it takes the goal column into account.

Note that there are no numbers in the indpendent or depedent columns... just lots of bins.

Note: this works as with _bestrest_ but now we reflect on the goal well. Also, since out goal is now two symbols _best_ and _rest_, our
measure for goal diversity is now _entropy_ which for sample contain n1 this and n2 that is

- p1=n1/(n1+n2)
- p2=n2/(n1+n2)
- entropy = -p1*log2(p1) - p2*log2(p2)


### Rank

Q: What is the purpose of `rank`?
A: List all the independent  bins in order of how well they select for the `best` goal.

The `main` function of `rank` first  generates `all` and `both`, which are both tables, but

- `all` assumes are the rows are in one class
- `both` assumes the rows are divided into `best` and `rest`

Then the rows holding best and rest are extracted from `both`. These
are stored in the `best` and `rest` lists, which are of length `nb` and `nr` respectively.

Next, we run over all the independent bins and ask how many `b` best and `r` rest rows are found in each bin.
From that we can generate the _score_ for that bn:

- _pb = b/nb_
- _pr = r/nr_
- _score = 0 if pb <= pr else pb^2/(pb+pr)_

(The _pb^2_ terms rewards things most common in best. And of _pb <= pr_ then we don't want to know about that range.)

## Make

In the following:

- List the variables
- List the targets that are files
- List the targets that are `phonies`; i.e. not targets
- In the rule `all: monte_pi_sprng` what is the value of `$@` and `$<`?
- In the `monte_pi_sprng.o` rule , what is the `target`, the `prerequisties` and the `command`.
- What is the default target executed if you just type `make`?
- If you track pack through all the dependancies there is an opportunity to speed up (a small part of) the following. Which part of
  the following could benefit from having two cores on one machine?

```make
CC=cc
CFLAGS=  -O3 -I /usr/local/lib/sprng/include -I /usr/local/lib/pgplot -g
OBJECTS= monte_pi_sprng.o plot.o
LIBS = -L/usr/local/lib/sprng/lib -llcg -L/usr/local/lib/pgplot -lcpgplot -lpgplot -lX11 -lftn -lm


# --- targets
all:    monte_pi_sprng
monte_pi_sprng:   $(OBJECTS) 
       $(CC)  -o monte_pi_sprng  $(OBJECTS) $(LIBS)
        
monte_pi_sprng.o: /usr/local/lib/sprng/include/sprng.h /usr/local/lib/pgplot/cpgplot.h monte_pi_sprng.c
       $(CC) $(CFLAGS) -c monte_pi_sprng.c
       
plot.o: /usr/local/lib/pgplot/cpgplot.h plot.c
       $(CC) $(CFLAGS) -c plot.c 


# --- remove binary and executable files
clean:
       rm -f monte_pi_sprng $(OBJECTS)
```

In the following, the goal is to find all the soruce files (in `$(Src)` and extract the comments from each.
But there is a subtle error. What is it? How to fix it?

```
countFiles:
	cd $(Src)
	echo -n "$(PWD)'s number of files is:"
	ls | wc -l
```

In the following:

- What is the role of the `%` character?
- What does the `patsubst` do on the first line?
- If I changed directories and want to build object files from my c code in that new directory,
  what would need to be changed? (Hint: trick question).

```
FILENAME:=  $(patsubst %.c,%.o,$(wildcard *.c))

all:$(FILENAME)
    @echo $(FILENAME)

%.o : %.c
    gcc -c   $< -o $@
```


