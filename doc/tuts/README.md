[monte_carlo](monte_carlo.md) | [brooks2](brooks.md) | [dom](dom.md) | [bestrest](bestrest.md) | [super](super.md) | [rank](rank.md)

----

# A Data Mining Pipeline

## 1. Why Pipes?

<img src="https://alvinalexander.com/images/fp-book/unix-pipelines/1-unix-pipes-solution.png" align=right>
Using a chain of commands like we do in the project in fact one of the key design ideas of
Unix, and it remains astonishingly relevant today. 

Let’s look at it in some more depth so that we can borrow some ideas from Unix [1].

Doug McIlroy, the inventor of Unix pipes, first described them like this in 1964 [2]:
“We should have some ways of connecting programs like [a] garden hose—screw in
another segment when it becomes necessary to massage data in another way. This is
the way of I/O also.” The plumbing analogy stuck, and the idea of connecting programs
with pipes became part of what is now known as the Unix philosophy—a set of
design principles that became popular among the developers and users of Unix. The
philosophy was described in 1978 as follows [3, 4]:
    
1. Make each program do one thing well. To do a new job, build afresh rather than complicate old programs by adding new “features”.

2. Expect the output of every program to become the input to another, as yet unknown, program. Don’t clutter output with extraneous information. Avoid
stringently columnar or binary input formats. Don’t insist on interactive input.

3. Design and build software, even operating systems, to be tried early, ideally within weeks. Don’t hesitate to throw away the clumsy parts and rebuild them.

4. Use tools in preference to unskilled help to lighten a programming task, even if you have to detour to build the tools and expect to throw some of them out after you’ve finished using them.

This approach—automation, rapid prototyping, incremental iteration, being friendly to experimentation, and breaking down large projects into manageable chunks — sounds remarkably like the Agile and DevOps movements of today. Surprisingly little has changed in four decades.

A Unix shell like bash lets us easily compose these small programs into surprisingly powerful data processing jobs. Even though many of these programs are written by different groups of people, they can be joined together in flexible ways. 

We'll see what what Unix does to enable this composability.

### 1.1 Uniform interface

If you expect the output of one program to become the input to another program,
that means those programs must use the same data format—in other words, a compatible
interface. If you want to be able to connect any program’s output to any program’s
input, that means that all programs must use the same input/output interface.

In Unix, that interface is a file (or, more precisely, a file descriptor). A file is just an
ordered sequence of bytes. Because that is such a simple interface, many different
things can be represented using the same interface: an actual file on the filesystem, a
communication channel to another process (Unix socket, stdin, stdout), a device
driver (say `/dev/audio` or `/dev/lp0`), a socket representing a TCP connection, and so
on. It’s easy to take this for granted, but it’s actually quite remarkable that these very
different things can share a uniform interface, so they can easily be plugged together.ii

By convention, many (but not all) Unix programs treat this sequence of bytes as
ASCII text. Input file is treated as a list of records separated by the \n (newline, ASCII 0x0A) character.
The choice of `\n` is arbitrary.

The parsing of each record (i.e., a line of input) is more vague. Unix tools commonly
split a line into fields by whitespace or tab characters, but CSV (comma-separated),
pipe-separated, and other encodings are also used. Even a fairly simple tool like
xargs has half a dozen command-line options for specifying how its input should be
parsed.

Although it’s not perfect, even decades later, the uniform interface of Unix is still
something remarkable. Not many pieces of software interoperate and compose as
well as Unix tools do: you can’t easily pipe the contents of your email account and
your online shopping history through a custom analysis tool into a spreadsheet and
post the results to a social network or a wiki. Today it’s an exception, not the norm,
to have programs that work together as smoothly as Unix tools do.

## 1.2 Separation of logic and wiring

Another characteristic feature of Unix tools is their use of standard input (`stdin`) and
standard output (`stdout`). If you run a program and don’t specify anything else,
stdin comes from the keyboard and stdout goes to the screen. However, you can
also take input from a file and/or redirect output to a file. Pipes let you attach the
stdout of one process to the stdin of another process (with a small in-memory
buffer, and without writing the entire intermediate data stream to disk).

A program can still read and write files directly if it needs to, but the Unix approach
works best if a program doesn’t worry about particular file paths and simply uses
`stdin` and `stdout`. This allows a shell user to wire up the input and output in whatever
way they want; the program doesn’t know or care where the input is coming
from and where the output is going to. Separating the input/output wiring
from the program logic makes it easier to compose small tools into bigger systems.

## 2. What's this project about?

The goal of this project is to write your own programs and combine them with the tools provided
by the operating system. Your program just needs to read input from stdin and write
output to stdout, and it can participate in data processing pipelines.

```make
all: eg1

define hi
  echo "\n### $@ ######################\n"
endef

some=cut -d, -f 4,5,8,9 | sort -t, -n -k 4  | sed 's/,/,	/g'

Auto=cat $(Test)/data/auto.csv
Auto10=cat $(Test)/data/auto10000.csv
Auto1M=cat $(Test)/data/auto1M.csv
Weather=cat $(Test)/data/auto.csv
DSL= dsl/monte_carlo 1| dsl/brooks2

ok:; @bash $(Etc)/lua2bin

eg0:  ok; @$(hi); $(DSL) | dom | bestrest | super | rank
eg0a: ok; @$(hi); $(DSL) | dom | bestrest
eg0b: ok; @$(hi); $(DSL) | dom | bestrest |super
eg1:  ok; @$(hi); cat $(Test)/data/weather.csv | dom
eg2:  ok; @$(hi); $(Auto) | dom | $(some); $(Auto) | head -1 | $(some)
eg3:  ok; @$(hi); $(Auto) | dom | bestrest
eg4:  ok; @$(hi); $(Auto) | dom | bestrest | super
eg5:  ok; @$(hi); $(Auto) | dom | bestrest | super | rank
eg6:  ok; @$(hi); $(Auto10) | dom | bestrest | super | rank
eg7:  ok; @$(hi); $(Auto1M) | dom | bestrest | super | rank
```

Now if you type

      make 
      
it will run the first rule

       all: eg1

which in turn will run

        eg1:  ok; @$(hi); cat $(Test)/data/weather.csv | dom
        
        
(Hint: so to change the behaviour of the default `nake` change, change the first rule.)
 
Note that all the `eg*` rules are `pipes` where stuff from one source is filtered to become the source for the next item in the pipe.

Your task: 

- Understand each part of this pipeline,
  its inputs and outputs,
- Then replace two, maybe three, of  filters with something coded in another language.

Note one debugging step. If you want to understand
what something does within a pipeline, run the pipe
just up to and after it. For example, to understand
`dom` in this pipeline...

    monte_carlo | brooks2 | dom | bestrest| super | rank

first run

    monte_carlo | brooks2 

to work out the input to `dom`. Then run

    monte_carlo | brooks2 | dom 

to see the outout from dom


To make that easier, try this:

    monte_carlo | brooks2  | head20 > b4dom.txt

the 

    monte_carlo | brooks2 | dom > afterdom.txt

then diff the two files to see what is going on

    diff b4dom.txt afterdom.txt

     

# References

[1] Martin Kleppmann: “Kafka, Samza, and the Unix Philosophy of Distributed Data,” martin.kleppmann.com, August 5, 2015.

[2] Doug McIlroy: Internal Bell Labs memo, October 1964. Cited in: Dennis M. Richie: “Advice from Doug McIlroy,” cm.bell-labs.com.

[3] M. D. McIlroy, E. N. Pinson, and B. A. Tague: “UNIX Time-Sharing System: Foreword,” The Bell System Technical Journal, volume 57, number 6, pages 1899–1904, July 1978.

[4] Eric S. Raymond: The Art of UNIX Programming. Addison-Wesley, 2003. ISBN: 978-0-13-142901-7
