# 1. The Unix Philosophy

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

## 1.1 Uniform interface

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
ASCII text.



# References

[1] Martin Kleppmann: “Kafka, Samza, and the Unix Philosophy of Distributed Data,” martin.kleppmann.com, August 5, 2015.

[2] Doug McIlroy: Internal Bell Labs memo, October 1964. Cited in: Dennis M. Richie: “Advice from Doug McIlroy,” cm.bell-labs.com.

[3] M. D. McIlroy, E. N. Pinson, and B. A. Tague: “UNIX Time-Sharing System: Foreword,” The Bell System Technical Journal, volume 57, number 6, pages 1899–1904, July 1978.

[4] Eric S. Raymond: The Art of UNIX Programming. Addison-Wesley, 2003. ISBN: 978-0-13-142901-7
