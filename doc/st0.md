<a href="http://tiny.cc/plm19">home</a> ::
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> ::
<a href="https://github.com/txt/plm19/tree/master/src">src</a> ::
<a href="http://tiny.cc/plm19give">submit</a> ::
<a href="https://plm19.slack.com/">chat</a> ::
<a href="https://github.com/txt/plm19/blob/master/license.md">&copy;</a> 2019, <a href="http://menzies.us">timm</a>
<br>
<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a>

# Beginner Smalltak

This homework is optional and wills core you zero marks.

But it will get you started with Smalltalk.

On codeanywhere:

    sudo apt-get update
    sudo apt-get install gnu-smalltalk

Test it out

    $ gst
    GNU Smalltalk ready

    st> 1+1.
    2
    st> control-d
    $

Working!

Now split your environment into a shell (on left) and editor (on right). Edit the file `st101`
     
     #!/usr/bin/env gst
     
     "things in quotes are comments"
     
     Transcript nextPutAll: 1 printString; 
                cr " command are grouped by '!'"
     !
     
     ! Object methods !
     oo
       Transcript nextPutAll: self printString; 
                  cr!
     !  
     
     | x y | "locals vars. live till next '!'"
     x := 1.
     y:=2.
     (x+y) oo  "should print 3"
     !
     
     "now go to 
      https://people.eecs.berkeley.edu/~fateman/264/papers/smalltalk-tutorial.html 
      and try the examples there in section 1 and 2"
     
     "e.g. 1.2"
     
     'Hello, world' oo!

Remember to `chmod +x st101` before running it with

      ./st101  


