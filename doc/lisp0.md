

<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>

# Lisp exercise 1

Zero marks tutorial


## Add three numbers


```
sudo apt-get install clisp
chmod +x lisp0
./zero.lisp
```

zero.lisp:
```
#!/usr/bin/env clisp
;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

(defun test1()
  (let ((x 0))
    (dolist (y '(1 2 3) x)
      (setf x (+ x y)))))

  (print (test1))
```

## Great intro Tutorial

https://learnxinyminutes.com/docs/common-lisp/

Read the above tutorial. Wr

- atom
- s-expression (aka list)
- quote
- strings
- symbols
- characters
- cons
- array
- hash
- lambda
- defun
- defmacro


