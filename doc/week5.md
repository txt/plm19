<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>


# Review, week5

## Patterns

### Proxy
### Layers

e,g, Data-Model-Dialog

### Model View controller

### Subject Observor

### Blackboard

Layers + Subject-Observor

## Unification

A wide range of tasks typically assinged to complex theorem proving algorithms can be implemented 
by a single _unification_ algorithm.
     - Enter Logic Programming.

- 1950s: lets build parse trees!
- 1960, LISP, computation= rewrite parse trees from bottom to top

```
; input
(+ 
  (* 2 
     (/ 4 5))
  (+ 10
     1))

; first rewrite
(+ 
  (* 2 
     0.8)
  (+ 10
     1))

; second rewrite
(+ 
  1.6
  (+ 10
     1))

; third rewrite
(+ 
  1.6
  11)

; fourth rewrite

12.6
```

- 1970s: Prolog: comptuation = find bindings that let you match trees
