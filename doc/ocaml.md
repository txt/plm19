



<a href="http://tiny.cc/plm19"><img width=1000 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png"></a><br>
&nbsp;<a href="http://tiny.cc/plm19">home</a> |
<a href="https://github.com/txt/plm19/blob/master/doc/syllabus.md">syllabus</a> |
<a href="https://github.com/txt/plm19/tree/master/src">src</a> |
<a href="http://tiny.cc/plm19give">submit</a> |
<a href="https://plm19.slack.com/">chat</a> |
<a href="https://github.com/txt/plm19/blob/master/LICENSE.md">&copy;2019</a> 
by <a href="http://menzies.us">Tim Menzies</a>




# OCAML

OCaml is a strictly evaluated functional language with some imperative features.
F# is also heavily influenced by OCaml.

```
let rec factorial n =
    if n = 0 then 1
    else n * factorial (n-1)
;;
```

Pure function language. Functions define a relationship between input types and output types.

A strongly typed language (at compile time, you know if a variable is a string, number, whatever) and supported type inferencing (so you can check if you are doing dumb things like adding a number to a string).

When you are in the top level loop, OCaml will print the inferred type after you enter an expression.

```ocaml
# let inc x = x + 1 ;;
val inc : int -> int = <fun> # the type signature is inferred
# let a = 99 ;;
val a : int = 99 # the type signature is inferred
```

For a source file you can use “ocamlc -i /path/to/file.ml” command to print all names and type signatures.

```ocaml
$ cat sigtest.ml
let inc x = x + 1
let add x y = x + y

let a = 1

$ ocamlc -i ./sigtest.ml
val inc : int -> int
val add : int -> int -> int
val a : int
```
OCaml is a wonderful language.
Instead of using manually written type annotations, it infers types of expressions using Hindley-Milner algorithm. It makes type annotations unnecessary in most cases, but can be a major source of confusion for beginners.
That said, not too popular:

![](../etc/img/ocaml.png)


OCaml, like many functional lanugages,
 supports recursive type definitions. Question: is the following a type system or a grammar or both?

```
type paragraph =
    Normal of par_text
  | Pre of string * string option
  | Heading of int * par_text
  | Quote of paragraph list
  | Ulist of paragraph list * paragraph list list
  | Olist of paragraph list * paragraph list list

and par_text = text list

and text =
    Text of string
  | Emph of string
  | Bold of string
  | Struck of par_text
  | Code of string
  | Link of href
  | Anchor of string
  | Image of img_ref

and href = { href_target : string; href_desc : string; }

and img_ref = { img_src : string; img_alt : string; }
```
The Ulist and Olist constructors take the first item followed by a (possible empty) list of items, to prevent empty lists --- this way, there's at least one element.

So eigenclass.org uses the above to define a cool generator for HTML pages using a set of short-cuts:

+ any character can be escaped with \,
+ emphasis is done with __ (less prone to accidental use than _) and bold text with *,
+ typographical abuses like bold emphasized text are not allowed,
+ headers are done with !level1 header, !!level2 header, etc.,
+ the # character is thus free and can be used for numbered lists, replacing 1.,
+ pre-formatted code is done with
```
{{
  whatever
}}
```
It is possible to extend the markup with custom processors, using 
_{{extension-name_ blocks; for instance, raw HTML is inserted with
```
{{html
  <b> whatever </b>
}}
```
(before you try to inject arbitrary HTML in the comments: this extension is only enabled in the main text).

The above grammar is the core of a parser that generates HTML. And, interestingly, in this ultra-high level language, this parse runs very very fast.
```
                                      runtimes                       memory
                                      -----------------------------  -------------
                          LoCs        README.1  README.8  README.32  README.32 MEM
discount         C        ~4500                 0.016s    0.063s     2.8MB
Bluecloth        Ruby     1100        0.130s    2.16s     30s        31MB
markdown         Perl     1400        0.068s    0.66s     segfault   segfault
python-markdown  Python   1900        0.090s    0.35s     2.06s      23MB
Pandoc           Haskell   900 + 450  0.068s    0.55s     2.7s       25MB
----------------------------------------------------------------------------------
Simple_markup    OCaml    313 + 55              0.012s    0.043s     3.5MB
```
So, in the 21st century, you can have you cake and eat it too:

+ High-level descriptions.
+ Tiny code.
+ Fast runtimes.

Excellent example of how pure theory improves implementations.

 
