;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

#|
In this assignment you will use the LISP macro system
to learn that "objects" can be viewed as macros that
generate "lambdas".

To find what you need to do, look for TODOx"

When its all done, you should see output like this:

    > clisp oo1d.lisp
    (ENCAPSULATON 105.0 110.25 110.25)
    (ENCAPSULATION 90.25)
    (ENCAPSULATION 70.25)
    (ENCAPSULATION 50.25)
    (ENCAPSULATION 30.25)
    (ENCAPSULATION 10.25)
    (ENCAPSULATION -9.75)
    (ENCAPSULATION -29.75)
    (ENCAPSULATION -49.75)
    (ENCAPSULATION -69.75)
    (ENCAPSULATION -89.75)

    (POLYMORPHISM 115.7079632679489662L0)

    (INHERITANCE 100 105.0 110.25 110.25 1)
    (INHERITANCE 90.25)
    (INHERITANCE 70.25)
    (INHERITANCE 50.25)
    (INHERITANCE 30.25)
    (INHERITANCE 10.25)
    (INHERITANCE INSUFFICIENT-FUNDS)
    (INHERITANCE INSUFFICIENT-FUNDS)
    (INHERITANCE INSUFFICIENT-FUNDS)
    (INHERITANCE INSUFFICIENT-FUNDS)
    (INHERITANCE INSUFFICIENT-FUNDS)

    (META ((ID . 2) (INTEREST-RATE . 0.05) (BALANCE . 0) (NAME)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BACKGROUND:

In the following you need to debug some macro expansions. One tool
you can use for that is "xpand"  For example, LISP's "case" statement
is a macro that expands into a "cond". e.g.

    (xpand '(case x ((1 2 3) (print 'num)) 
                      ((a b c) (print 'sym)) 
                      ("string" (print 'string)) 
                      (otherwise (print 'dull))))

    (let ((#:case-key-3001 x))
     (cond
      ((or (eql #:case-key-3001 '1)
           (eql #:case-key-3001 '2)
           (eql #:case-key-3001 '3))     (print 'num))
      ((or (eql #:case-key-3001 'a)
           (eql #:case-key-3001 'b)
           (eql #:case-key-3001 'c))     (print 'sym))
      ((eql #:case-key-3001 '"string")   (print 'string))
      (t                                 (print 'dull))))

|#

(defun xpand(x)
  (terpri) 
  (write 
    (macroexpand-1 x) :pretty t :right-margin 40 :case :downcase)
  (terpri))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ENCAPSULATION

The following code calls the defthing macro to return a function
that can create an "account".

    (defthing
      account
      :has  ((name) (balance 0) (interest-rate .05))
      :does ((withdraw (amt)
                       (decf balance amt))
             (deposit (amt)
                      (incf balance amt))
             (interest ()
                       (incf balance
                             (* interest-rate balance)))))
    
"defthing" creates a function that returns a lambda body.
This lambda body holds a case statement which, when called
with a message, returns another lambda body that can do something (run
some code or return a value). For example, from the above
defthing, the following function is generated:

    (defun account (&key (name) (balance 0) (interest-rate .05))
     (lambda (#:message2822)
      (case #:message2822
       (withdraw
        (lambda (amt) (decf balance amt)))
       (deposit
        (lambda (amt) (incf balance amt)))
       (interest
        (lambda nil
         (incf balance
          (* interest-rate balance))))
       (name (lambda nil name))
       (balance (lambda nil balance))
       (interest-rate
        (lambda nil interest-rate)))))

Note the jump from the input form to the output form.
The forms "(name)" and "(balance .05)" expands into two
accessor lines (within the case statement.

    (name    (lambda nil name))
    (balance (lambda nil balance))

which has the general form:

    (slotname (lambda nil soltname))

The forms "(name)" and "(balance .05)" also expands into
init form in the header of the function.

    (defun account (&key (name) (balance 0) (interest-rate .05))
       ...
    )

which has the general form:

    (defun account (&key INITS)
       ...
    )

The code for this is as follows (but "methods-as-case"
and "datas-as-case" is missing... till you write it.

|#

(defun send (obj mess &rest args) 
  (apply (funcall obj mess) args))

(defmacro defthing (klass &key has does)
  (let* ((message (gensym "MESSAGE")))
    `(defun ,klass (&key ,@has) 
       (lambda (,message)
         (case ,message
           ,@(methods-as-case does)
           ,@(datas-as-case (mapcar #'car has)))))))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
1. Make defthing work

TODO 1a. Why does mapcar call #'car over the "has"?
TODO 1b. Why is message set to a gensym?
TODO 1c. Implement "data-as-case": 

    (datas-as-case '(name balance interest-rate))
    ==>
    ((NAME (LAMBDA NIL NAME)) 
     (BALANCE (LAMBDA NIL BALANCE)) 
     (INTEREST-RATE (LAMBDA NIL INTEREST-RATE)))
    
1d. Implement  "methods-as-case"

     (methods-as-case '((more (x) (+ x 1)) (less (x) (- x 1))))
     ==>
     ((MORE (LAMBDA (X) (+ X 1))) 
      (LESS (LAMBDA (X) (- X 1))))
     

Now that that is working, the following should
expand nicely:
|#

; but first, uncomment this code
'(defthing
  account
  :has  ((name) (balance 0) (interest-rate .05))
  :does ((withdraw (amt)
                     (decf balance amt))
         (deposit (amt)
                  (incf balance amt))
         (interest ()
                   (incf balance
                         (* interest-rate balance)))))

#|

TODO 1e. Show the result of expanding you account.
|#

; uncomment this to see what an account looks like
'(xpand (account))

#|
1f. Fix "withdraw" in "account" such that if you withdraw more than
what is there, it  returns the symbol 'insufficient-funds
 
TODO 1f.. Show the output from the following function

|#

(defun encapsulation ()
   (let ((acc (account :balance 100)))
      (print `(encapsulaton 
                  ,(send acc 'interest)
                  ,(send acc 'interest)
                  ,(send acc 'balance)))
      (dotimes (i 10)
         (print `(encapsulation 
                    ,(send acc 'withdraw 20))))
      ))


; to run encapuatlion, uncomment the following
'(encapsulation)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POLYMORPHISM

TODO 2a. Define an object "cirle" with variables x,y
    (for  the center of the circle) and radius 
    (to hold the size of the circle). Add a method 
    "area" that returns 2 *pi*radius^2

; run this to peek inside circle
'(xpand (circle))

TODO 2b. Define an object "rectangle" with variables x1,x2,y1,y2
    that all default value of 0. Add
    a method "area" that returns the area of that rectangle
TODO 2c. Show the output from the following test

|#

(defun polymorphism()
  (let ((sum 0)
        (all (list (circle :radius 1) 
                   (rectangle :x2 10 :y2 10)
                   (circle :radius 2))))
    (dolist (one all)
      (incf sum (send one 'area)))
    (print `(polymorphism ,sum))))

; to run, uncomment the following
'(polymorphism)

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Inheritance


3. To  make inheritance work, we accumulate the "defthing" specs.
So when a subclass is created, we include into its definition all
its details from the superclass.

To store that accumulation, use a hashtable that stores "about"
structs.

|#
(defvar *meta* (make-hash-table))

(defstruct about has does )

#|
Also, yu'll need to write a new definition of the "defthing" macro
which we call "defklass". As a side-effect of creating the lambda,
it also sends that object a pointer to itself (see "_self!") as
well as telling the object what kind of thing it is (see "_ako!").
This new "defklass" macro returns:

     `(defun ,klass (&key ,@has) 
            (let ((,self (lambda (,message)
                           (case ,message
                             ,@(methods-as-case does)
                             ,@(datas-as-case (mapcar #'car has))))))
              (send ,self '_self! ,self)
              (send ,self '_isa! ',klass)
              ,self))))

The key thing about defklass is that, before it returns anything,
it combines the subckass "has" and "uses" unformation with that of
the parent class.

To make that work, defklass has a ":isa" creation key.  Using that
"isa" thing, we look up the "has" and "uses" slots of the parent
and then concat them into the new definitions.  Then we store the
concatenated parts so we can access them later.

To store these concatenated definitions, use

  (setf (gethash klass *meta*)
        (make-about :has has :does does))

To access these definitions, use

  (let* ((b4          (and isa (gethash isa *meta*)))
         (has-before  (and b4 (about-has b4)))
         (does-before (and b4 (about-does b4))))
      etc

Here's the new defklass macros using it, we define the following
hierarchy:

object
  account
    trimmed-account

|#

; implement defklass here

(let ((_counter 0))
  (defun counter () (incf _counter)))

(defun meta? (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\_)))

; uncomment the following when defklass is implemented

'(defklass 
  object 
  :has ((_self)  (_isa) (id (counter)))
  :does (
         (_isa!  (x) (setf _isa  x))
         (_self! (x) (setf _self x))
         (show () (let ((slot-values)
                        (slots (mapcar #'car 
                                 (about-has (gethash _isa *meta*)))))
                      (dolist (one slots slot-values)
                        (if (not (meta? one))
                          (push `(,one . ,(send _self one)) 
                                slot-values)))))))

; uncomment the following when defklass is implemented
'(defklass
  account
  :isa object
  :has  ((name) (balance 0) (interest-rate .05))
  :does ((withdraw (amt)
                     (decf balance amt))
         (deposit (amt)
                  (incf balance amt))
         (interest ()
                   (incf balance
                         (* interest-rate balance)))))

; uncomment this to see what is going on
'(xpand (account))

; uncomment the following when defklass is implemented
'(defklass
  trimmed-account
  :isa account
  :does ((withdraw (amt)
                   (if (<= amt balance)
                     (decf balance amt)
                     'insufficient-funds))))
(defun inheritance ()
  (let ((acc (trimmed-account)))
    (print `(inheritance ,(send acc 'deposit 100)
                         ,(send acc 'interest)
                         ,(send acc 'interest)
                         ,(send acc 'balance)
                         ,(send acc 'id)))
      (dotimes (i 10)
        (print `(inheritance ,(send acc 'withdraw 20))))
      ))

; TODO: 3a show that the following works correctly

'(inheritance)

'(xpand (trimmed-account))
; TODO: 3b. show that the following prints out the slots of an object.

(defun meta ()
   (let ((acc (trimmed-account)))
      (print `(meta ,(send acc 'show))
   )))

'(meta)
