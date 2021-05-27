Functions, Evaluation, Flow Control, Definitions and Syntax

apply
Syntax:
Symbol type: function
applyfnargs(zero or more) => value
Argument description:

 fn  a function designator  
 args  call arguments  

APPLY function call supplied function with specified arguments. Argument list is constructed
as (append (butlast args) (first (last args))). Note that there is limitation of maximal number
of arguments, see CALL-ARGUMENTS-LIMIT constant. See also FUNCALL, LAMBDA. 


(apply #'+ 1 2 3 '(4 5 6)) => 21
(apply #'sin '(1.0)) => 0.84147096
(apply #'sin 1.0 nil) => 0.84147096
;;-------------------------------------------------------------------
case
Syntax:
Symbol type: macro
caseexpressionvariants(zero or more) => an object
Argument description:

 expression  a value used to distinguish between  
   variants  
 variants  list of match-values and code variants  

CASE macro is used for branching. Variants are tested sequentially for EQL equality with
from the top. See also IF, CASE. 


(case (+ 1 2)
      (5 "variant 1, five")
      ((2 3) "variant 2, two or three")
      (otherwise "variant 3, none of above")) => "variant 2, two or three"
;;-------------------------------------------------------------------
cond
Syntax:
Symbol type: macro
condvariants => an object
Argument description:

 variants  list of test and code variants  

COND macro is used for branching. Variants are tested sequentially from the top. See also
IF, CASE. 


(cond ((> 3 4) "variant 1")
      ((> 4 2) "variant 2")
      (t "always valid variant")) => "variant 2"
;;------------------------------------------------------------------
defparameter
Syntax:
Symbol type: macro
defparameternameinitial-valuedocumentation(optional) => an object
Argument description:

 name  a name for global variable  
 initial-value  an expression  
 documentation  documentation string  

DEFPARAMETER defines global variable with dynamic scoping. Usual conventions dictate
to make such variables easy to distinguish so their name is surrounded by stars. Value for
variable is reevaluated for each occurence (unlike with DEFVAR). See also DEFVAR, LET,
SETQ. 


(defparameter *my-global-variable* (+ 3 5)) => *MY-GLOBAL-VARIABLE*
*my-global-variable* => 8
;;-----------------------------------------------------------------
defun
Syntax:
Symbol type: macro
defunnameargsforms(zero or more) => symbol
Argument description:

 name  symbol  
 args  arguments of function  
 forms  sequentially executed forms  

DEFUN form creates named function. The function is associated with definition
environment. Named functions can be called simply by specifying their name in function
position in parenthesis, or they can be acquired by FUNCTION special form, or
SYMBOL-FUNCTION function. Arguments of function can be regular (matched by position),
optional (with default values), keyword (matched by keyword symbol) and rest (taking rest of
arguments into a list). Result of function application is value of the last form unless return
function or nonlocal exit is executed. Functions can be redefined. See also LAMBDA,
FUNCALL, APPLY. 


(defun myname (x) (+ x 3)) => MYNAME

(defun myname (x y) (* x y) (+ x y)) (myname 2 3) => 5

(defun myname (&optional (one 1) (two 2)) (list one two)) (myname) => (1 2)
(defun myname (&optional (one 1) (two 2)) (list one two)) (myname 10) => (10 2)
(defun myname (&optional (one 1) (two 2)) (list one two)) (myname 10 20) => (10 20)

(defun myname (&rest myargs) (length myargs)) (myname) => 0
(defun myname (&rest myargs) (length myargs)) (myname 4 5 6) => 3
(defun myname (&rest myargs) (length myargs)) (myname '(4 5 6)) => 1

(defun myname (&key one two) (list one two)) (myname) => (NIL NIL)
(defun myname (&key one two) (list one two)) (myname :two 7) => (NIL 7)
(defun myname (&key one two) (list one two)) (myname :two 7 :one 4) => (4 7)
;;-----------------------------------------------------------------
eval
Syntax:
Symbol type: function
evalform => value
Argument description:

 form  a value forming lisp expression  

EVAL function interprets (or compiles and runs) the argument and returns the result. See
also APPLY, LAMBDA, FUNCALL. 


(eval '(+ 1 2)) => 3
(eval '(let ((x 2)) (sin x))) => 0.9092974
(let ((expr '(((x 2)) (sin x)))) (eval (cons 'let expr))) => 0.9092974
;;-----------------------------------------------------------------
flet
Syntax:
Symbol type: special form
fletbindingsbody(zero or more) => an object
Argument description:

 bindings  list containing function definitions   
 body  program code in which definitions above  
   are effective, implicit progn  

FLET is special form for local function binding. Bindings are not recursive and cannot refer
to each other. Each binding contains function name, arguments, and function body. See
LABELS, DEFUN, LAMBDA. 


(flet ((sin2x (x) (sin (* 2 x)))
       (cos2x (x) (cos (* 2 x))))
 (+ (sin2x 0.2) (cos2x 0.2)))
=> 1.3104793
;;-----------------------------------------------------------------
funcall
Syntax:
Symbol type: function
funcallfnargs(zero or more) => value
Argument description:

 fn  a function designator  
 args  call arguments  

FUNCALL function call supplied function with specified arguments. Argument list is same as
in the rest of funcall call. Function designator is function itself or symbol specifying global
function name. Note that there is limitation of maximal number of arguments, see
CALL-ARGUMENTS-LIMIT constant. See also APPLY, LAMBDA. 


(funcall #'+ 1 2 3 4 5 6) => 21
(funcall #'sin 1.0) => 0.84147096
(funcall 'sin 1.0) => 0.84147096
;;------------------------------------------------------------------
function
Syntax:
Symbol type: special form
functionsymbol => function
Argument description:

 symbol  symbol of function name  

FUNCTION is special form for accessing namespace of functions. See also QUOTE. 


(function sin) => #<FUNCTION>
#'sin => #<FUNCTION>
(funcall #'sin 1.0) => 0.84147096
;;----------------------------------------------------------------
if
Syntax:
Symbol type: special syntax
iftestthenelse(optional) => an object
Argument description:

 test  an expression  
 then  an expression  
 else  an expression, default NIL  

IF special form is used for branching. Either "then" or "else" branch is taken. Then branch is
selected when "test" result is not NIL. See also COND, CASE. 


(if (> 3 4) "variant 1" "variant 2") => "variant 2"
(if (> 4 3) "variant 1" "variant 2") => "variant 1"
;;------------------------------------------------------------------
labels
Syntax:
Symbol type: special form
labelsbindingsbody(zero or more) => an object
Argument description:

 bindings  list containing function definitions   
 body  program code in which definitions above  
   are effective, implicit progn  

LABELS is special form for local function binding. Bindings can be recursive and can refer
to each other. Each binding contains function name, arguments, and function body. See
FLET, DEFUN, LAMBDA. 


(labels ((fact2x (x) (fact (* 2 x)))
         (fact (x) (if (< x 2) 1 (* x (fact (1- x))))))
  (fact2x 3))
 => 720
;;------------------------------------------------------------------
lambda
Syntax:
Symbol type: special
lambdaargsforms(zero or more) => function
Argument description:

 args  arguments of function  
 forms  sequentially executed forms  

LAMBDA form creates function object associated with definition environment. This function
object is called "closure". It can be applied later with funcall. Arguments of function can be
regular (matched by position), optional (with default values), keyword (matched by keyword
symbol) and rest (taking rest of arguments into a list). Lambda form don't have to be prefixed
with "#'" syntax. Result of function application is value of the last form unless return function
or nonlocal exit is executed. 


(lambda (x) (+ x 3)) => <#closure>

(funcall (lambda (x y) (* x y) (+ x y)) 2 3) => 5

(funcall (lambda (&optional (one 1) (two 2)) (list one two))) => (1 2)
(funcall (lambda (&optional (one 1) (two 2)) (list one two)) 10) => (10 2)
(funcall (lambda (&optional (one 1) (two 2)) (list one two)) 10 20) => (10 20)

(funcall (lambda (&rest myargs) (length myargs))) => 0
(funcall (lambda (&rest myargs) (length myargs)) 4 5 6) => 3
(funcall (lambda (&rest myargs) (length myargs)) '(4 5 6)) => 1

(funcall (lambda (&key one two) (list one two))) => (NIL NIL)
(funcall (lambda (&key one two) (list one two)) :two 7) => (NIL 7)
(funcall (lambda (&key one two) (list one two)) :two 7 :one 4) => (4 7)
;;-----------------------------------------------------------------
let
Syntax:
Symbol type: special form
letbindingsbody(zero or more) => an object
Argument description:

 bindings  list of variable - initial value pairs  
 body  program code in which definitions above  
   are effective, implicit progn  

LET is special form for variable binding. Bindings are described in two element lists where
the first element specifies name and the second is code to compute its value, or single
variable without default initialization. There are also declarations possible before body. 


(let (a b (c 3) (d (+ 1 2))) (list a b c d)) => (NIL NIL 3 3)
;;-----------------------------------------------------------------
progn
Syntax:
Symbol type: special form
prognlist => value
Argument description:

 list  expressions  

PROGN calls its expression in the order they have been written. Resulting value is the value
of the last form unless non-local control flow forced earlier return. See also PROG1, PROG2.

Note that many macros and special forms behave partially as PROGN. It is called "implicit
progn". 


(progn 1 2 3 4 5) => 5
(progn 1 2 (sin 2.0) 4 (sin 1.0)) => 0.84147096
(progn) => NIL
;;-----------------------------------------------------------------
quote
Syntax:
Symbol type: special form
quotedata => value
Argument description:

 data  data  

QUOTE is special form for data quotation. The apostrophe character is reader macro
synonym for QUOTE. See also FUNCTION. 


(+ 1 2 3) => 6
(quote (+ 1 2 3)) => (+ 1 2 3)
'(+ 1 2 3) => (+ 1 2 3)
(let ((some-symbol 4)) some-symbol) => 4
(let ((some-symbol 4)) (quote some-symbol)) => SOME-SYMBOL
(let ((some-symbol 4)) 'some-symbol) => SOME-SYMBOL
;;-----------------------------------------------------------------
setf
Syntax:
Symbol type: macro
setfpairs(zero or more) => an object
Argument description:

 pairs  pairs of places and values  

SETF is similar to SETQ but works with generalized places. Many functions for read access
can be turned into write access. See LET, SETQ. SETF expanders can be defined in
multiple ways, most easier is (defun (setf my-name) arguments body...). 


(let (a b) (setf a 4) (setf b 3) (setf a (+ a b))) => 7
(let ((a #(1 2 3 4))) (setf (aref a 2) 'new-value) a) => #(1 2 NEW-VALUE 4)
(let ((a '(1 2 3 4))) (setf (third a) 'new-value) a) => (1 2 NEW-VALUE 4)
;;-----------------------------------------------------------------
setq
Syntax:
Symbol type: special form
setqpairs(zero or more) => an object
Argument description:

 pairs  pairs of variables and values  

SETQ special form sets a variable. If a variable name is not know, implementation may
create new one global and dynamic one. See also LET, SETF. 


(let (a b) (setq a 4) (setq b 3) (setq a (+ a b))) => 7
;;-----------------------------------------------------------------
