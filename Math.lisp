Mathematics, Arithmetics, Logic and Comparisons
*
Syntax:
Symbol type: function
*numbers(zero or more) => number
Argument description:

 numbers  numeric values  

* function computes product of numbers. It does type conversions for numbers. There is no
wraparound in integer numbers - they are arbitrary long. It works for all number types including
integer, rational, floating point and complex. 


(* 1 2 3) => 6
(* 1 2) => 2
(* 1) => 1
(*) => 1

(* 1234567890123456789 9876543210987654321) => 12193263113702179522374638011112635269
(* 1.3 -5) => -6.5
(* 1.3d0 -5) => -6.5d0
(* #c(2 4) 3) => #C(6 12)
(* 3/4 7/9) => 7/12
;;-------------------------------------------------------------------
+
Syntax:
Symbol type: function
+numbers(zero or more) => number
Argument description:

 numbers  numeric values  

+ function computes sum of numbers. It does type conversions for numbers. There is no
wraparound in integer numbers - they are arbitrary long. It works for all number types including
integer, rational, floating point and complex. 


(+ 1 2 3) => 6
(+ 1 2) => 3
(+ 1) => 1
(+) => 0

(+ 1234567890123456789 9876543210987654321) => 11111111101111111110
(+ 1.3 -5) => -3.7
(+ 1.3d0 -5) => -3.7d0
(+ #c(2 4) 1) => #C(3 4)
(+ 3/4 7/9) => 55/36
;;------------------------------------------------------------------
-
Syntax:
Symbol type: function
-numbers(one or more) => number
Argument description:

 numbers  numeric values  

- function computes difference between first value and sum of the rest. When called with only
one argument, it does negation. It does type conversions for numbers. There is no
wraparound in integer numbers - they are arbitrary long. It works for all number types including
integer, rational, floating point and complex. 


(- 1 2 3) => -4
(- 1 2) => -1
(- 1) => -1

(- 1234567890123456789 9876543210987654321) => -8641975320864197532
(- 1.3 -5) => 6.3
(- 1.3d0 -5) => 6.3d0
(- #c(2 4) 1) => #C(1 4)
(- 3/4 7/9) => -1/36
;;-----------------------------------------------------------------
/
Syntax:
Symbol type: function
/numbers(one or more) => number
Argument description:

 numbers  numeric values  

/ function computes division or reciprocal. When called with one argument it computes
reciprocal. When called with two or more arguments it does compute division of the first by the
all remaining number. It does type conversions for numbers. It works for all number types
including integer, rational, floating point and complex. Note that division by zero invokes
DIVISION-BY-ZERO condition. 


(/ 10) => 1/10
(/ 10.0) => 0.1

(/ 10 2) => 5
(/ 2 10) => 1/5
(/ 100 2 5 2) => 5
(/ 100 (* 2 5 2)) => 5

(/ 1234567890123456789 9876543210987654321) => 13717421/109739369
(/ 1.3 -5) => -0.26
(/ 1.3d0 -5) => -0.26d0
(/ #c(2 4) 3) => #C(2/3 4/3)
(/ 3/4 7/9) => 27/28
;;-----------------------------------------------------------------
/=
Syntax:
Symbol type: function
/=numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

/= function compares numbers according to "equal" predicate. Result is true if no two
numbers are equal to each other, otherwise result is false. Note that only two argument version
result is negation of = function, that is (/= a b) is same as (not (= a b)). 

(/= 1 2) => T
(/= 2 1) => T
(/= 2 2.001) => T
(/= 2 2) => NIL
(/= 2 2.0) => NIL
(/= 0.0 -0.0) => NIL
(/= #c(1.2 4.5) #c(1.2 4.5)) => NIL

(/= 1 2 3 4 5) => T
(/= 4 4 4 3 4) => NIL
(/= 1 2 3 4 4) => NIL
(/= 1 2 3 4.0 4) => NIL
(/= 5) => T
;;-----------------------------------------------------------------
1+
Syntax:
Symbol type: function
1+number => number
Argument description:

 number  numeric value  

1+ function adds one to the argument. See +. 


(1+ 5) => 6
(1+ 1234567890123456789) => 1234567890123456790
(1+ 1.3) => 2.3
(1+ 1.3d0) => 2.3d0
(1+ #c(2 4)) => #C(3 4)
(1+ 3/4) => 7/4
;;----------------------------------------------------------------
1-
Syntax:
Symbol type: function
1-number => number
Argument description:

 number  numeric value  

1- function subtracts one from the argument. See -. 


(1- 5) => 4
(1- 1234567890123456789) => 1234567890123456788
(1- 1.3) => 0.29999995
(1- 1.3d0) => 0.30000000000000004d0
(1- #c(2 4)) => #C(1 4)
(1- 3/4) => -1/4
;;-----------------------------------------------------------------
<
Syntax:
Symbol type: function
<numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

< function compares numbers according to "less than" predicate. Each (overlapping) pair of
the numbers is compared by it. The result is true if all compared pairs satisfy comparison.
Note that complex numbers cannot be compared. 

(< 1 2) => T
(< 2 1) => NIL
(< 2 2.001) => T
(< 2 2) => NIL

(< 1234567890123456789 9876543210987654321) => T
(< 1 2 3 4 5) => T
(< 1 2 4 3 5) => NIL
(< 1 2 4 4 5) => NIL
(< 3/4 7/9) => T
(< 5) => T
;;----------------------------------------------------------------
<=
Syntax:
Symbol type: function
<=numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

<= function compares numbers according to "less than or equal" predicate. Each
(overlapping) pair of the numbers is compared by it. The result is true if all compared pairs
satisfy comparison. Note that complex numbers cannot be compared. 


(<= 1 2) => T
(<= 2 1) => NIL
(<= 2 2.001) => T
(<= 2 2) => T

(<= 1234567890123456789 9876543210987654321) => T
(<= 1 2 3 4 5) => T
(<= 1 2 4 3 5) => NIL
(<= 1 2 4 4 5) => T
(<= 3/4 7/9) => T
(<= 5) => T
;;-----------------------------------------------------------------
=
Syntax:
Symbol type: function
=numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

= function compares numbers according to "equal" predicate. Result is true if every specified
number is equal to each other, otherwise result is false. 


(= 1 2) => NIL
(= 2 1) => NIL
(= 2 2.001) => NIL
(= 2 2) => T
(= 2 2.0) => T
(= 0.0 -0.0) => T
(= #c(1.2 4.5) #c(1.2 4.5)) => T

(= 1 2 3 4 5) => NIL
(= 4 4 4 3 4) => NIL
(= 4 4 4 4 4) => T
(= 4 4 4 4.0 4) => T
(= 5) => T
;;-----------------------------------------------------------------
>
Syntax:
Symbol type: function
>numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

> function compares numbers according to "greater than" predicate. Each (overlapping) pair
of the numbers is compared by it. The result is true if all compared pairs satisfy comparison.
Note that complex numbers cannot be compared. 


(> 2 1) => T
(> 1 2) => NIL
(> 2.001 2) => T
(> 2 2) => NIL

(> 9876543210987654321 1234567890123456789) => T
(> 5 4 3 2 1) => T
(> 5 3 4 2 1) => NIL
(> 5 4 4 2 1) => NIL
(> 7/9 3/4) => T
(> 5) => T
;;-----------------------------------------------------------------
>=
Syntax:
Symbol type: function
>=numbers(one or more) => T or NIL
Argument description:

 numbers  numeric values  

>= function compares numbers according to "greater than or equal" predicate. Each
(overlapping) pair of the numbers is compared by it. The result is true if all compared pairs
satisfy comparison. Note that complex numbers cannot be compared. 


(>= 2 1) => T
(>= 1 2) => NIL
(>= 2.001 2) => T
(>= 2 2) => T

(>= 9876543210987654321 1234567890123456789) => T
(>= 5 4 3 2 1) => T
(>= 5 3 4 2 1) => NIL
(>= 5 4 4 2 1) => T
(>= 7/9 3/4) => T
(>= 5) => T
;;-----------------------------------------------------------------
and
Syntax:
Symbol type: macro
andforms(zero or more) => value
Argument description:

 forms  forms which value is considered  

AND macro computes logical "and" function. Forms evaluation starts from left. Value from the
first form that decides result is returned so forms at end of argument list may not evaluated. 


(and t t t nil t) => NIL
(and t t t t) => T
(and) => T

(and (progn (write "SEEN") nil) (progn (write "UNSEEN") t)) => "SEEN" NIL
(and 4 5 6) => 6
;;-----------------------------------------------------------------
ceiling
Syntax:
Symbol type: function
ceilingnumberdivisor => quotient (numeric value), remainder (numeric value)
Argument description:

 number  number  
 divisor  non-zero real number, default is 1  

CEILING function returns two values, the first is result of dividing number by divisor and
truncating toward positive infinity. Second result remainder that satisfies equation: quotient *
divisor + remainder = number. 


(ceiling 10) => 10, 0
(ceiling 10.3 2) => 6, -1.6999998
(ceiling 3/4) => 1, -1/4

(multiple-value-list (ceiling 20 7)) => (3 -1)
;;-----------------------------------------------------------------
cos
Syntax:
Symbol type: function
cosnumber => numeric value
Argument description:

 number  numeric value, angle in radians  

COS function computes cosine of value in radians. 


(cos 0.0) => 1.0
(cos 1.0) => 0.5403023
(cos 1.0d0) => 0.5403023058681398d0
(cos #c(1.0 1.0)) => #C(0.83373 -0.9888977)
;;-----------------------------------------------------------------
decf
Syntax:
Symbol type: macro
decfplacedecrement(optional) => numeric value
Argument description:

 place  place with numeric value  
 decrement  numeric value  

DECF macro modifies a place with numeric value. Its value is decremented by decrement
number. Default decrement is 1. 


(let ((a 10)) (decf a) a) => 9
(let ((a 10)) (decf a 2.3) a) => 7.7
(let ((a 10)) (decf a -2.3) a) => 12.3
(let ((a (list 10 11 12 13))) (decf (elt a 2) 2.3) a) => (10 11 9.7 13)
;;-----------------------------------------------------------------
eq
Syntax:
Symbol type: function
eqobject1object2 => T or NIL
Argument description:

 object1  first object  
 object2  second object  

EQ function compares object identity. It works for symbols and identical objects. It is not
suitable for comparing numbers - see EQL and =. Result is true if they are same, otherwise
false. 


(eq 'moo 'moo) => T
(eq 'moo 'foo) => NIL
(eq 1 1) => T
(eq 1 2) => NIL
(eq 1234567890123456789 1234567890123456789) => NIL

(eq (cons 1 2) (cons 1 2)) => NIL
(let ((x (cons 1 2))) (eq x x)) => T
;;-----------------------------------------------------------------
eql
Syntax:
Symbol type: function
eqlobject1object2 => T or NIL
Argument description:

 object1  first object  
 object2  second object  

EQL function compares object identity, numbers and characters. Numbers are considered as
equal only when they have the both same value and type. Result is true if they are same,
otherwise false. 


(eql 'moo 'moo) => T
(eql 'moo 'foo) => NIL
(eql 1 1) => T
(eql 1 2) => NIL
(eql 1234567890123456789 1234567890123456789) => T

(eql 1.0 1) => NIL
(eql 1.0 1.0) => T

(eql (cons 1 2) (cons 1 2)) => NIL
(let ((x (cons 1 2))) (eql x x)) => T
;;-----------------------------------------------------------------
equal
Syntax:
Symbol type: function
equalobject1object2 => T or NIL
Argument description:

 object1  first object  
 object2  second object  

EQUAL function compares same things as eql, additionally result is true under some other
situations: conses are compared recursively (in both car and cdr part), string and bit-vectors
are compared element-wise. Result is true if they are same, otherwise false. 


(equal "moo" "moo") => T
(equal "moo" "MoO") => NIL
(equal #*1010101 #*1010101) = T
(equal (vector 2 3 4) (vector 2 3 4)) = NIL
(equal (cons 1 2) (cons 1 2)) => T
(let ((x (cons 1 2))) (equal x x)) => T

(equal 'moo 'moo) => T
(equal 'moo 'foo) => NIL
(equal 1 1) => T
(equal 1 2) => NIL
(equal 1234567890123456789 1234567890123456789) => T

(equal 1.0 1) => NIL
(equal 1.0 1.0) => T
;;-----------------------------------------------------------------
equalp
Syntax:
Symbol type: function
equalpobject1object2 => T or NIL
Argument description:

 object1  first object  
 object2  second object  

EQUALP function compares same things as equal, additionally result is true under some
other situations: conses are compared recursively (in both car and cdr part), any sequence is
compared recursively (element-wise), strings and characters are compared case
insensitively. Result is true if they are same, otherwise false. 


(equalp "moo" "moo") => T
(equalp "moo" "MoO") => T
(equalp "moo" "moo ") => NIL
(equalp (vector 2 3 4) (vector 2 3 4)) = T
(equalp (cons 1 2) (cons 1 2)) => T
(let ((x (cons 1 2))) (equalp x x)) => T

(equalp 'moo 'moo) => T
(equalp 'moo 'foo) => NIL
(equalp "a" 'a) => NIL
(equalp 1 1) => T
(equalp 1 2) => NIL
(equalp 1234567890123456789 1234567890123456789) => T

(equalp 1.0 1) => T
(equalp 1.0 1.0) => T
;;-----------------------------------------------------------------
exp
Syntax:
Symbol type: function
expnumber => number
Argument description:

 number  number to be raised  

EXP function returns e raised to the power number, where e is the base of the natural
logarithms. 


(exp 1) => 2.7182817
(exp 1.0) => 2.7182817
(exp 1.0d0) => 2.718281828459045d0
(exp 10) => 22026.465

(log (exp 10)) => 10.0
;;-----------------------------------------------------------------
expt
Syntax:
Symbol type: function
exptbase-numberpower-number => number
Argument description:

 base-number  number to be raised  
 power-number  number that specifies power  

EXPT function returns base-number raised to the power-number. 


(expt 2 8) => 256
(expt 2 32) => 4294967296
(expt 2 64) => 18446744073709551616
(expt 10 3) => 1000
(expt 5 1/3) => 1.709976
(expt 1.709976 3) => 5.0

(expt 1 2) => 1
(expt 1.0 2) => 1.0
(expt 1.0d0 2) => 1.0d0
(expt -2 5) => -32
(expt 2 2.5) => 5.656854
(expt -2 2.5) => #C(1.7318549e-15 5.656854)
(expt 2 -3) => 1/8
(expt -2 -3) => -1/8
(expt -2.4 -3) => -0.072337955
(expt -2.4 -3.3) => #C(-0.032698035 0.045004968)

(expt (expt 10 5) 1/5) => 10.0
;;-----------------------------------------------------------------
floor
Syntax:
Symbol type: function
floornumberdivisor => quotient (numeric value), remainder (numeric value)
Argument description:

 number  number  
 divisor  non-zero real number, default is 1  

FLOOR function returns two values, the first is result of dividing number by divisor and
truncating toward negative infinity. Second result remainder that satisfies equation: quotient *
divisor + remainder = number. 


(floor 10) => 10, 0
(floor 10.3 2) => 5, 0.3000002
(floor 3/4) => 0, 3/4

(multiple-value-list (floor 20 7)) => (2 6)
;;-----------------------------------------------------------------
incf
Syntax:
Symbol type: macro
incfplaceincrement(optional) => numeric value
Argument description:

 place  place with numeric value  
 increment  numeric value  

INCF macro modifies a place with numeric value. Its value is incremented by increment
number. Default increment is 1. 


(let ((a 10)) (incf a) a) => 11
(let ((a 10)) (incf a 2.3) a) => 12.3
(let ((a 10)) (incf a -2.3) a) => 7.7
(let ((a (list 10 11 12 13))) (incf (elt a 2) 2.3) a) => (10 11 14.3 13)
;;-----------------------------------------------------------------
isqrt
Syntax:
Symbol type: function
isqrtnumber => integer value
Argument description:

 number  positive integer  

ISQRT function computes integer part of square root of number. See also SQRT. 


(isqrt 10) => 3
(isqrt 4) => 2
;;-----------------------------------------------------------------
logand
Syntax:
Symbol type: function
logandintegers(zero or more) => integer
Argument description:

 integers  integers for bitwise operations  

LOGIOR function computes bitwise logical "and" function. 


(logand) => -1
(logand 1 2) => 0
(logand #xff #xaa) => 170
(logand #b1010 #b100 #b11) => 0
;;-----------------------------------------------------------------
logior
Syntax:
Symbol type: function
logiorintegers(zero or more) => integer
Argument description:

 integers  integers for bitwise operations  

LOGIOR function computes bitwise logical "or" function. 


(logior) => 0
(logior 1 2) => 3
(logior #xf0 #xf) => 255
(logior #b1010 #b100 #b11) => 15
;;-----------------------------------------------------------------
max
Syntax:
Symbol type: function
maxnumbers(one or more) => numeric value
Argument description:

 numbers  comparable numbers  

MAX function returns the maximal number from arguments. Type of resulting number may be
different when arguments multiple precision numbers. 


(max 1 3 2) => 3
(max 4) => 4
(= 3.0 (max 3 1 3.0 2 3.0d0)) => T
;;-----------------------------------------------------------------
min
Syntax:
Symbol type: function
minnumbers(one or more) => numeric value
Argument description:

 numbers  comparable numbers  

MIN function returns the minimal number from arguments. Type of resulting number may be
different when arguments multiple precision numbers. 


(min 1 3 2) => 1
(min 4) => 4
(= 3.0 (min 3 7 3.0 8 3.0d0)) => T
;;-----------------------------------------------------------------
mod
Syntax:
Symbol type: function
modnumberdivisor => number
Argument description:

 number  real number  
 divisor  real number  

MOD function returns modulus of two integer arguments. Non-integer arguments are first
turned into integers by floor operation. Note that division by zero invokes DIVISION-BY-ZERO
condition. 


(mod -1 5) => 4                                                              
(mod 13 4) => 1                                                              
(mod -13 4) => 3                                                             
(mod 13 -4) => -3                                                            
(mod -13 -4) => -1                                                           
(mod 13.4 1) => 0.4                                                          
(mod -13.4 1) => 0.6                                                         
;;-----------------------------------------------------------------
nil
Syntax:
Symbol type: symbol
nil => symbol 

NIL symbol denotes empty list and false value. There is also ubiquitous NIL constant which
contains NIL symbol. NIL is considered as false value by comparison functions and control
operators (unlike any other). Empty list, that is '() or even (), is the same as NIL value. 


nil => NIL
'nil => NIL
() => NIL
'() => NIL

(eq nil 'nil) => T
(eq 'nil ()) => T
(eq () '()) => T

(not t) => NIL
(not nil) => T
(not 234.3) => NIL
;;-----------------------------------------------------------------
not
Syntax:
Symbol type: function
notvalue => value
Argument description:

 value  logical value  

NOT computes logical negation. Note that any other value than NIL is considered as true. NOT
is identical to NULL, but used in conjunction with boolean logic processing unlike NOT which
is used in list processing. 


(not t) => NIL
(not nil) => T
(not 234.3) => NIL
;;-----------------------------------------------------------------
or
Syntax:
Symbol type: macro
orforms(zero or more) => value
Argument description:

 forms  forms which value is considered  

OR macro computes logical "or" function. Forms evaluation starts from left. Value from the
first form that decides result is returned so forms at end of argument list may not evaluated. 


(or t t t nil t) => T
(or nil nil nil) => NIL
(or) => NIL

(or (progn (write "SEEN") 123) (progn (write "UNSEEN") t)) => "SEEN" 123
(or 4 5 6) => 4
;;-----------------------------------------------------------------
random
Syntax:
Symbol type: function
randomlimitrandom-state(optional) => numeric value
Argument description:

 limit  positive number, integer or real  
 random-state  object representing random state  

RANDOM function generates random numbers. For integer argument N, result is integer
between zero (including) and N (excluding). For real argument X, result is real between zero
(including) and X (excluding). All generated numbers have approximately same probability.
Default value for random-state is stored in *random-state* global variable. 


(<= 0 (random 20) 19) => T
(let ((x (random 1.0))) (or (= x 0) (< 0 x 1.0))) => T
;;-----------------------------------------------------------------
round
Syntax:
Symbol type: function
roundnumberdivisor => quotient (numeric value), remainder (numeric value)
Argument description:

 number  number  
 divisor  non-zero real number, default is 1  

FLOOR function returns two values, the first is result of dividing number by divisor and
truncating toward nearest even integer. Second result remainder that satisfies equation:
quotient * divisor + remainder = number. 


(round 10) => 10, 0
(round 10.3 2) => 5, 0.3000002
(round 3/4) => 0, 3/4
(round 3/2) => 2, -1/2

(multiple-value-list (round 20 7)) => (3 -1)
;;-----------------------------------------------------------------
sin
Syntax:
Symbol type: function
sinnumber => numeric value
Argument description:

 number  numeric value, angle in radians  

SIN function computes sine of value in radians. 


(sin 0.0) => 0.0
(sin 1.0) => 0.84147096
(sin 1.0d0) => 0.8414709848078965d0
(sin #c(1.0 1.0)) => #C(1.2984576 0.63496387)
;;-----------------------------------------------------------------
sqrt
Syntax:
Symbol type: function
sqrtnumber => numeric value
Argument description:

 number  number  

SQRT function computes square root of number. Number may be integer, real or complex.
See also ISQRT. 


(sqrt 10) => 3.1622777
(sqrt 10.0) => 3.1622777
(sqrt 10.0d0) => 3.1622776601683795d0
(sqrt 4) => 2.0

(sqrt -4) => #C(0.0 2.0)
(sqrt #C(0.0 2.0)) => #C(1.0 1.0)
;;-----------------------------------------------------------------
t
Syntax:
Symbol type: symbol
t => symbol 

T symbol denotes true value. There is also ubiquitous T constant which contains T symbol. T
is not only true value, all values except NIL are treat as true by comparison functions and
control operators. Note that constants cannot be redefined (even locally) so there is no chance
to make variable t in same name space with T (the true symbol). 


't => T
t => T
(eq 't t) => T

(not t) => NIL
(not nil) => T
(not 234.3) => NIL
;;------------------------------------------------------------------
zerop
Syntax:
Symbol type: function
zeropnumber => boolean
Argument description:

 number  a number  

ZEROP function returns true if the argument is zero. 


(zerop 0) => T
(zerop -0.0) => T
(zerop #c(0 0.0)) => T
(zerop #c(0 0.1)) => NIL
(zerop 3/3) => NIL
;;-----------------------------------------------------------------
