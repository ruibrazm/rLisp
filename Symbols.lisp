Symbol, Characters, Hash, Structure, Objects and Conversions

atom
Syntax:
Symbol type: function
atomobject => T or NIL
Argument description:

 object  an object  

ATOM function returns true if the argument is not a cons cell, otherwise it returns false. See
CONS and LIST. 


(atom nil) => T
(atom 'some-symbol) => T
(atom 3) => T
(atom "moo") => T
(atom (cons 1 2)) => NIL
(atom '(1 . 2)) => NIL
(atom '(1 2 3 4)) => NIL
(atom (list 1 2 3 4)) => NIL
;;-------------------------------------------------------------------
coerce
Syntax:
Symbol type: function
coerceobjectresult-type => an object
Argument description:

 object  an object  
 result-type  a type specifier  

COERCE function converts between different types. See full documentation for conversion
description. 


(coerce '(a b c) 'vector) => #(A B C)
(coerce #(a b c) 'list) => (A B C)
(coerce 4.4d0 'single-float) => 4.4
(coerce 4.4s0 'double-float) => 4.400000095367432d0
(coerce "x" 'character) => #\x
;;-------------------------------------------------------------------
gethash
Syntax:
Symbol type: function
gethashkeyhashtabledefault(optional) => an object
Argument description:

 key  an object  
 hashtable  a hash-table  
 default  an object, default is NIL  

GETHASH function reads associated value for given key in hashtable. (SETF GETHASH)
adds or replaces associated values. See also MAKE-HASH-TABLE. 


(defparameter *tab* (make-hash-table)) => *TAB*
(gethash 'x *tab*) => NIL, NIL
(setf (gethash 'x *tab*) "x") => "x"
(setf (gethash 'y *tab*) "yy") => "yy"
(gethash 'x *tab*) => "x", T
(gethash 'y *tab*) => "yy", T
(gethash 'z *tab* 'moo) => MOO, NIL
;;-------------------------------------------------------------------
intern
Syntax:
Symbol type: function
internstirngpackage(optional) => symbol, status
Argument description:

 stirng  a string  
 package  a package designator, default is current  
   package  

INTERN function makes a new symbol from string. Possible status values are: :inherited,
:external, :internal, or nil. 


(intern "MOO") => MOO, NIL
(intern "MOO") => MOO, :INTERNAL
(intern "moo") => |moo|, NIL
;;------------------------------------------------------------------
make-hash-table
Syntax:
Symbol type: function
make-hash-tabletest(keyword)size(keyword)rehash-size(keyword)rehash-threshold
(keyword) => hash-table
Argument description:

 test  EQ, EQL EQUAL or EQUALP;  
   default is EQL  
 size  a non-negative integer  
 rehash-size  a real number  
 rehash-threshold  a real number  

MAKE-HASH-TABLE creates a new hash-table. Size parameter specifies initial size of
inner table. Test specifies comparison operator for keys. See also GETHASH. 


(defparameter *tab* (make-hash-table)) => *TAB*
(gethash 'x *tab*) => NIL, NIL
(setf (gethash 'x *tab*) "x") => "x"
(setf (gethash 'y *tab*) "yy") => "yy"
(gethash 'x *tab*) => "x", T
(gethash 'y *tab*) => "yy", T
(gethash 'z *tab* 'moo) => MOO, NIL
;;------------------------------------------------------------------
