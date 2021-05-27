Conses, Lists and related functions

append
Syntax:
Symbol type: function
appendlist(zero or more) => list
Argument description:

 list  lists to be concatenated  

APPEND function concatenates list arguments into one list. Resulting list is shallow copy of
specified lists except for the last which is directly shared. See also MAPCAN, CONS, LIST,
LIST*.

(append) => NIL
(append '(1 2 3)) => (1 2 3)
(append '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
(append '(1 2 3) '(4 5 6) '(7 8 9)) => (1 2 3 4 5 6 7 8 9)
(let ((x '(tail list))) (eq x (cddr (append '(front list) x)))) => T
;;------------------------------------------------------------------
assoc
Syntax:
Symbol type: function
associtemalistkey(keyword)test(keyword) => cons cell or NIL
Argument description:

 item  a key object  
 alist  alist - list of cons cell with key-value pairs  
 key  function for extracting key before test  
 test  function key and item comparison  

ASSOC function searches supplied list for cons cell that have item as car part. Return value
is the cell with key-value pair which key matched testing conditions, otherwise NIL. Default
comparison operator is EQL. 

Associative list, or for short alist, is a list with key-value pairs in cons cells. That is ((key1 .
value1) (key2 . value2) ...) 


(assoc 'a '((a . 1) (b . 2) (c . 3))) => (A . 1)
(assoc 'x '((a . 1) (b . 2) (c . 3))) => NIL
(assoc 'b '((a . 1) (b . 2) (c . 3) (b . 4))) => (B . 2)
(assoc "b" '(("a" . 1) ("b" . 2))) => NIL
(assoc "b" '(("a" . 1) ("b" . 2)) :test #'equal) => ("b" . 2)
(assoc 7 '((6 . a) (9 . b)) :key #'1+) => (6 . A)
(assoc 5 nil) => NIL
;;-----------------------------------------------------------------
butlast
Syntax:
Symbol type: function
butlastlistn(optional) => list
Argument description:

 list  a list  
 n  a non-negative integer, default is 1  

BUTLAST function returns the argument list copy without N last elements. See LAST. 


(butlast '(1 2 3)) => (1 2)

(butlast '(1 2 3) 0) => (1 2 3)
(butlast '(1 2 3) 1) => (1 2)
(butlast '(1 2 3) 2) => (1)
(butlast '(1 2 3) 3) => NIL
(butlast '(1 2 3) 4) => NIL
;;------------------------------------------------------------------
car
Syntax:
Symbol type: function
carlist => value
Argument description:

 list  cons or full list  

CAR function returns the first element of list, that is the car part of its cons cell argument. CAR
is identical to FIRST. 


(car '(1 2 3)) => 1
(car (cons 'a 'b)) => A
(car (cons '(1 2 3) '(a b c))) => (1 2 3)
(car '()) => NIL
(car nil) => NIL
;;-----------------------------------------------------------------
cddr
Syntax:
Symbol type: function
cddrlist => value
Argument description:

 list  cons or full list  

CDDR function is composition of two CDR functions. That is, (CDDR X) is same as (CDR
(CDR X)). There are other CAR and CDR combinations, see HyperSpec, CAR and CDR. 


(cddr '(1 2 3 4)) => (3 4)
(cddr '(1 2 . x)) => X
(cddr '(1 . nil)) => NIL
(cddr nil) => NIL
;;-----------------------------------------------------------------
cdr
Syntax:
Symbol type: function
cdrlist => value
Argument description:

 list  cons or full list  

CDR function returns cdr part of cell in the argument, that is list of all elements but first. CDR
is identical to REST. 


(cdr '(1 2 3)) => (2 3)
(cdr (cons 'a 'b)) => B
(cdr (cons '(1 2 3) '(a b c))) => (A B C)
(cdr '()) => NIL
(cdr nil) => NIL
;;-----------------------------------------------------------------
cons
Syntax:
Symbol type: function
conscar-partcar-part => cons cell
Argument description:

 car-part  an object  
 car-part  an object  

CONS function make new cons object. The cons cell contains exactly two values. The first is
named car, the second is named cdr. These cells are used to create one-way linked lists. See
also CAR, CDR and LIST. 

These names come from historical names "Contents of Address part of Register" and
"Contents of Decrement part of Register". 


(cons 1 2) => (1 . 2)
(cons 1 (cons 2 (cons 3 nil))) => (1 2 3)
(cons 1 (cons 2 (cons 3 'x))) => (1 2 3 . X)
(cons (cons (cons 'a 'b) 'c) 'd) => (((A . B) . C) . D)
(car (cons 1 2)) => 1
(cdr (cons 1 2)) => 2
;;-----------------------------------------------------------------
consp
Syntax:
Symbol type: function
conspobject => T or NIL
Argument description:

 object  an object  

CONSP function returns true if the argument refers to cons cell, otherwise it returns false. See
CONS and LIST. 


(consp nil) => NIL
(consp 'some-symbol) => NIL
(consp 3) => NIL
(consp "moo") => NIL
(consp (cons 1 2)) => T
(consp '(1 . 2)) => T
(consp '(1 2 3 4)) => T
(consp (list 1 2 3 4)) => T
;;-----------------------------------------------------------------
first
Syntax:
Symbol type: function
firstlist => value
Argument description:

 list  cons or full list  

FIRST function returns the first element of list, that is the car part of its cons cell argument.
FIRST is identical to CAR. 


(first '(1 2 3)) => 1
(first (cons 'a 'b)) => A
(first (cons '(1 2 3) '(a b c))) => (1 2 3)
(first '()) => NIL
(first nil) => NIL
;;-----------------------------------------------------------------
getf
Syntax:
Symbol type: function
getfplacekeydefault(optional) => value
Argument description:

 place  a place with list  
 key  keying value, also know as indicator  
 default  answer when key-value pair is not found,  
   default is NIL  

GETF function searches supplied plist for value with matching key. Plist is list of even
number of items. Each item pair specifies key and value. I.e. (K1 V1 K2 V2 ...). Return value
is either value for first matching key, or specified default. Keys are matched by EQ function,
therefore only suitable values are symbols and integers in range between
MOST-NEGATIVE-FIXNUM and MOST-POSITIVE-FIXNUM constants. See also SETF,
ASSOC and FIND. 


(getf '(a b 4 d a x) 'a) => B
(getf '(a b 4 d a x) 'x) => NIL
(getf '(a b 4 d a x) 'x 'not-found) => NOT-FOUND
(getf '(a b 4 d a x) 4 'not-found) => D
;;-----------------------------------------------------------------
last
Syntax:
Symbol type: function
lastlistn(optional) => list
Argument description:

 list  a list  
 n  a non-negative integer, default is 1  

LAST function returns the list of N last elements of list argument. See BUTLAST. 


(last '(1 2 3)) => (3)

(last '(1 2 3) 0) => NIL
(last '(1 2 3) 1) => (3)
(last '(1 2 3) 2) => (2 3)
(last '(1 2 3) 3) => (1 2 3)
(last '(1 2 3) 4) => (1 2 3)
(last '(a . b) 0) => B
(last '(a . b) 1) => (A . B)
(last '(a . b) 2) => (A . B)
;;-----------------------------------------------------------------
list
Syntax:
Symbol type: function
listlist(zero or more) => list
Argument description:

 list  list of objects  

LIST function makes new list from arguments. 


(list 1 2 3) => (1 2 3)
(list 'a #c(1 2) "moo") => (A #C(1 2) "moo")
(car (list 1 2 3)) => 1
(cdr (list 1 2 3)) => (2 3)
(list) => NIL
(eq (list) nil) => T
(eq (list) '()) => T
(equal (list 1) (cons 1 nil)) => T
(equal (list 1 'a) (cons 1 (cons 'a nil))) => T
(equal (list 1 'a 3) (cons 1 (cons 'a (cons 3 nil)))) => T
(equal (list 1 'a 3) '(1 . (a . (3 . nil)))) => T
(equal '(1 2 3) (list 1 2 3)) => T
;;-----------------------------------------------------------------
list-length
Syntax:
Symbol type: function
list-lengthlist => integer or NIL
Argument description:

 list  list or cyclic list  

LIST-LENGTH function computes length of the lists. LIST-LENGTH will return NIL if it
encounters cyclic cons cell structure. LIST-LENGTH is slower than LENGTH because of
additional cycle checking. 


(list-length '(a . (b . nil))) => 2
(list-length '#1=(a . (b . #1#))) => NIL

(list-length (list 'a 'b 'c)) => 3
(list-length nil) => 0
(list-length (cons "moo" nil)) => 1
(list-length (cons "moo" (cons "boo" nil))) => 2
;;-----------------------------------------------------------------
listp
Syntax:
Symbol type: function
listpobject => T or NIL
Argument description:

 object  an object  

LISTP function returns true if the argument is refers to object of type list; otherwise it returns
false. Objects of list type can contain cons cells or NIL value (list terminator). See CONS,
CONSP and LIST. 


(listp nil) => T
(listp 'some-symbol) => NIL
(listp 3) => NIL
(listp "moo") => NIL
(listp (cons 1 2)) => T
(listp '(1 . 2)) => T
(listp '(1 2 3 4)) => T
(listp (list 1 2 3 4)) => T
;;-----------------------------------------------------------------
mapc
Syntax:
Symbol type: function
mapcfnlists(one or more) => the first list from lists argument
Argument description:

 fn  function that takes as many arguments as  
   there are lists  
 lists  lists which elements are processed in  
   parallel  

MAPC applies function FN to elements of lists with same index. Each application result
forgotten. Elemnts are processed only up to length of the shortest list argument. See
MAPCAR, MAPCAN, MAPCON, DOLIST. 


(setq dummy nil) =>  NIL
(mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
      '(1 2 3 4)
      '(a b c d e)
      '(x y z)) =>  (1 2 3 4)
dummy => (1 A X 2 B Y 3 C Z)
;;----------------------------------------------------------------
mapcan
Syntax:
Symbol type: function
mapcanfnlists(one or more) => list
Argument description:

 fn  function that takes as many arguments as  
   there are lists  
 lists  lists which elements are processed in  
   parallel  

MAPCAN applies function FN to elements of lists with same index. Each application result
is concatenated into resulting list. See MAPCAR. 


(mapcan (lambda (x) (list (+ x 10) 'x)) '(1 2 3 4)) => (11 X 12 X 13 X 14 X)

(mapcan #'list '(a b c d)) => (A B C D)

(mapcan (lambda (x) (if (> x 0) (list x) nil)) '(-4 6 -23 1 0 12 )) => (6 1 12)
;;-----------------------------------------------------------------
mapcar
Syntax:
Symbol type: function
mapcarfnlists(one or more) => list
Argument description:

 fn  function that takes as many arguments as  
   there are lists  
 lists  lists which elements are processed in  
   parallel  

MAPCAR applies function FN to elements of lists with same index. Each application result
is put into resulting list. Length of resulting list is the length of the shortest list argument. See
MAPCAN. 


(mapcar (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)

(mapcar #'round '(1.3 2.7 3.4 4.5)) => (1 3 3 4)

(mapcar #'list '(123 symbol "string" 345) '(1 2 3)) => ((123 1) (SYMBOL 2) ("string" 3))

(mapcar #'* '(3 4 5) '(4 5 6)) => (12 20 30)
;;-----------------------------------------------------------------
mapcon
Syntax:
Symbol type: function
mapconfnlists(one or more) => list
Argument description:

 fn  function that takes as many arguments as  
   there are lists  
 lists  lists which elements are processed in  
   parallel  

MAPCON applies function FN to the successive cdr of lists. Each application result is
DESTRUCTIVELY concatenated into resulting list. In case of FN results that are fresh lists
(non-sharing), the result is same as with (APPLY #'APPEND (MAPLIST ...)). See MAPCAR,
MAPCAN, MAPCON, MAP, MAPC. 


(mapcon (lambda (x) (list 'start x 'end)) '(1 2 3 4))
=> (START (1 2 3 4) END START (2 3 4) END START (3 4) END START (4) END)
;;-----------------------------------------------------------------
maplist
Syntax:
Symbol type: function
maplistfnlists(one or more) => list
Argument description:

 fn  function that takes as many arguments as  
   there are lists  
 lists  lists which elements are processed in  
   parallel  

MAPLIST applies function FN to the successive cdr of lists. Each application result is
concatenated into resulting list. See MAPCAR, MAPCAN, MAPCON, MAP, MAPC. 


(maplist (lambda (x) (list 'start x 'end)) '(1 2 3 4))
=> ((START (1 2 3 4) END) (START (2 3 4) END) (START (3 4) END) (START (4) END))
;;-----------------------------------------------------------------
member
Syntax:
Symbol type: function
memberitemlisttest(keyword)key(keyword) => tail or NIL
Argument description:

 item  an item to be found  
 list  a list to be searched  
 test  function key and item comparison  
 key  function for extracting value before test  

MEMBER function searches a list for the first occurrence of an element (item) satisfying the
test. Return value is tail of the list starting from found element or NIL when item is not found.
See also MEMBER-IF, POSITION, POSITION-IF, FIND and FIND-IF. 


(member 1 '(0 1 0 0 0 1 0)) => (1 0 0 0 1 0)
(member 2 '(0 1 0 0 0 1 0)) => NIL
(member #\h '(#\H #\o #\l #\a)) => NIL
(member #\h '(#\H #\o #\l #\a) :test #'char-equal) => (#\H #\o #\l #\a)
(member #\h '(#\H #\o #\l #\a) :key #'char-downcase) => (#\H #\o #\l #\a)
;;-----------------------------------------------------------------
null
Syntax:
Symbol type: function
nullobject => T or NIL
Argument description:

 object  an object  

NULL function returns true if the argument is NIL, otherwise it returns false. NULL is identical
to NOT, but used in conjunction with list processing unlike NOT which is used in boolean
logic processing. 


(null '()) => T
(null '(1 2 3)) => NIL
(null nil) => T
(null t) => NIL
(null 234.4) => NIL
(null "lisp") => NIL
;;-----------------------------------------------------------------
pop
Syntax:
Symbol type: macro
popplace => list
Argument description:

 place  a place containing list  

POP macro modifies variable or generally place. It replaces the cons cell value with its cdr.
Effectively it removes first element of the list found at the place. Result is the first element of
the original list. See also PUSH, PUSH-NEW and ACONS. 


(let ((x '(1 2 3))) (pop x)) => 1
(let ((x '(1 2 3))) (pop x) x) => (2 3)
(let ((x '((a b c) (3 2 1) (e f g)))) (pop (second x)) x) => ((A B C) (2 1) (E F G))
(let ((x '())) (pop x) x) => NIL
;;-----------------------------------------------------------------
push
Syntax:
Symbol type: macro
pushitemplace => list
Argument description:

 item  an object  
 place  a place which can contain any object, but  
   usually list  

PUSH macro modifies variable or generally place. It makes a new cons cell filled with item
as car and previous value as cdr, that is effectively prepends new item to list found at the
place. See also PUSH-NEW, ACONS and POP. 


(let ((x 'x)) (push 4 x) x) => (4 . X)
(let ((x '(3 2 1))) (push 4 x) x) => (4 3 2 1)
(let ((x '((a b c) (3 2 1) (e f g)))) (push 4 (second x)) x) => ((A B C) (4 3 2 1) (E F G))
;;-----------------------------------------------------------------
pushnew
Syntax:
Symbol type: macro
pushnewitemplacekey(keyword)test(keyword) => list
Argument description:

 item  an object  
 place  a place which can contain any object, but  
   usually list  
 key  function for extracting value before test  
 test  function key and item comparison  

PUSHNEW macro modifies variable or generally place. It conditionally makes a new cons
cell filled with item as car and previous value as cdr, that is effectively prepends new item to
list found at the place. New element is pushed only when it does not appear in place. Test
argument specifies comparison operator. Default comparison operator is EQL. Key
argument specifies function for extracting relevant value from list items. Default key is
IDENTITY. See also PUSH-NEW, ACONS and POP. 


(let ((x 'x)) (pushnew 4 x) x) => (4 . X)
(let ((x '(3 2 1))) (pushnew 4 x) x) => (4 3 2 1)
(let ((x '(3 2 1))) (pushnew 3 x) x) => (3 2 1)
(let ((x '((a b c) (3 2 1) (e f g)))) (pushnew 4 (second x)) x) => ((A B C) (4 3 2 1) (E F G))
(let ((x '((a b c) (3 2 1) (e f g)))) (pushnew 3 (second x)) x) => ((A B C) (3 2 1) (E F G))
(let ((x '("3" "2" "1"))) (pushnew "3" x) x) => (3 2 1)
(let ((x '("31" "24" "13"))) (pushnew "44" x :key (lambda (x) (elt x 0))) x) => ("44" "31" "24" "13")
(let ((x '("31" "24" "13"))) (pushnew "44" x :key (lambda (x) (elt x 1))) x) => ("31" "24" "13")
;;-----------------------------------------------------------------
rest
Syntax:
Symbol type: function
restlist => value
Argument description:

 list  cons or full list  

REST function returns list of all elements but first, that is cdr part of argument. REST is
identical to CDR. 


(rest '(1 2 3)) => (2 3)
(rest (cons 'a 'b)) => B
(rest (cons '(1 2 3) '(a b c))) => (A B C)
(rest '()) => NIL
(rest nil) => NIL
;;-----------------------------------------------------------------
rplaca
Syntax:
Symbol type: function
rplacaconsobject => cons
Argument description:

 cons  a cons cell  
 object  an object  

RPLACA function changes CAR part of CONS cell to specified value. See RPLACD,
SETF, CONS. 

This can be also writen as (SETF (CAR cons) object). 


(let ((my-list (list 5 3 6 2))) (rplaca my-list 'bla) my-list) => (BLA 3 6 2)
;;-----------------------------------------------------------------
rplacd
Syntax:
Symbol type: function
rplacdconsobject => cons
Argument description:

 cons  a cons cell  
 object  an object  

RPLACD function changes CDR part of CONS cell to specified value. See RPLACA,
SETF, CONS. 

This can be also writen as (SETF (CDR cons) object). 


(let ((my-list (list 5 3 6 2))) (rplaca (cdr my-list) '(x y)) my-list) => (5 (X Y) 6 2)
;;-----------------------------------------------------------------
second
Syntax:
Symbol type: function
secondlist => value
Argument description:

 list  cons or full list  

SECOND function returns second element of list, that is car part of cdr part of its cons cell.
SECOND is identical to CADR. 


(second '(1 2 3)) => 2
(second (cons 'a (cons 'b 'c))) => B
;;-----------------------------------------------------------------
set-difference
Syntax:
Symbol type: function
set-differencelist1list2key(keyword)test(keyword) => list
Argument description:

 list1  a list  
 list2  a list  
 key  function for extracting value before test  
 test  function key and item comparison  

SET-DIFFERENCE function computes set difference, that is a list of elements that appear
in list1 but do not appear in list2. Test argument specifies comparison operator. Default
comparison operator is EQL. Key argument specifies function for extracting relevant value
from list items. Default key is IDENTITY. Resulting item order is not specified. See also
SET-EXCLUSIVE-OR, UNION and INTERSECTION. 


(set-difference '(a b c) '(b c d)) => (A)
(set-difference '("a" "b" "c") '("b" "c" "d")) => ("c" "b" "a")
(set-difference '("a" "b" "c") '("b" "c" "d") :test #'equal) => ("a")
(set-difference '((a . 2) (b . 3) (c . 1)) '((b . 1) (c . 2) (d . 4)) :test #'equal) => ((C . 1) (B . 3) (A . 2))
(set-difference '((a . 2) (b . 3) (c . 1)) '((b . 1) (c . 2) (d . 4)) :key #'car) => ((A . 2))
(set-difference '((a . 2) (b . 3) (c . 1)) '((b . 1) (c . 2) (d . 4)) :key #'cdr) => ((B . 3))
;;-----------------------------------------------------------------
union
Syntax:
Symbol type: function
unionlist-1list-2key(keyword)test(keyword)test-not(keyword) => list
Argument description:

 list-1  list to be joined  
 list-2  other list to be joined  
 key  function for extracting value before test  
 test  function for comparison of two values  
 test-not  function for comparison of two values  

UNION function computes union of two lists. Resulting list contains elements that appear in
one or other list. See INTERSECTION, SET-DIFFERENCE, SET-EXCLUSIVE-OR. 


(union '(1 2 3) '(2 3 4)) => (1 2 3 4)
(union '((1) (2) (3)) '((2) (3) (4))) => ((3) (2) (1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :test #'equal) => ((1) (2) (3) (4))
(union '((1) (2) (3)) '((2) (3) (4)) :key #'first) => ((1) (2) (3) (4))
;;-----------------------------------------------------------------
