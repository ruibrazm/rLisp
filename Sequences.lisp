Sequences (Lists, Strings) and Arrays

aref
Syntax:
Symbol type: function
arefarraysubscripts(zero or more) => element
Argument description:

 array  an array  
 subscripts  a list of valid array indices  

AREF function accesses specified elements of arrays. Every array index is counted from
zero. Accessing out-of-bounds indices signals condition, or causes crash and/or undefined
behavior, depending on compilation safety mode. Note that vectors (including strings which
are special vectors) are treated as one dimensional arrays so aref works on them too. 

AREF with conjunction of SETF may be used to set array elements. 

(aref "hola" 0) => #\h
(aref "hola" 3) => #\a
(aref #(5 3 6 8) 1) => 3
(aref (make-array '(10 10) :initial-element 'moo) 9 9) => MOO

(let ((a (make-array '(3 3) :initial-element 'moo))) (setf (aref a 1 1) 'x) a) => #2A((MOO MOO MOO) (MOO X MOO) (MOO MOO MOO))
;;-------------------------------------------------------------------
concatenate
Syntax:
Symbol type: function
concatenateresult-typeseqs(one or more) => sequence
Argument description:

 result-type  sequence type specifier or NIL  
 seqs  sequences  

CONCATENATE creates new sequence and fills it with data from arguments. See also
MAPCAN. 


(concatenate 'string "hello" " " "world") => "hello world"
(concatenate 'list "hello" " " "world") => (#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d)
(concatenate 'vector "hello" " " "world") => #(#\h #\e #\l #\l #\o #\  #\w #\o #\r #\l #\d)
(concatenate 'vector '(1 2) '(3 4)) => #(1 2 3 4)
;;-------------------------------------------------------------------
copy-seq
Syntax:
Symbol type: function
copy-seqseq => sequence
Argument description:

 seq  a sequence  

COPY-SEQ function makes new sequence copy from old sequence. Note that there is no
COPY-ARRAY function, but it can be emulated by this tricky code bellow: 


(defun copy-array (array)
 (let ((dims (array-dimensions array)))
   (adjust-array
    (make-array dims :displaced-to array)
    dims)))

(let ((a "hello world")) (eq a (copy-seq a))) => NIL
(let ((a "hello world")) (equal a (copy-seq a))) => T
;;------------------------------------------------------------------
count
Syntax:
Symbol type: function
countitemsequencetest(keyword)from-end(keyword)start(keyword)end(keyword)key
(keyword)test(keyword)test-not(keyword) => integer
Argument description:

 item  an item to be found  
 sequence  a sequence to be searched  
 test  function key and item comparison  
 from-end  direction of search, default is NIL - forward  
 start  starting position for search, default is 0  
 end  final position for search, default is NIL - end  
   of sequence  
 key  function for extracting value before test  
 test  function for comparison of two values  
 test-not  function for comparison of two values  

COUNT function counts specified elements in sequence. Return value is number of
occurancesf or NIL if no occurance is not found. See also COUNT-IF, POSITION,
POSITION-IF, FIND, FIND-IF and MEMBER. 


(count #\s "Some sequence") => 1
(count #\s "Some sequence" :key #'char-downcase) => 2
(count #\s "Some sequence" :key #'char-downcase :start 1) => 1
(count #\x "Some sequence") => 0
(count '(1 2) #(9 3 (1 2) 6 7 8)) => 0
(count '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => 1
(count 1 #(0 1 0 0 0 1 0) :from-end t) => 2
;;------------------------------------------------------------------
elt
Syntax:
Symbol type: function
eltsequenceindex => element
Argument description:

 sequence  a sequence  
 index  valid sequence index  

ELT function accesses specified elements of sequences. The index is counted from zero.
Accessing out-of-bounds indices signals condition, or causes crash and/or undefined
behavior, depending on compilation safety mode. Unlike AREF, ELT works on lists too. 

ELT may by used with conjunction of SETF. 


(elt "hola" 0) => #\h
(elt "hola" 3) => #\a
(elt #(5 3 6 8) 1) => 3
(elt '(5 3 6 8) 1) => 3

(let ((a (list 1 2 3 4))) (setf (elt a 1) 'x) a) => (1 X 3 4)
(let ((a (copy-seq "hola"))) (setf (elt a 1) #\O) a) => "hOla"
;;------------------------------------------------------------------
find
Syntax:
Symbol type: function
finditemsequencetest(keyword)from-end(keyword)start(keyword)end(keyword)key(keyword)
=> element
Argument description:

 item  an item to be found  
 sequence  a sequence to be searched  
 test  function key and item comparison  
 from-end  direction of search, default is NIL - forward  
 start  starting position for search, default is 0  
 end  final position for search, default is NIL - end  
   of sequence  
 key  function for extracting value before test  

FIND function searches for an element (item) satisfying the test. Return value is element
itself or NIL if item is not found. See also POSITION, POSITION-IF, FIND, FIND-IF and
MEMBER. 


(find #\s "Some sequence") => #\s
(find #\s "Some sequence" :key #'char-downcase) => #\S
(find #\s "Some sequence" :key #'char-downcase :start 1) => #\s
(find #\x "Some sequence") => NIL
(find '(1 2) #(9 3 (1 2) 6 7 8)) => NIL
(find '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => (1 2)
(find 1 #(0 1 0 0 0 1 0) :from-end t) => 1
;;------------------------------------------------------------------
length
Syntax:
Symbol type: function
lengthseq => integer
Argument description:

 seq  sequence of objects  

LENGTH function computes length of the list, vector, string or other sequences. For lists,
LENGTH may get stuck in cyclic cons structures unlike LIST-LENGTH. 


(length "hola") => 4
(length "") => 0
(length #(2 'a 5.6)) => 3
(length #*101010101110) => 12
(length (list 'a 'b 'c)) => 3
(length nil) => 0
(length '(a . (b . nil))) => 2
(length (cons "moo" nil)) => 1
(length (cons "moo" (cons "boo" nil))) => 2
;;------------------------------------------------------------------
make-array
Syntax:
Symbol type: function
make-arraydimensionselement-type(keyword)initial-element(keyword)initial-contents
(keyword)adjustable(keyword)fill-pointer(keyword)displaced-to
(keyword)displaced-index-offset(keyword) => an array
Argument description:

 dimensions  list of dimensions, or non-negative integer  
 element-type  a type specifier, default is T - any type  
 initial-element  a value, default is implementation  
   dependent  
 initial-contents  an object  
 adjustable  a generalized boolean, default is NIL  
 fill-pointer  a valid fill pointer for the array, or T or NIL  
 displaced-to  an array or NIL, default is NIL  
 displaced-index-offset  a valid array row-major index for displaced  
   arrays, default is 0  

MAKE-ARRAY function creates a new array. Array can be adjustable if specified, that is its
dimensions can be shrinked or enlarged by ADJUST-ARRAY function. 

One-dimensional arrays can have a fill-pointer. Fill-pointer makes array look like as if it
would be shorter with only as many elements as fill-pointer specifies - while elements at the
real end of array a still retained. Such array can be very easily enlarged or shrinked in
bounds of the real size just by setting fill-pointer which is very fast. Functions like
VECTOR-PUSH, VECTOR-PUSH-EXTEND and VECTOR-POP make use of this. 

Arrays can be displaced onto another array. Such array can have different dimensions and
elements are shared on underlying row-major element order. 

See also AREF, ELT, ADJUST-ARRAY, ARRAY-DIMENSION, ARRAY-DIMENSIONS,
FILL-POINTER, ARRAY-IN-BOUNDS-P, ARRAY-ROW-MAJOR-INDEX, ARRAYP. 


(make-array 5 :initial-element 'x) => #(X X X X X)
(make-array '(2 3) :initial-element 'x) => #2A((X X X) (X X X))
(length (make-array 10 :fill-pointer 4)) => 4
(array-dimensions (make-array 10 :fill-pointer 4)) => (10)
(make-array 10 :element-type 'bit :initial-element 0) => #*0000000000
(make-array 10 :element-type 'character :initial-element #\a) => "aaaaaaaaaa"
(let ((a (make-array '(2 2) :initial-element 'x :adjustable t))) (adjust-array a '(1 3) :initial-element 'y) a) => #2A((X X Y))
;;------------------------------------------------------------------
make-sequence
Syntax:
Symbol type: function
make-sequenceresult-typesizeinitial-element(keyword) => sequence
Argument description:

 result-type  sequence type specifier  
 size  a non-negative integer  
 initial-element  element which is used to fill sequence,  
   default is implementation dependent  

MAKE-SEQUENCE creates a new sequence of specified type and number of elements.
See also MAP. 


(make-sequence 'list 4 :initial-element 'x) => (X X X X)
(make-sequence 'vector 4 :initial-element 'x) => #(X X X X)
(make-sequence 'vector 4 :initial-element #\a) => #(#\a #\a #\a #\a)
(make-sequence 'string 4 :initial-element #\a) => "aaaa"
;;------------------------------------------------------------------
map
Syntax:
Symbol type: function
mapresult-typefnseqs(one or more) => sequence or NIL
Argument description:

 result-type  sequence type specifier or NIL  
 fn  function that takes as many arguments  
   as there are sequences  
 seqs  sequences which elements are  
   processed in parallel  

MAP applies function FN to elements of sequence with same index. Each application result
is put into resulting sequence. Length of resulting sequence is the length of the shortest
sequence in argument. Return value is NIL when NIL was specified as result-type. See also
MAPC, MAPCAR and MAPCAN. 


(map 'list (lambda (x) (+ x 10)) '(1 2 3 4)) => (11 12 13 14)

(map 'vector #'identity "hola") => #(#\h #\o #\l #\a)
(map '(vector character) #'identity #(#\h #\o #\l #\a)) => "hola"
(map 'string #'identity '(#\h #\o #\l #\a)) => "hola"

(map 'vector #'list '(123 symbol "string" 345) '(1 2 3)) => #((123 1) (SYMBOL 2) ("string" 3))

(map 'list #'* '(3 4 5) '(4 5 6)) => (12 20 30)
(map 'nil #'* '(3 4 5) '(4 5 6)) => NIL
;;------------------------------------------------------------------
map-into
Syntax:
Symbol type: function
map-intoresult-sequencefnseqs(one or more) => result-sequence
Argument description:

 result-sequence  sequence type specifier or NIL  
 fn  function that takes as many arguments  
   as there are sequences  
 seqs  sequences which elements are  
   processed in parallel  

MAP-INTO applies function fn to elements of sequence with same index. Each application
result is destructively put into resulting sequence. The iteration terminates when the shortest
sequence (of any of the sequences or the result-sequence) is exhausted. Return value is
same as the first argument. See also MAP, MAPCAR and MAPCAN. 


(let ((a (list 1 2 3 4))) (map-into a #'* a a) a) => (1 4 9 16)
(let ((a (vector 1 2 3 4))) (map-into a #'* a a) a) => #(1 4 9 16)
(let ((a (vector 1 2 3 4))) (map-into a #'1+ '(1 2)) a) => #(2 3 3 4)
;;------------------------------------------------------------------
position
Syntax:
Symbol type: function
positionitemsequencetest(keyword)from-end(keyword)start(keyword)end(keyword)key
(keyword) => index or NIL
Argument description:

 item  an item to be found  
 sequence  a sequence to be searched  
 test  function key and item comparison  
 from-end  direction of search, default is NIL - forward  
 start  starting position for search, default is 0  
 end  final position for search, default is NIL - end  
   of sequence  
 key  function for extracting value before test  

POSITION function searches for an element (item) satisfying the test. Return value is index
of such item or NIL if item is not found. Index is relative to start of the sequence regardless of
arguments. See also POSITION-IF, FIND, FIND-IF and MEMBER. 


(position #\s "Some sequence") => 5
(position #\s "Some sequence" :key #'char-downcase) => 0
(position #\s "Some sequence" :key #'char-downcase :start 1) => 5
(position #\x "Some sequence") => NIL
(position '(1 2) #(9 3 (1 2) 6 7 8)) => NIL
(position '(1 2) #(9 3 (1 2) 6 7 8) :test #'equal) => 2
(position 1 #(0 1 0 0 0 1 0) :from-end t) => 5
;;------------------------------------------------------------------
reduce
Syntax:
Symbol type: function
reducefnseqinitial-value(keyword)key(keyword)from-end(keyword)start(keyword)end
(keyword) => an object
Argument description:

 fn  a two argument function  
 seq  a sequence  
 initial-value  an object  
 key  function for extracting values from  
   sequence  
 from-end  direction flag, default is NIL  
 start  bounding index  
 end  bounding index  

REDUCE applies function fn to its previous result and next element. The result is what fn
returned in last call. For the first call fn is called with either initial-value and first element or
first two elements. See also MAPCAR, MAPCAN, MAP. 


(reduce #'list '(1 2 3 4)) => (((1 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0) => ((((0 1) 2) 3) 4)
(reduce #'list '(1 2 3 4) :initial-value 0 :from-end t) => (1 (2 (3 (4 0))))
(reduce #'list '(1 2 3 4) :from-end t) => (1 (2 (3 4)))
(reduce (lambda (x y) (+ (* x 10) y)) '(1 2 3 4)) => 1234
(reduce #'+ '(1 2 3 4)) => 10
(reduce #'* '(1 2 3 4) :initial-value 1) => 24
;;------------------------------------------------------------------
remove
Syntax:
Symbol type: function
removeitemseqfrom-end(keyword)test(keyword)test-not(keyword)start(keyword)end
(keyword)count(keyword)key(keyword) => sequence
Argument description:

 item  an object  
 seq  a sequence  
 from-end  boolean specifying processing direction  
 test  equality test  
 test-not  non-equality test  
 start  bounding index, default 0  
 end  bounding index, default nil  
 count  integer for how many elements to  
   remove, or nil  
 key  function of one argument  

REMOVE make new sequence of the same type that has some elements removed. COUNT
may limit the number of removed elements. See also REMOVE-IF, DELETE, DELETE-IF,
SUBSEQ, and REMOVE-DUPLICATES. 


(remove #\s "Sample string sequence") => "Sample tring equence"
(remove #\s "Sample string sequence" :count 1) => "Sample tring sequence"
(remove #\s "Sample string sequence" :test #'char-equal) => "ample tring equence"
(remove nil '(1 2 nil 4 nil 6)) => (1 2 4 6)
;;------------------------------------------------------------------
reverse
Syntax:
Symbol type: function
reverseseq => a sequence
Argument description:

 seq  a sequence  

REVERSE function makes new sequence with reverted order of elements. See also MAP,
MAPCAR and MAPCAN. 


(reverse '(1 2 3 4)) => (4 3 2 1)
(reverse '#(1 2 3 4)) => #(4 3 2 1)
(reverse "hola") => "aloh"
;;------------------------------------------------------------------
search
Syntax:
Symbol type: function
searchsequence1sequence2test(keyword)from-end(keyword)start1(keyword)start2
(keyword)end1(keyword)end2(keyword)key(keyword) => position
Argument description:

 sequence1  a sequence to be found in sequence2  
 sequence2  a sequence to be searched  
 test  function key and item comparison  
 from-end  direction of search, default is NIL - forward  
 start1  starting position in sequence1, default is 0  
 start2  starting position in sequence2, default is 0  
 end1  final position in sequence1, default is NIL -  
   end of sequence  
 end2  final position in sequence2, default is NIL -  
   end of sequence  
 key  function for extracting value before test  

SEARCH function searches for one sequence in another. See also POSITION,
POSITION-IF, FIND, FIND-IF and MEMBER. 


(search "lo" "hello world") => 3
(search "lo" "HelLo WoRLd" :key #'char-upcase) => 3
(search "lo" "HelLo WoRLd") => NIL
;;------------------------------------------------------------------
some
Syntax:
Symbol type: function
somepredicatesequences(one or more) => T or NIL
Argument description:

 predicate  predicate function  
 sequences  sequences  

SOME function searches the sequences for values for which predicate returns true. It there
is such list of values that occupy same index in each sequence, return value is true,
otherwise false. 


(some #'alphanumericp "") => NIL
(some #'alphanumericp "...") => NIL
(some #'alphanumericp "ab...") => T
(some #'alphanumericp "abc") => T

(some #'< '(1 2 3 4) '(2 3 4 5)) => T
(some #'< '(1 2 3 4) '(1 3 4 5)) => T
(some #'< '(1 2 3 4) '(1 2 3 4)) => NIL
;;------------------------------------------------------------------
string
Syntax:
Symbol type: function
stringobject => string
Argument description:

 object  an object  

STRING function converts symbols, characters and possibly some other types into a string.
If object is of string type, it is directly returned. 


(string 'moo) => "MOO"
(string #\a) => "a"
(string "some string") => "some string"
;;-----------------------------------------------------------------
string-downcase
Syntax:
Symbol type: function
string-downcasestringstart(keyword)end(keyword) => string
Argument description:

 string  a string  
 start  integer bouded by string length  
 end  integer bouded by string length  

STRING-DOWNCASE function converts string into its upcase reprezentation. returned. See
also STRING-UPCASE, STRING-CAPITALIZE, CHAR-UPCASE and CHAR-DOWNCASE.


(string-downcase "SOME STRING") => "some string"
(string-downcase "SOME STRING" :start 2) => "SOme string"
(string-downcase "SOME STRING" :start 2 :end 8) => "SOme strING"
;;-----------------------------------------------------------------
string-upcase
Syntax:
Symbol type: function
string-upcasestringstart(keyword)end(keyword) => string
Argument description:

 string  a string  
 start  integer bouded by string length  
 end  integer bouded by string length  

STRING-UPCASE function converts string into its upcase reprezentation. returned. See also
STRING-DOWNCASE, STRING-CAPITALIZE, CHAR-UPCASE and CHAR-DOWNCASE.


(string-upcase "some string") => "SOME STRING"
(string-upcase "some string" :start 2) => "soME STRING"
(string-upcase "some string" :start 2 :end 8) => "soME STRing"
;;------------------------------------------------------------------
subseq
Syntax:
Symbol type: function
subseqseqstartend(optional) => sequence
Argument description:

 seq  a sequence  
 start  bounding index  
 end  bounding index, default NIL  

SUBSEQ function makes new sequence as a subseqence of argument. Default ending
index is end of sequence. See also COPY-SEQ and MAP. 

SUBSEQ may be used with SETF. 


(subseq "hello world" 3) => "lo world"
(subseq "hello world" 3 5) => "lo"
(let ((a "hello world")) (setf (subseq a 3 5) "LO") a) => "helLO world"
(let ((a "hello world")) (setf (subseq a 3 5) "YYY") a) => "helYY world"
;;------------------------------------------------------------------
vector
Syntax:
Symbol type: function
vectorlist(zero or more) => vector
Argument description:

 list  list of objects  

VECTOR function makes new simple general vector from arguments. See also LIST. 


(vector 1 2 3) => #(1 2 3)
(vector 'a #c(1 2) "moo") => #(A #C(1 2) "moo")
(elt (vector 1 2 3) 0) => 1
(elt (vector 1 2 3) 1) => 2
(vector) => #()
(equal #(1 2 3) (vector 1 2 3)) => NIL
(equalp #(1 2 3) (vector 1 2 3)) => T
(type-of (vector 1 2 3)) => (SIMPLE-VECTOR 3)
;;------------------------------------------------------------------
vector-pop
Syntax:
Symbol type: function
vector-popvector => an object
Argument description:

 vector  a vector with fill pointer  

VECTOR-POP function pops a element from specified vector. Supplied vector must have
fill-pointer (see MAKE-ARRAY). Fill-pointer is decremented. The element to be popped is
found at new fill-pointer position. See also MAKE-ARRAY, VECTOR-POP and
VECTOR-PUSH. Return value is object found at previous end of vector. 


(defparameter *v* (make-array 2 :fill-pointer 0)) => *V*
(vector-push 4 *v*) => 0
(vector-push 3 *v*) => 1
*v* => #(4 3)
(vector-pop *v*) => 3
(vector-pop *v*) => 4
;;------------------------------------------------------------------
vector-push
Syntax:
Symbol type: function
vector-pushnew-elementvector => index or NIL
Argument description:

 new-element  a object  
 vector  a vector with fill pointer  

VECTOR-PUSH function pushes new-element into specified vector. Supplied vector must
have fill-pointer (see MAKE-ARRAY). New element is placed at last fill-pointer position and
fill-pointer is incremented. See also MAKE-ARRAY, VECTOR-POP and VECTOR-PUSH.
Return value is index at which the new item was placed, or NIL if there is no room. 


(defparameter *v* (make-array 2 :fill-pointer 0)) => *V*
(vector-push 4 *v*) => 0
(vector-push 3 *v*) => 1
(vector-push 2 *v*) => NIL
*v* => #(4 3)
(vector-pop *v*) => 3
(vector-pop *v*) => 4
;;------------------------------------------------------------------
vector-push-extend
Syntax:
Symbol type: function
vector-push-extendnew-elementvectorextension(keyword) => index
Argument description:

 new-element  a object  
 vector  a vector with fill pointer  
 extension  a positive integer  

VECTOR-PUSH-EXTEND function pushes new-element into specified vector. Supplied
vector must be adjustable (see MAKE-ARRAY) and have fill-pointer. New element is placed
at last fill-pointer position and fill-pointer is incremented. Vector size is adjusted a number of
items as specified by extension argument, if necessary. See also MAKE-ARRAY,
VECTOR-POP and VECTOR-PUSH. Return value is index at which the new item was
placed. 


(defparameter *v* (make-array 2 :fill-pointer 0 :adjustable t)) => *V*
(vector-push-extend 4 *v*) => 0
(vector-push-extend 3 *v*) => 1
(vector-push-extend 2 *v*) => 2
*v* => #(4 3 2)
(vector-pop *v*) => 2
(vector-pop *v*) => 3
(vector-pop *v*) => 4
;;------------------------------------------------------------------
