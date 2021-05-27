Input and output

format
Syntax:
Symbol type: function
formatdestinationcontrol-stringargs(zero or more) => string or NIL
Argument description:

 destination  T, NIL, stream or string with fill-pointer  
 control-string  a string with formating directives  
 args  format arguments for control-string  

FORMAT function does a complex text formatting. Formatting rules are driven by
control-string and arguments in arg. When destination is stream or string with fill-pointer, the
resulting string is written to it. T as a destination means "write to terminal". NIL as destination
means "return the formatted string back as string". See also WRITE-STRING, TERPRI,
PRINC, PRIN1 and PRINT. 

Control string is composed of normal text and embedded directives. Directives begin with
tilde (~) character. Most common are: ~a - output with aesthetics, ~s - standard output, ~%
newline, tilde parenthesis - flow control, tilde tilde - escape sequence for tilde. See full
documentation or examples for more. 


(format nil "Items in list:~%~{~a, ~}" '(1 2 3 4)) => "Items in list:
1, 2, 3, 4, "
(format nil "~{~a~^, ~}" '(1 2 3 4)) => "1, 2, 3, 4"
(format nil "~f" 3.141592) => "3.141592"
(format nil "~2,3f" 3.141592) => "3.142"
(format nil "~7,3f" 3.141592) => "  3.142"
(format nil "~a ~s" "xyz" "xyz") => "xyz \"xyz\""
;;-------------------------------------------------------------------
read
Syntax:
Symbol type: function
readinput-stream(optional)eof-error-p(optional)eof-value(optional)recursive-p(optional) => an
object
Argument description:

 input-stream  an input stream, default is standard  
   input  
 eof-error-p  a boolean, true (default) is EOF should  
   be signaled  
 eof-value  an object that is returned as EOF value  
 recursive-p  flag to note recursive processing  

READ function reads arbitrary readable lisp object from input stream. Reading process uses
*read-table*. Note that *read-eval* global variable controls read-time evaluation (#. macro). 


(let ((s (make-string-input-stream "(1 2 3)"))) (read s)) => (1 2 3)
(let ((s (make-string-input-stream "#(1 2 3)"))) (read s)) => #(1 2 3)
(let ((s (make-string-input-stream "\"hola\""))) (read s)) => "hola"
;;------------------------------------------------------------------
read-char
Syntax:
Symbol type: function
read-charinput-stream(optional)eof-error-p(optional)eof-value(optional)recursive-p
(optional) => char
Argument description:

 input-stream  an input stream, default is standard  
   input  
 eof-error-p  a boolean, true (default) is EOF should  
   be signaled  
 eof-value  an object that is returned as EOF value  
 recursive-p  flag to note recursive processing  

READ-CHAR function reads a character from input stream. 


(let ((s (make-string-input-stream (format nil "line 1~%line 2~%line 3)")))) (read-char s))
=> #\l
;;------------------------------------------------------------------
read-line
Syntax:
Symbol type: function
read-lineinput-stream(optional)eof-error-p(optional)eof-value(optional)recursive-p(optional)
=> line, missing-newline-p
Argument description:

 input-stream  an input stream, default is standard  
   input  
 eof-error-p  a boolean, true (default) is EOF should  
   be signaled  
 eof-value  an object that is returned as EOF value  
 recursive-p  flag to note recursive processing  

READ-LINE function reads a line from input stream into string. 


(let ((s (make-string-input-stream (format nil "line 1~%line 2~%line 3)")))) (read-line s))
=> "line 1", NIL
;;------------------------------------------------------------------
write-string
Syntax:
Symbol type: function
write-stringstringoutput-stream(optional)start(keyword)end(keyword) => string
Argument description:

 string  a string  
 output-stream  a stream, default is standard output  
 start  bounding index  
 end  bounding index  

WRITE-STRING function writes string into standard output or specified output stream. See
WRITE-LINE, FORMAT. 


(write-string "xyz")
xyz
=> "xyz"
;;------------------------------------------------------------------
