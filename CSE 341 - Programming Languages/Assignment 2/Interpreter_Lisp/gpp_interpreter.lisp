;;; =================================================
;;; =======     UMUT AY ALPER - 1801042097    =======
;;; =================================================
;; LOADS THE LEXER CODE
(load "gpp_lexer.lisp")

(setq KEYWORDS '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while"))
(setq OPERATORS '("+" "-" "/" "*" "(" ")" "**" """" ","))

(defun POP-LAST (list)
    (loop for l on list
        while (rest l)
        collect (first l)
    )
)
(defun REVERSE (l)
    (cond
        ((null l) '())
        (T (append (REVERSE (cdr l)) (list (car l))))
    )
) 
(defun CONCATENATE(seq1 seq2)
    (cond ((not (null seq1)) (cons (car seq1) (CONCATENATE (cdr seq1) seq2)))
          (T (cond ((not (null seq2)) (cons (car seq2) (CONCATENATE seq1 (cdr seq2))))
                   (T nil))))
)
(defun CHECK-SUBSTRING (string1 string2)
    (cond
        ((zerop (length string1)) nil) 
        ((> (length string1) (length string2)) nil) 
        ((string= string1 (subseq string2 0 (length string1))) string1) 
        (t (CHECK-SUBSTRING string1 (subseq string2 1)))
    )
) 
(defun CHECK-TOKEN(liste)
    (setq OPcount 0)
    (setq CPcount 0)
    (setq deffunControl 0)
    (dolist (subList liste)
        (if (equal (nth 0 subList) "OP_OP")
            (setq OPcount (+ OPcount 1))
        )
        (if (equal (nth 0 subList) "OP_CP")
            (setq CPcount (+ CPcount 1))
        )
        (if (not (equal (CHECK-SUBSTRING "SYNTAX__ERROR" (nth 0 subList)) nil))
            (progn 
                (if (not (equal (nth 1 sublist) "'"))
                    (return-from CHECK-TOKEN nil)
                )
            )
        )
        (if (equal (nth 0 subList) "KW_DEFFUN")
            (setq deffunControl 1)
        )
    )

    (if (not (equal OPcount CPcount))
        (return-from CHECK-TOKEN nil)
        (progn 
            (if (equal deffunControl 1)
                (return-from CHECK-TOKEN "OP-DEFUN")
                (return-from CHECK-TOKEN T)
            )
        )
    )
)
(defun CHECK-DOUBLE-ASTERIKS(number1 number2)
    (setq result 1)
    (setq control 0)
    (loop 
        (setq result (* result number1))
        (setq control (+ control 1))
        (when (equal control number2) (return-from CHECK-DOUBLE-ASTERIKS result))
    )
)
(defun CHECK-INT(expression)
    (setq controlValue 0)
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (return-from CHECK-INT nil)
                )
            )
        )
        (setq controlValue 1)
    )
    T
)
(defun CHECK-BINARY(expression)
    (setq controlValue 0)
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (or (equal element "true") (equal element "false") (equal element "\"true\"") (equal element "\"false\"")) nil)
                    (return-from CHECK-BINARY nil)
                )
            )
        )
        (setq controlValue 1)
    )
    T
)
(defun CHECK-BOOLEAN(trueORfalse)
    (if (or (equal trueORfalse "true")  (equal trueORfalse"\"true\""))
        T
        nil
    )
)
(defun OP-PLUS(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (+ (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)
(defun OP-MINUS(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (- (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)
(defun OP-DIVIDE(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (/ (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)
(defun OP-MULTIPLY(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (* (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression)))
        nil
    )
)
(defun OP-ASTERIKS(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (return-from OP-ASTERIKS (CHECK-DOUBLE-ASTERIKS (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
        nil
    )
)
(defun OP-AND(expression)
    (if (equal (not (CHECK-BINARY expression)) nil)
        (progn 
            (setq resultB (and (CHECK-BOOLEAN (nth 1 expression)) (CHECK-BOOLEAN (nth 2 expression))))
            (if (equal resultB T)
                (return-from OP-AND "true")
                (return-from OP-AND "false")
            )
        )
        nil
    )
)
(defun itisOR(expression)
    (if (equal (not (CHECK-BINARY expression)) nil)
        (progn 
            (setq resultB (or (CHECK-BOOLEAN (nth 1 expression)) (CHECK-BOOLEAN (nth 2 expression))))
            (if (equal resultB T)
                (return-from itisOR "true")
                (return-from itisOR "false")
            )
        )
        nil
    )
)
(defun OP-NOT(expression)
    (if (equal (not (CHECK-BINARY expression)) nil)
        (progn 
            (setq resultB (not (CHECK-BOOLEAN (nth 1 expression))))
            (if (equal resultB T)
                (return-from OP-NOT "true")
                (return-from OP-NOT "false")
            )
        )
        nil
    )
)
(defun OP-EQUAL(expression)
    (if (equal (not (CHECK-BINARY expression)) nil)
        (progn 
            (setq resultB (equal (CHECK-BOOLEAN (nth 1 expression)) (CHECK-BOOLEAN (nth 2 expression))))
            (if (equal resultB T)
                (return-from OP-EQUAL "true")
                (return-from OP-EQUAL "false")
            )
        )
        (progn 
            (if (equal (not (CHECK-INT expression)) nil)
                (progn 
                    (setq resultB (equal (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
                    (if (equal resultB T)
                        (return-from OP-EQUAL "true")
                        (return-from OP-EQUAL "false")
                    )
                )
                nil
            )
        )
    )
)
(defun OP-LESS-THAN(expression)
    (if (equal (not (CHECK-INT expression)) nil)
        (progn 
            (setq resultB (< (parse-integer (nth 1 expression)) (parse-integer (nth 2 expression))))
            (if (equal resultB T)
                (return-from OP-LESS-THAN "true")
                (return-from OP-LESS-THAN "false")
            )
        )
        nil
    )
)
(defun OP-SET(expression) 
    (if (equal (parse-integer (nth 2 expression) :junk-allowed t) nil)
        (progn 
            (if (equal (type-of (nth 2 expression)) 'CONS)
                (return-from OP-SET (nth 2 expression))
                (return-from OP-SET nil)
            )
        )
        (return-from OP-SET (parse-integer (nth 2 expression)))
    )
)
(defun OP-LIST(expression)
    (setq controlValue 0)
    (setq returnList '())
    (dolist (element expression) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (progn 
                        (setq liste1 (read-from-string element))
                        (if (equal (type-of liste1) 'CONS)
                            (setq returnList (append returnList (list liste1)))
                            (return-from OP-LIST nil)
                        )
                    )
                    (setq returnList (append returnList (list (parse-integer element)))) 
                )
            )
        )
        (setq controlValue 1)
    )
    (return-from OP-LIST returnList)
)
(defun OP-APPEND(expression) 
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 expression)))
    (setq liste2 (read-from-string (nth 2 expression)))
    (if (equal (type-of liste2) 'CONS)
        (progn 
            (if (equal (parse-integer (nth 1 expression) :junk-allowed t) nil)
                (progn 
                    (if (equal (type-of liste1) 'CONS)
                        (progn 
                            (setq liste2 (append liste1 liste2))
                            (return-from OP-APPEND liste2) 
                        )
                    )
                    nil
                )
                (progn 
                    (setq liste2 (append liste2 (parse-integer (nth 1 expression))))
                    (return-from OP-APPEND liste2)
                )
            )
        )
        nil
    )
)
(defun OP-CONCATENATE(expression)
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 expression)))
    (setq liste2 (read-from-string (nth 2 expression)))   
    (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
        (return-from OP-CONCATENATE (CONCATENATE liste1 liste2))
        nil
    )
)
(defun OP-IF(expression)
    (setq EX_BI (nth 1 expression))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) nil)
        (return-from CHECK-BINARY nil)
        (progn 
            (cond 
                ((equal (length expression) 3)
                    (progn 
                        (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                            (return-from OP-IF "false")
                            (progn 
                                (setq liste2 '())
                                (setq liste2 (read-from-string (nth 2 expression)))
                                (if (equal (type-of liste2) 'CONS)
                                    (return-from OP-IF liste2)
                                    nil
                                )     
                            )
                        )                    
                    )
                )
                ((equal (length expression) 4)
                    (progn 
                        (setq liste1 '())
                        (setq liste2 '())
                        (setq liste1 (read-from-string (nth 2 expression)))
                        (setq liste2 (read-from-string (nth 3 expression)))   
                        (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
                            (progn 
                                (if (or (equal EX_BI "true") (equal EX_BI "\"true\""))
                                    (return-from OP-IF liste1)
                                    (return-from OP-IF liste2)
                                )
                            )
                            nil
                        )
                    )
                )
                (t nil)
            )
        )
    )
)
(defun OP-WHILE(expression) 
    (setq EX_BI (nth 1 expression))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) T)
        (progn 
            (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                (return-from OP-WHILE "false")
                (progn 
                    (setq liste2 '())
                    (setq liste2 (read-from-string (nth 2 expression)))
                    (if (equal (type-of liste2) 'CONS)
                        (return-from OP-WHILE liste2)
                        nil
                    )     
                )
            )
        )
    )
)
(defun OP-LOAD(expression)
    (if (type-of (nth 1 expression) 'simple-array)
        (return-from OP-LOAD (nth 1 expression))
        nil
    )
)
(defun OP-DEFUN(expression)
    (format t "SYNTAX OK. ~%")
)
(defun OP-CHECK(expression) 
    (cond 
        ( (not (equal (searchList (nth 0 expression) OPERATORS) nil))
            ;; operator kontrolü ...
            (progn 
                (cond 
                    ( (equal (nth 0 expression) "+") ;; + operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (OP-PLUS expression))
                                (return-from OP-CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "-") ;; - operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (OP-MINUS expression))
                                (return-from OP-CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "/") ;; / operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (OP-DIVIDE expression))
                                (return-from OP-CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "*") ;; "*" operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (OP-MULTIPLY expression))
                                (return-from OP-CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 expression) "**") ;; "**"  operatoru ise
                        (if (equal (length expression) 3)
                            (progn 
                                (setq result (OP-ASTERIKS expression))
                                (return-from OP-CHECK result)
                            )
                            nil
                        )
                    )
                )
            )
        )
        ( (not (equal (searchList (nth 0 expression) KEYWORDS) nil))
            ;; keyword kontrolü ...
            (cond 
                ( (equal (nth 0 expression) "and") ;; and keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-AND expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "or") ;; or keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (itisOR expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "not") ;; not keywordu ise
                    (if (equal (length expression) 2)
                        (progn 
                            (setq result (OP-NOT expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "equal") ;; equal keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-EQUAL expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "less") ;; less keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-LESS-THAN expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "set") ;; set keywordu ise
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-SET expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "list") ;; set keywordu ise
                    (progn 
                        (setq result "")
                        (if (equal (length expression) 1) 
                            (setq result "NIL")
                            (setq result (OP-LIST expression))
                        )
                        (return-from OP-CHECK result)
                    )  
                )
                ( (equal (nth 0 expression) "append") ;; append keywordu ise
                    (progn 
                        (setq result (OP-APPEND expression))
                        (return-from OP-CHECK result)
                    )  
                )
                ( (equal (nth 0 expression) "concat") ;; concat keywordu ise 
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-CONCATENATE expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "if") ;; if keywordu ise 
                    (progn 
                        (setq result (OP-IF expression))
                        (return-from OP-CHECK result) 
                    )
                    nil
                )
                ( (equal (nth 0 expression) "while") ;; while keywordu ise 
                    (if (equal (length expression) 3)
                        (progn 
                            (setq result (OP-WHILE expression))
                            (return-from OP-CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 expression) "deffun") ;; deffun keywordu ise 
                    (return-from OP-CHECK "SYNTAX OK.")     
                )
                ( (equal (nth 0 expression) "load") ;; load keywordu ise 
                    (if (equal (length expression) 2)
                        (progn 
                            (setq result (OP-LOAD expression))
                            (return-from OP-CHECK result)   
                        )
                        nil
                    ) 
                )
            )
        )
        (t nil)
    )
)
(defun OP-PARSER(string_)
    ;; Stackler string tutacaklar ...
    (setq stack1 '())
    (setq stack2 '())
    (setq word "")
    (setq controlFirst T)
    (setq listControl nil)
    (setq previousCH #\1)

    (loop for ch across string_ do 
        
        ;; (format t "~%Stack 1 : ~a~%" stack1)
        ;; (format t "Stack 2 : ~a~%" stack2)
        (if (equal controlFirst T)
            (progn 
                (if (equal (or (equal ch (code-char 39.)) (equal ch (code-char 40))) T)
                    (progn 
                        (if (equal ch (code-char 39.)) 
                            (setq listControl T)
                        )
                        (setq controlFirst nil)
                    )
                    (progn 
                        (format t "~%SYNTAX_ERROR Expression not recognized~%")
                        (return-from OP-PARSER nil)
                    )
                )
            )
            (progn
                (cond 
                    ((equal ch (code-char 41.)) ;; Kapalı parantez kontrolü
                        (progn 
                            (setq rev-stack1 (REVERSE stack1))
                            (dolist (element rev-stack1) ;; Acık parantez bulana kadar git ...
                                (if (equal element (string (code-char 40.)))
                                    (progn 
                                        (setq stack1 (POP-LAST stack1)) 
                                        (return)
                                    )
                                    (progn 
                                        (setq stack2 (append stack2 (list element)))
                                        (setq stack1 (POP-LAST stack1)) 
                                    ) 
                                )
                            )
                            (setq stack2 (REVERSE stack2))  ;; Listenin tersini almak lazım yoksa problem cikarmaktadir ...
                            (setq control (OP-CHECK stack2))
                            (if (equal control nil)
                                (progn 
                                    (format t "~%SYNTAX_ERROR Expression not recognized~%")
                                    (return-from OP-PARSER nil)
                                )
                                (progn 
                                    (setq stack2 '())
                                    (setq stack1 (append stack1 (list (write-to-string control))))
                                )                    
                            )
                        )
                    )

                    ((equal ch #\space) ;; Bosluk kontrolu ...
                        (progn 
                            (if (not (equal word ""))
                                (progn 
                                    (setq stack1 (append stack1 (list word))) ;; Bosluksa stack1'e push et ...
                                    (setq word "")
                                )
                            )
                        )
                    )
                    
                    ((equal listControl T) ;; ilk basta '() ile liste olusturabildiginden onu kontrol eder ...
                        (progn 
                            (if (equal ch (code-char 40.))
                                (progn 
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq listControl nil)
                                )
                            )
                        )   
                    )

                    ((not (equal ch #\;))    ; Yorum satırını anlamak icin yazılmıştır. Yorum satırı ise program bir şey yapmayacak.
                        (progn 
                            (if (and (equal previousCH #\') (equal ch (code-char 40.)))
                                (progn ;; '( gelirse liste oldugunu anlaması icin
                                    (setq stack1 (append stack1 (list "(")))
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq word "")
                                )
                                (setf word (concatenate 'string word (string ch)))
                            )
                        )   
                    )
                )
            )
        )
        (setq previousCH ch)
	)

    ;; Sonucun ekrana basılması ...
    (if (equal (nth 0 stack1) "\"true\"")
        (format t "true")
        (progn 
            (if (equal (nth 0 stack1) "\"false\"")
                (format t "false")
                (progn 
                    (if (not (equal (nth 0 stack1) nil))
                        (format t "~a" (nth 0 stack1))
                    )
                )
            )
        )
    )
    (terpri)
)
(defun GPPINTERPRETER() 
    (format t "###############################################################~%")
    (format t "################ THIS IS THE G++ INTERPRETER ##################~%")
    (format t "################    GOKHAN HAS - 161044067   ##################~%")
    (format t "#### g++ dosyaismi.g++ yazarak dosyadan test edebilirsiniz ####~%")
    (format t "###################  g++ helloword.g++  #######################~%")
    (format t "###############################################################~%")
    (setq number 1)
    (loop 
        (terpri)
        (format t "[~d]> " number)
        (setq read_string (string-downcase (string (read-line))))
        (if (equal (CHECK-SUBSTRING "g++" read_string) nil)
            (progn 
                (if (not (equal read_string "exit"))
                    (progn 
                        (setq controlLOAD 0)
                        (setq number (+ number 1))
                        (setq tokens '())
                        (setq list_ '())
                        (setq list_ (append list_ (list read_string)))
                        (setq read_string (nth 0 (trim-list list_)))
                        
                        (if (not (equal (CHECK-SUBSTRING "( load" read_string) nil))
                            (progn
                                (format t "~%~a~%" (subseq read_string 6 (- (length read_string) 1)))
                                (setq controlLOAD 1)
                            )
                        )
                        (if (equal controlLOAD 0)
                            (progn 
                                (setq tokens (gpp_lexer read_string))
                                (if (not (equal (CHECK-TOKEN tokens) nil))
                                    (progn 
                                        (if (equal (CHECK-TOKEN tokens) "OP-DEFUN")
                                            (OP-DEFUN tokens)
                                            (OP-PARSER read_string)
                                        )
                                    )
                                    (format t "~%SYNTAX_ERROR Expression not recognized~%")
                                )
                            )
                        )
                    )
                    (format t "BYE.")
                )
            )
            (progn ;; Dosyadan okuma yapılacak ..
                (setq number (+ number 1))
                (setq read_string (subseq read_string 4 (length read_string)))
                (setq line2 "")
                (let ((in (open read_string :if-does-not-exist nil)))
                    (when in
                        (loop for line = (read-line in nil)
                            while line do 
                            (progn 
                                (setq line2 line)
                                (setq list_ '())
                                (setq list_ (append list_ (list line)))
                                (setq read_string (nth 0 (trim-list list_)))
                                (OP-PARSER read_string)
                            )
                        )
                        (close in)
                    )
                )
            )
        )
        
        (when (equal read_string "exit") (return ))
    )
)
 
(GPPINTERPRETER)
