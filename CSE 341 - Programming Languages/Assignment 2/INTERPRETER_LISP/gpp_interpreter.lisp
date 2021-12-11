;;; =================================================
;;; =======     UMUT AY ALPER - 1801042097    =======
;;; =================================================

;; ---------------------------------------------------------------------------------------

;; ==================================================
;;  THE LEXER CODE
;; ==================================================

(setq KEYWORDS_LIST '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while"))
(setq KEYWORDS_TOKEN_LIST '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT"
                            "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_WHILE"))

(setq OPERATORS_LIST '("+" "-" "/" "*" "(" ")" "**" """" ","))
(setq OPERATORS_TOKEN_LIST '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))

(setq TOKENS_LIST '())

(defun READFILE (FILENAME)
  (with-open-file (stream FILENAME)
    (loop for LINE = (read-LINE stream nil)
          while LINE
          	collect LINE
    )
  )
)

(defun ISNUMBER (NUM)
    (and (<= (char-code NUM) 57) (>= (char-code NUM) 48)) 
)

(defun ISCHARACTER (CHR)
    (and (<= (char-code CHR) 122) (>= (char-code CHR) 65))
)

(defun ISWHITESPACE (CHR)
    (= (char-code CHR) 32)
)

(defun LIST-SEARCH (STR LLIST)
    (dotimes (index (length LLIST))
        (if (equal (nth index LLIST) STR)
            (return-from LIST-SEARCH index)
        )
    )
    nil
)

(defun LIST-TRIM (list)
	(setq newList '())
	(dolist (LINE list)
		(setq resultString "")
		(loop for CHR across LINE
			do 
				(cond
					((equal (string CHR) "(") (setq resultString (concatenate 'string resultString (string "( "))))
					((equal (string CHR) ")") (setq resultString (concatenate 'string resultString (string " )"))))
					(t (setq resultString (concatenate 'string resultString (string CHR))))
				)
		)
		(setq newList (append newList (list resultString)))
	)
	newList
)

(defun CHECK-ID (STR)
	(setq CHR-number 0)
	(setq NUM-number 0)
	(setq ifisFirst 0)
	(setq syntax_control nil)

	(loop for c across STR do
		(if (equal ifisFirst 0)
			(if (ISNUMBER c) ;; such as 123AD is not identifier, chechs its ...
				(progn 
					(setq syntax_control T)
					(setq ifisFirst 1)
				)
				(setq ifisFirst 1)
			)
		)
		(if (ISCHARACTER c)
			(setq CHR-number (+ CHR-number 1))
		)
		(if (ISNUMBER c)
			(setq NUM-number (+ NUM-number 1))
		)
	)
	(if (equal (length STR) CHR-number)
		(return-from CHECK-ID "IDENTIFIER")
		;; token is IDENTIFIER, because consists of characters ...
	)
	(if (equal (length STR) NUM-number)
		(return-from CHECK-ID "VALUE")
		;; token is VALUE, because consists of numbers ...
	)
	(if (equal syntax_control T)
		(progn 
			(return-from CHECK-ID (ERROR-MSG STR))
			;; concreate syntax_error message ...
		)
		(return-from CHECK-ID "IDENTIFIER")
	)
)

(defun DETERMINE-TOKENS (LINE) 
	(setq controlComma 0)
	(setq controlSpace 0)
	(setq tokens '())
	(setq word "")
	(setq integers "")
	(loop for CHR across LINE do ;; CHR is character ...
		
		(cond 
			( (equal CHR #\;) ;; Control comment using controlComma variable ...
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("COMMENT"))))
						(return-from DETERMINE-TOKENS tokens)
					)
				)
			)

			((equal CHR #\*) ;; Control DBL_MLT operator .... like using controlcomma ....	
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("OP_DBLMULT" "**"))))
					)
				)

			)

			( (not (eq (LIST-SEARCH (string CHR) OPERATORS_LIST) nil))
				;; Control operators ...
				(setq index (LIST-SEARCH (string CHR) OPERATORS_LIST))
				(setq tokens (append tokens (list (list (nth index OPERATORS_TOKEN_LIST) (string CHR)))))
			)

			( (not (equal (ISCHARACTER CHR) nil))
				;; WORD STRING'I GUNCELLENECEK ...
				(setf word (concatenate 'string word (string CHR)))
			)

			( (not (equal (ISNUMBER CHR) nil))
				;; INTEGER STRINGI GUNCELLENECEK
				(setf word (concatenate 'string word (string CHR)))
				(setf integers (concatenate 'string integers (string CHR)))
			)

			( (equal (ISWHITESPACE CHR) t)
				;; If the character is space, control the  word string and determined its token ...
				(setq controlSpace 1)
				(if (not (equal word ""))
					(progn 
						(if (not (equal (LIST-SEARCH word KEYWORDS_LIST) nil))
							(progn 
								(setq index (LIST-SEARCH word KEYWORDS_LIST)) ;; If is keyword ...
								(setq tokens (append tokens (list (list (nth index KEYWORDS_TOKEN_LIST) word))))
								(setq word "")	
							)
							(progn ;; Determine if the word string is identifier, value or sytanx_error using helper function such as CHECK-ID ...
								;(setq tokens (append tokens (list (CHECK-ID word))))
								(setq tokens (append tokens (list (list (CHECK-ID word) word))))
								(setq word "")
							)
						)
					)
				)							
			)
			;; An error occured, because the program does not syntax analysis correctly ...
			(t (setq tokens (append tokens (list (list (ERROR-MSG CHR) (string CHR))))))
		)
	)
	(return-from DETERMINE-TOKENS tokens)
)

(defun ERROR-MSG (STR-CHR)
	(setq error "SYNTAX__ERROR ")
	(setq error2 " cannot be tokenized")
	(setq error (concatenate 'string error (string STR-CHR)))
	(setq error (concatenate 'string error error2))
	error
)

(defun LEXER (LLIST)
	(dolist (LINE LLIST)
		(setq TOKENS_LIST (append TOKENS_LIST (DETERMINE-TOKENS LINE)))
	)
	TOKENS_LIST
)

(defun PRINT-TOKENS (LLIST)
	(terpri)
	(dolist (LINE LLIST)
		(format t "~a~%" LINE)
	)
	(terpri)
)

(defun GPP-LEXER (FILE-NAME)
	(setq TOKENS_LIST '())
    (setq string-list '())
    (setq new-string-list '())

    ;(setq FILE-NAME (READFILE FILENAME))
	(setq file-list '())
	(setq file-list (append file-list (list FILE-NAME)))
	
	(setf new-string-list (LIST-TRIM file-list))
	(LEXER new-string-list)
	(return-from GPP-LEXER TOKENS_LIST)
)

;; ---------------------------------------------------------------------------------------

;;; =================================================
;;  THE PARSER CODE
;; ==================================================


(defun CHECK-TOKENS(LLIST)
    (setq OPcount 0)
    (setq CPcount 0)
    (setq DEFUN-FLAG 0)
    (dolist (SUBLIST LLIST)
        (if (equal (nth 0 SUBLIST) "OP_OP")
            (setq OPcount (+ OPcount 1))
        )
        (if (equal (nth 0 SUBLIST) "OP_CP")
            (setq CPcount (+ CPcount 1))
        )
        (if (not (equal (STR-CONCAT "SYNTAX__ERROR" (nth 0 SUBLIST)) nil))
            (progn 
                (if (not (equal (nth 1 SUBLIST) "'"))
                    (return-from CHECK-TOKENS nil)
                )
            )
        )
        (if (equal (nth 0 SUBLIST) "KW_DEFFUN")
            (setq DEFUN-FLAG 1)
        )
    )

    (if (not (equal OPcount CPcount))
        (return-from CHECK-TOKENS nil)
        (progn 
            (if (equal DEFUN-FLAG 1)
                (return-from CHECK-TOKENS "IS-DEFUN")
                (return-from CHECK-TOKENS T)
            )
        )
    )
)


(defun REMOVE-LAST (list)
    (loop for LL on list
        while (rest LL)
        collect (first LL)
    )
)

(defun LIST-REVERSE (LL)
    (cond
        ((null LL) '())
        (T (append (LIST-REVERSE (cdr LL)) (list (car LL))))
    )
) 

(defun LIST-CONCATENATE (SEQ1 SEQ2)
    (cond ((not (null SEQ1)) (cons (car SEQ1) (LIST-CONCATENATE (cdr SEQ1) SEQ2)))
          (T (cond ((not (null SEQ2)) (cons (car SEQ2) (LIST-CONCATENATE SEQ1 (cdr SEQ2))))
                   (T nil))))
)

(defun STR-CONCAT (STR1 STR2)
    (cond
        ((zerop (length STR1)) nil) 
        ((> (length STR1) (length STR2)) nil) 
        ((string= STR1 (subseq STR2 0 (length STR1))) STR1) 
        (t (STR-CONCAT STR1 (subseq STR2 1)))
    )
) 

(defun DBMULTI(NUM1 NUM2)
    (setq result 1)
    (setq control 0)
    (loop 
        (setq result (* result NUM1))
        (setq control (+ control 1))
        (when (equal control NUM2) (return-from DBMULTI result))
    )
)

(defun CHECK-INTEGER(EXPR)
    (setq controlValue 0)
    (dolist (element EXPR) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (return-from CHECK-INTEGER nil)
                )
            )
        )
        (setq controlValue 1)
    )
    T
)

(defun CHECK-BINARY(EXPR)
    (setq controlValue 0)
    (dolist (element EXPR) 
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

(defun CHANGE-BINARY(BOOL-BINARY)
    (if (or (equal BOOL-BINARY "true")  (equal BOOL-BINARY"\"true\""))
        T
        nil
    )
)

(defun IS-PLUS(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (+ (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR)))
        nil
    )
)

(defun IS-MINUS(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (- (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR)))
        nil
    )
)

(defun IS-DIVIDE(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (/ (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR)))
        nil
    )
)

(defun IS-MULT(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (* (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR)))
        nil
    )
)

(defun IS-DOUBLE-MULT(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (return-from IS-DOUBLE-MULT (DBMULTI (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR))))
        nil
    )
)

(defun IS-AND(EXPR)
    (if (equal (not (CHECK-BINARY EXPR)) nil)
        (progn 
            (setq resultB (and (CHANGE-BINARY (nth 1 EXPR)) (CHANGE-BINARY (nth 2 EXPR))))
            (if (equal resultB T)
                (return-from IS-AND "true")
                (return-from IS-AND "false")
            )
        )
        nil
    )
)

(defun IS-OR(EXPR)
    (if (equal (not (CHECK-BINARY EXPR)) nil)
        (progn 
            (setq resultB (or (CHANGE-BINARY (nth 1 EXPR)) (CHANGE-BINARY (nth 2 EXPR))))
            (if (equal resultB T)
                (return-from IS-OR "true")
                (return-from IS-OR "false")
            )
        )
        nil
    )
)

(defun IS-NOT(EXPR)
    (if (equal (not (CHECK-BINARY EXPR)) nil)
        (progn 
            (setq resultB (not (CHANGE-BINARY (nth 1 EXPR))))
            (if (equal resultB T)
                (return-from IS-NOT "true")
                (return-from IS-NOT "false")
            )
        )
        nil
    )
)

(defun IS-EQUAL(EXPR)
    (if (equal (not (CHECK-BINARY EXPR)) nil)
        (progn 
            (setq resultB (equal (CHANGE-BINARY (nth 1 EXPR)) (CHANGE-BINARY (nth 2 EXPR))))
            (if (equal resultB T)
                (return-from IS-EQUAL "true")
                (return-from IS-EQUAL "false")
            )
        )
        (progn 
            (if (equal (not (CHECK-INTEGER EXPR)) nil)
                (progn 
                    (setq resultB (equal (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR))))
                    (if (equal resultB T)
                        (return-from IS-EQUAL "true")
                        (return-from IS-EQUAL "false")
                    )
                )
                nil
            )
        )
    )
)

(defun IS-LESS(EXPR)
    (if (equal (not (CHECK-INTEGER EXPR)) nil)
        (progn 
            (setq resultB (< (parse-integer (nth 1 EXPR)) (parse-integer (nth 2 EXPR))))
            (if (equal resultB T)
                (return-from IS-LESS "true")
                (return-from IS-LESS "false")
            )
        )
        nil
    )
)

(defun IS-SET(EXPR) 
    (if (equal (parse-integer (nth 2 EXPR) :junk-allowed t) nil)
        (progn 
            (if (equal (type-of (nth 2 EXPR)) 'CONS)
                (return-from IS-SET (nth 2 EXPR))
                (return-from IS-SET nil)
            )
        )
        (return-from IS-SET (parse-integer (nth 2 EXPR)))
    )
)

(defun IS-LIST(EXPR)
    (setq controlValue 0)
    (setq returnList '())
    (dolist (element EXPR) 
        (if (equal controlValue 1)
            (progn 
                (if (equal (parse-integer element :junk-allowed t) nil)
                    (progn 
                        (setq liste1 (read-from-string element))
                        (if (equal (type-of liste1) 'CONS)
                            (setq returnList (append returnList (list liste1)))
                            (return-from IS-LIST nil)
                        )
                    )
                    (setq returnList (append returnList (list (parse-integer element)))) 
                )
            )
        )
        (setq controlValue 1)
    )
    (return-from IS-LIST returnList)
)

(defun IS-APPEND(EXPR) 
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 EXPR)))
    (setq liste2 (read-from-string (nth 2 EXPR)))
    (if (equal (type-of liste2) 'CONS)
        (progn 
            (if (equal (parse-integer (nth 1 EXPR) :junk-allowed t) nil)
                (progn 
                    (if (equal (type-of liste1) 'CONS)
                        (progn 
                            (setq liste2 (append liste1 liste2))
                            (return-from IS-APPEND liste2) 
                        )
                    )
                    nil
                )
                (progn 
                    (setq liste2 (append liste2 (parse-integer (nth 1 EXPR))))
                    (return-from IS-APPEND liste2)
                )
            )
        )
        nil
    )
)

(defun IS-CONCATENATE(EXPR)
    (setq liste1 '())
    (setq liste2 '())
    (setq liste1 (read-from-string (nth 1 EXPR)))
    (setq liste2 (read-from-string (nth 2 EXPR)))   
    (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
        (return-from IS-CONCATENATE (LIST-CONCATENATE liste1 liste2))
        nil
    )
)

(defun IS-IF(EXPR)
    (setq EX_BI (nth 1 EXPR))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) nil)
        (return-from CHECK-BINARY nil)
        (progn 
            (cond 
                ((equal (length EXPR) 3)
                    (progn 
                        (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                            (return-from IS-IF "false")
                            (progn 
                                (setq liste2 '())
                                (setq liste2 (read-from-string (nth 2 EXPR)))
                                (if (equal (type-of liste2) 'CONS)
                                    (return-from IS-IF liste2)
                                    nil
                                )     
                            )
                        )                    
                    )
                )
                ((equal (length EXPR) 4)
                    (progn 
                        (setq liste1 '())
                        (setq liste2 '())
                        (setq liste1 (read-from-string (nth 2 EXPR)))
                        (setq liste2 (read-from-string (nth 3 EXPR)))   
                        (if (and (equal (type-of liste1) 'CONS) (equal (type-of liste2) 'CONS))
                            (progn 
                                (if (or (equal EX_BI "true") (equal EX_BI "\"true\""))
                                    (return-from IS-IF liste1)
                                    (return-from IS-IF liste2)
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

(defun IS-WHILE(EXPR) 
    (setq EX_BI (nth 1 EXPR))
    (if (equal (or (equal EX_BI "true") (equal EX_BI "false") (equal EX_BI "\"true\"") (equal EX_BI "\"false\"")) T)
        (progn 
            (if (or (equal EX_BI "false") (equal EX_BI "\"false\""))
                (return-from IS-WHILE "false")
                (progn 
                    (setq liste2 '())
                    (setq liste2 (read-from-string (nth 2 EXPR)))
                    (if (equal (type-of liste2) 'CONS)
                        (return-from IS-WHILE liste2)
                        nil
                    )     
                )
            )
        )
    )
)

(defun IS-LOAD(EXPR)
    (if (type-of (nth 1 EXPR) 'simple-array)
        (return-from IS-LOAD (nth 1 EXPR))
        nil
    )
)

(defun IS-DEFUN(EXPR)
    (format t "SYNTAX OK. ~%")
)

(defun CHECK(EXPR) 
    (cond 
        ( (not (equal (LIST-SEARCH (nth 0 EXPR) OPERATORS_LIST) nil))
            ;; operator kontrolü ...
            (progn 
                (cond 
                    ( (equal (nth 0 EXPR) "+") ;; + operatoru ise
                        (if (equal (length EXPR) 3)
                            (progn 
                                (setq result (IS-PLUS EXPR))
                                (return-from CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 EXPR) "-") ;; - operatoru ise
                        (if (equal (length EXPR) 3)
                            (progn 
                                (setq result (IS-MINUS EXPR))
                                (return-from CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 EXPR) "/") ;; / operatoru ise
                        (if (equal (length EXPR) 3)
                            (progn 
                                (setq result (IS-DIVIDE EXPR))
                                (return-from CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 EXPR) "*") ;; "*" operatoru ise
                        (if (equal (length EXPR) 3)
                            (progn 
                                (setq result (IS-MULT EXPR))
                                (return-from CHECK result)
                            )
                            nil
                        )
                    )
                    ( (equal (nth 0 EXPR) "**") ;; "**"  operatoru ise
                        (if (equal (length EXPR) 3)
                            (progn 
                                (setq result (IS-DOUBLE-MULT EXPR))
                                (return-from CHECK result)
                            )
                            nil
                        )
                    )
                )
            )
        )
        ( (not (equal (LIST-SEARCH (nth 0 EXPR) KEYWORDS_LIST) nil))
            ;; keyword kontrolü ...
            (cond 
                ( (equal (nth 0 EXPR) "and") ;; and keywordu ise
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-AND EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "or") ;; or keywordu ise
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-OR EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "not") ;; not keywordu ise
                    (if (equal (length EXPR) 2)
                        (progn 
                            (setq result (IS-NOT EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "equal") ;; equal keywordu ise
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-EQUAL EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "less") ;; less keywordu ise
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-LESS EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "set") ;; set keywordu ise
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-SET EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "list") ;; set keywordu ise
                    (progn 
                        (setq result "")
                        (if (equal (length EXPR) 1) 
                            (setq result "NIL")
                            (setq result (IS-LIST EXPR))
                        )
                        (return-from CHECK result)
                    )  
                )
                ( (equal (nth 0 EXPR) "append") ;; append keywordu ise
                    (progn 
                        (setq result (IS-APPEND EXPR))
                        (return-from CHECK result)
                    )  
                )
                ( (equal (nth 0 EXPR) "concat") ;; concat keywordu ise 
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-CONCATENATE EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "if") ;; if keywordu ise 
                    (progn 
                        (setq result (IS-IF EXPR))
                        (return-from CHECK result) 
                    )
                    nil
                )
                ( (equal (nth 0 EXPR) "while") ;; while keywordu ise 
                    (if (equal (length EXPR) 3)
                        (progn 
                            (setq result (IS-WHILE EXPR))
                            (return-from CHECK result) 
                        )
                        nil
                    )
                )
                ( (equal (nth 0 EXPR) "deffun") ;; deffun keywordu ise 
                    (return-from CHECK "SYNTAX OK.")     
                )
                ( (equal (nth 0 EXPR) "load") ;; load keywordu ise 
                    (if (equal (length EXPR) 2)
                        (progn 
                            (setq result (IS-LOAD EXPR))
                            (return-from CHECK result)   
                        )
                        nil
                    ) 
                )
            )
        )
        (t nil)
    )
)

(defun PARSER(string_)
    ;; Stackler string tutacaklar ...
    (setq stack1 '())
    (setq stack2 '())
    (setq word "")
    (setq controlFirst T)
    (setq listControl nil)
    (setq previousCH #\1)

    (loop for CHR across string_ do 
        
        ;; (format t "~%Stack 1 : ~a~%" stack1)
        ;; (format t "Stack 2 : ~a~%" stack2)
        (if (equal controlFirst T)
            (progn 
                (if (equal (or (equal CHR (code-char 39.)) (equal CHR (code-char 40))) T)
                    (progn 
                        (if (equal CHR (code-char 39.)) 
                            (setq listControl T)
                        )
                        (setq controlFirst nil)
                    )
                    (progn 
                        (format t "~%SYNTAX_ERROR EXPR not recognized~%")
                        (return-from PARSER nil)
                    )
                )
            )
            (progn
                (cond 
                    ((equal CHR (code-char 41.)) ;; Kapalı parantez kontrolü
                        (progn 
                            (setq rev-stack1 (LIST-REVERSE stack1))
                            (dolist (element rev-stack1) ;; Acık parantez bulana kadar git ...
                                (if (equal element (string (code-char 40.)))
                                    (progn 
                                        (setq stack1 (REMOVE-LAST stack1)) 
                                        (return)
                                    )
                                    (progn 
                                        (setq stack2 (append stack2 (list element)))
                                        (setq stack1 (REMOVE-LAST stack1)) 
                                    ) 
                                )
                            )
                            (setq stack2 (LIST-REVERSE stack2))  ;; Listenin tersini almak lazım yoksa problem cikarmaktadir ...
                            (setq control (CHECK stack2))
                            (if (equal control nil)
                                (progn 
                                    (format t "~%SYNTAX_ERROR EXPR not recognized~%")
                                    (return-from PARSER nil)
                                )
                                (progn 
                                    (setq stack2 '())
                                    (setq stack1 (append stack1 (list (write-to-string control))))
                                )                    
                            )
                        )
                    )

                    ((equal CHR #\space) ;; Bosluk kontrolu ...
                        (progn 
                            (if (not (equal word ""))
                                (progn 
                                    (setq stack1 (append stack1 (list word))) ;; Bosluksa stack1'e push et ...
                                    (setq word "")
                                )
                            )
                        )
                    )
                    
                    ((equal listControl T) ;; ilk basta '() ile LLIST olusturabildiginden onu kontrol eder ...
                        (progn 
                            (if (equal CHR (code-char 40.))
                                (progn 
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq listControl nil)
                                )
                            )
                        )   
                    )

                    ((not (equal CHR #\;))    ; Yorum satırını anlamak icin yazılmıştır. Yorum satırı ise program bir şey yapmayacak.
                        (progn 
                            (if (and (equal previousCH #\') (equal CHR (code-char 40.)))
                                (progn ;; '( gelirse LLIST oldugunu anlaması icin
                                    (setq stack1 (append stack1 (list "(")))
                                    (setq stack1 (append stack1 (list "list")))
                                    (setq word "")
                                )
                                (setf word (concatenate 'string word (string CHR)))
                            )
                        )   
                    )
                )
            )
        )
        (setq previousCH CHR)
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

(defun GPP-INTERPRETER() 

    (format t " ==============  GPP INTERPRETER ============== ~%")
    (setq number 1)
    (loop 
        (terpri)
        (format t "[~d]> " number)
        (setq read_string (string-downcase (string (read-LINE))))
        (if (equal (STR-CONCAT "g++" read_string) nil)
            (progn 
                (if (not (equal read_string "exit"))
                    (progn 
                        (setq controlLOAD 0)
                        (setq number (+ number 1))
                        (setq tokens '())
                        (setq list_ '())
                        (setq list_ (append list_ (list read_string)))
                        (setq read_string (nth 0 (LIST-TRIM list_)))
                        
                        (if (not (equal (STR-CONCAT "( load" read_string) nil))
                            (progn
                                (format t "~%~a~%" (subseq read_string 6 (- (length read_string) 1)))
                                (setq controlLOAD 1)
                            )
                        )
                        (if (equal controlLOAD 0)
                            (progn 
                                (setq tokens (GPP-LEXER read_string))
                                (if (not (equal (CHECK-TOKENS tokens) nil))
                                    (progn 
                                        (if (equal (CHECK-TOKENS tokens) "IS-DEFUN")
                                            (IS-DEFUN tokens)
                                            (PARSER read_string)
                                        )
                                    )
                                    (format t "~%SYNTAX_ERROR EXPR not recognized~%")
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
                        (loop for LINE = (read-LINE in nil)
                            while LINE do 
                            (progn 
                                (setq line2 LINE)
                                (setq list_ '())
                                (setq list_ (append list_ (list LINE)))
                                (setq read_string (nth 0 (LIST-TRIM list_)))
                                (PARSER read_string)
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
 
(GPP-INTERPRETER)
