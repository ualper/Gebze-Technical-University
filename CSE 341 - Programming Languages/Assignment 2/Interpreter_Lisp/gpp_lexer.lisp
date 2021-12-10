;;; =================================================
;;; =======     UMUT AY ALPER - 1801042097    =======
;;; =================================================

(setq LIST_TOKENS '())
(setq KEYWORDS '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "while"))
(setq KEYWORDS_TOKEN '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT"
                            "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE" "KW_WHILE"))

(setq OPERATORS'("+" "-" "/" "*" "(" ")" "**" """" ","))
(setq OPERATORS_TOKEN '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_OC" "OP_CC" "OP_COMMA"))

(defun READFILE (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          	collect line
    )
  )
)
(defun ISCHARACTER(ch)
    (and (<= (char-code ch) 122) (>= (char-code ch) 65))
)
(defun ISNUMBER(nb)
    (and (<= (char-code nb) 57) (>= (char-code nb) 48)) 
)
(defun ISSPACE (ch)
    (= (char-code ch) 32)
)
(defun SEARCH-IN-LIST (str liste)
    (dotimes (index (length liste))
        (if (equal (nth index liste) str)
            (return-from SEARCH-IN-LIST index)
        )
    )
    nil
)
(defun LIST-TRIMM (list)
	(setq newList '())
	(dolist (line list)
		(setq resultString "")
		(loop for ch across line
			do 
				(cond
					((equal (string ch) "(") (setq resultString (concatenate 'string resultString (string "( "))))
					((equal (string ch) ")") (setq resultString (concatenate 'string resultString (string " )"))))
					(t (setq resultString (concatenate 'string resultString (string ch))))
				)
		)
		(setq newList (append newList (list resultString)))
	)
	newList
)
(defun ERROR-MSG (strORch)
	(setq error "SYNTAX__ERROR ")
	(setq error2 " cannot be tokenized")
	(setq error (concatenate 'string error (string strORch)))
	(setq error (concatenate 'string error error2))
	error
)
(defun CHECK-ID (str)
	(setq ch-number 0)
	(setq nb-number 0)
	(setq ifisFirst 0)
	(setq syntax_control nil)

	(loop for c across str do
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
			(setq ch-number (+ ch-number 1))
		)
		(if (ISNUMBER c)
			(setq nb-number (+ nb-number 1))
		)
	)
	(if (equal (length str) ch-number)
		(return-from CHECK-ID "IDENTIFIER")
		;; token is IDENTIFIER, because consists of characters ...
	)
	(if (equal (length str) nb-number)
		(return-from CHECK-ID "VALUE")
		;; token is VALUE, because consists of numbers ...
	)
	(if (equal syntax_control T)
		(progn 
			(return-from CHECK-ID (ERROR-MSG str))
			;; concreate syntax_error message ...
		)
		(return-from CHECK-ID "IDENTIFIER")
	)
)
(defun ASSIGN-TOKEN-NAME (line) 
	(setq controlComma 0)
	(setq controlSpace 0)
	(setq tokens '())
	(setq word "")
	(setq integers "")
	(loop for ch across line do ;; ch is character ...
		
		(cond 
			( (equal ch #\;) ;; Control comment using controlComma variable ...
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("COMMENT"))))
						(return-from ASSIGN-TOKEN-NAME tokens)
					)
				)
			)

			((equal ch #\*) ;; Control DBL_MLT operator .... like using controlcomma ....	
				(if (equal controlComma 0)
					(setf controlComma 1)
					(progn 
						(setf controlComma 0)
						(setq tokens (append tokens (list '("OP_DBLMULT" "**"))))
					)
				)

			)

			( (not (eq (SEARCH-IN-LIST (string ch) OPERATORS_LIST) nil))
				;; Control operators ...
				(setq index (SEARCH-IN-LIST (string ch) OPERATORS_LIST))
				(setq tokens (append tokens (list (list (nth index OPERATORS_TOKEN) (string ch)))))
			)

			( (not (equal (ISCHARACTER ch) nil))
				;; WORD STRING'I GUNCELLENECEK ...
				(setf word (concatenate 'string word (string ch)))
			)

			( (not (equal (ISNUMBER ch) nil))
				;; INTEGER STRINGI GUNCELLENECEK
				(setf word (concatenate 'string word (string ch)))
				(setf integers (concatenate 'string integers (string ch)))
			)

			( (equal (ISSPACE ch) t)
				;; If the character is space, control the  word string and determined its token ...
				(setq controlSpace 1)
				(if (not (equal word ""))
					(progn 
						(if (not (equal (SEARCH-IN-LIST word KEYWORDS) nil))
							(progn 
								(setq index (SEARCH-IN-LIST word KEYWORDS)) ;; If is keyword ...
								(setq tokens (append tokens (list (list (nth index KEYWORDS_TOKEN) word))))
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
			(t (setq tokens (append tokens (list (list (ERROR-MSG ch) (string ch))))))
		)
	)
	(return-from ASSIGN-TOKEN-NAME tokens)
)
(defun LEXER-HELPER (liste)
	(dolist (line liste)
		(setq LIST_TOKENS (append LIST_TOKENS (ASSIGN-TOKEN-NAME line)))
	)
	LIST_TOKENS
)
(defun TOKEN-TOSTRING (liste)
	(terpri)
	(dolist (line liste)
		(format t "~a~%" line)
	)
	(terpri)
)
(defun GPPLEXER (file_string)
	(setq LIST_TOKENS '())
    (setq string-list '())
    (setq new-string-list '())

    (setq file_string (READFILE filename))
	(setq file-list '())
	(setq file-list (append file-list (list file_string)))
	
	(setf new-string-list (LIST-TRIMM file-list))
	(LEXER-HELPER new-string-list)
	(return-from GPPLEXER LIST_TOKENS)
)