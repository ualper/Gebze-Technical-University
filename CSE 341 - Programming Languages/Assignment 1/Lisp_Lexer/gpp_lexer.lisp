;-------------------------------------------
; CHECKS THE OPENING OR CLOSING QUOTE
;-------------------------------------------
(defvar *WHICH-COMMENT* 0)
;-------------------------------------------
(DEFUN INTERPRETER-GPP (&OPTIONAL FILENAME)
	(LET ((OUT (open "parsed_lisp.txt" :direction :output)))
		(IF FILENAME (INTERPRETER-FILE FILENAME OUT) (INTERPRETER-SHELL OUT)) (close OUT)))
;-------------------------------------------
(DEFUN INTERPRETER-SHELL (OUT)
	(loop for line = (READ-LINE) WHILE (NOT (string= line "")) do (INTERPRETER line OUT)))
;-------------------------------------------
(DEFUN INTERPRETER-FILE (FILENAME OUT)
	(LET ((in (open FILENAME :IF-does-NOT-exist nil)))	
		(WHEN in (loop for line = (READ-LINE in nil) WHILE line do (INTERPRETER line OUT)) (close in)) (unless in (format t "Error! No such file: '~a'~%" FILENAME))))
;-------------------------------------------
(DEFUN INTERPRETER (STR OUT)
	(LET ((lst (STR-TO-LIST STR))) (map nil #'(lambda (TOKEN) (write-line (TOKENIZE TOKEN (GET-TOKENS)) OUT)) lst)))
;-------------------------------------------
(DEFUN GET-TOKENS ()
	(concatenate 'LIST (GET-KEYWORDS) (GET-OP)))
;-------------------------------------------
(DEFUN GET-KEYWORDS ()
	(pairlis '("false" "true" "disp"  "load" "exit" "IF" "for" "deffun" "set" "concat" "append" "LIST" "nil" "less" "equal" "NOT" "or" "and") '("KW_FALSE" "KW_TRUE" "KW_DISP" "KW_LOAD" "KW_EXIT" "KW_IF" "KW_FOR" "KW_DEFFUN" "KW_SET" "KW_CONCAT" "KW_APPEND" "KW_LIST" "KW_NIL" "KW_LESS" "KW_EQUAL" "KW_NOT" "KW_OR" "KW_AND")))
;-------------------------------------------
(DEFUN GET-OP ()
	(pairlis '("," "\"" "\"" "**" ")" "(" "*" "/" "-" "+") '("OP_COMMA" "OP_CC" "OP_OC" "OP_DBLMULT" "OP_CP" "OP_OP" "OP_MULT" "OP_DIV" "OP_MINUS" "OP_PLUS")))
;-------------------------------------------
(DEFUN TOKENIZE (TOKEN LIST-TOKEN)
	(LET ((value))
		(IF (or
			(SETF value (TOKENIZE-KEY-OP TOKEN LIST-TOKEN))
			(SETF value (TOKENIZE-IDENTIFIER TOKEN LIST-TOKEN))
			(SETF value (TOKENIZE-VALUE TOKEN))
			(SETF value (TOKENIZE-COMMENT TOKEN)))
		value (format nil "SYNTAX ERROR ~a cannot be tokenized" TOKEN))))
;-------------------------------------------
(DEFUN TOKENIZE-KEY-OP (TOKEN keys)
	(LET ((var (assoc TOKEN keys :test #'string=)))
		(IF (string= (CAR var) "\"")
			(IF (ZEROP *WHICH-COMMENT*) (progn (SETF *WHICH-COMMENT* 1) "OP_OC") (progn (SETF *WHICH-COMMENT* 0) "OP_CC"))
			(CDR var))))
;------------------------------------------->> [[:alpha:]][[:alnum:]]*
(DEFUN TOKENIZE-IDENTIFIER (TOKEN key) 
	(IF (IS-ALPHA (CHAR TOKEN 0))
		(loop for i across TOKEN do (WHEN (NOT (IS-ALNUM i)) (RETURN-FROM TOKENIZE-IDENTIFIER)))
		(RETURN-FROM TOKENIZE-IDENTIFIER)) "IDENTIFIER")
;------------------------------------------->> ("+"?|"-"?)([[:digit:]]|[1-9][[:digit:]]+)
(DEFUN TOKENIZE-VALUE (TOKEN) 
	(WHEN (and (CHAR= (CHAR TOKEN 0) #\0) (NOT (= (LENGTH TOKEN) 1))) (RETURN-FROM TOKENIZE-VALUE))

	(loop for i across TOKEN do (WHEN (NOT (IS-DIGIT i)) (RETURN-FROM TOKENIZE-VALUE)))
	"VALUE")
;-------------------------------------------
(DEFUN TOKENIZE-COMMENT (TOKEN) ; ";;".*
	(WHEN (and (> (LENGTH TOKEN) 1) (IS-SEMICOLON (CHAR TOKEN 0)) (IS-SEMICOLON (CHAR TOKEN 1))) "COMMENT"))
;-------------------------------------------
(DEFUN STR-TO-LIST (STR)
	(WHEN (IS-NOT-ZERO STR)
		(LET ((founded (FPOS-FOR-DELIM STR)) (fchr) (POS) (posQuote))
			(unless (NULL founded) (SETF fchr (CAR founded)) (SETF POS (CDR founded)))

			(cond 
				((NULL founded) (LIST STR))
				((IS-BRACKET fchr)
					(IF (ZEROP POS)
						(CONS (SUBSEQ STR 0 1) (STR-TO-LIST (SUBSEQ STR 1)))
						(CONS (SUBSEQ STR 0 POS) (CONS (SUBSEQ STR POS (+ POS 1)) (STR-TO-LIST (SUBSEQ STR (+ POS 1)))))))
				((IS-SPACE fchr)
					(IF (ZEROP POS)
						(STR-TO-LIST (SUBSEQ STR 1))
						(CONS (SUBSEQ STR 0 POS) (STR-TO-LIST (SUBSEQ STR (+ POS 1))))))
				((IS-QUOTE fchr)
					(IF (ZEROP POS)
						(CONS (SUBSEQ STR 0 1) (STR-TO-LIST (SUBSEQ STR 1)))
						(CONS (SUBSEQ STR 0 POS) (CONS (SUBSEQ STR POS (+ POS 1)) (STR-TO-LIST (SUBSEQ STR (+ POS 1)))))))
				(t (IF (or (<= (LENGTH STR) (+ POS 1)) (NOT (IS-SEMICOLON (CHAR STR (+ POS 1)))))
						(IF (ZEROP POS)
							(CONS (SUBSEQ STR 0 1) (STR-TO-LIST (SUBSEQ STR 1)))
							(CONS (SUBSEQ STR 0 POS) (CONS (SUBSEQ STR POS (+ POS 1)) (STR-TO-LIST (SUBSEQ STR (+ POS 1))))))
						(IF (ZEROP POS) (LIST STR) (LIST (SUBSEQ STR 0 POS) (SUBSEQ STR POS)))))))))
;-------------------------------------------
(DEFUN FPOS-FOR-DELIM (STR &OPTIONAL (POS 0))
	(WHEN (IS-NOT-ZERO STR)
		(IF (or (IS-BRACKET (CHAR STR 0)) (IS-SPACE (CHAR STR 0)) (IS-SEMICOLON (CHAR STR 0)) (IS-QUOTE (CHAR STR 0)))
			(CONS (CHAR STR 0) POS)
			(FPOS-FOR-DELIM (SUBSEQ STR 1) (+ POS 1)))))
;-------------------------------------------
(DEFUN FPOS-FOR-QUOTE (STR &OPTIONAL (POS 0))
	(WHEN (IS-NOT-ZERO STR) (IF (IS-QUOTE (CHAR STR 0)) POS (FPOS-FOR-QUOTE (SUBSEQ STR 1) (+ POS 1)))))
;-------------------------------------------
(DEFUN IS-ALNUM (CHR)
	(or (and (CHAR>= CHR #\0) (CHAR<= CHR #\9)) (and (CHAR>= CHR #\A) (CHAR<= CHR #\Z)) (and (CHAR>= CHR #\a) (CHAR<= CHR #\z))))
;-------------------------------------------
(DEFUN IS-ALPHA (CHR)
	(or (and (CHAR>= CHR #\A) (CHAR<= CHR #\Z)) (and (CHAR>= CHR #\a) (CHAR<= CHR #\z))))
;-------------------------------------------
(DEFUN IS-DIGIT (CHR)
	(and (CHAR>= CHR #\0) (CHAR<= CHR #\9)))
;-------------------------------------------
(DEFUN IS-SPACE (CHR)
	(or (and (CHAR>= CHR (code-CHAR 9)) (CHAR<= CHR (code-CHAR 13))) (CHAR= CHR (code-CHAR 32))))
;-------------------------------------------
(DEFUN IS-BRACKET (CHR)
	(or (CHAR= CHR #\() (CHAR= CHR #\))))
;-------------------------------------------
(DEFUN IS-QUOTE (CHR)
	(CHAR= CHR #\"))
;-------------------------------------------
(DEFUN IS-SEMICOLON (CHR)
	(CHAR= CHR #\;))
;-------------------------------------------
(DEFUN IS-NOT-ZERO (STR)
	(NOT (ZEROP (LENGTH STR))))
;-------------------------------------------
(IF *ARGS* (INTERPRETER-GPP (CAR *ARGS*)) (INTERPRETER-GPP))
;-------------------------------------------