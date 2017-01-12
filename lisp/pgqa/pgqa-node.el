;; Copyright (C) 2016 Antonin Houska
;;
;; This file is part of PGQA.
;;
;; PGQA is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; PGQA is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License qalong
;; with PGQA. If not, see <http://www.gnu.org/licenses/>.

(require 'eieio)

(defclass pgqa-node ()
  (
   (region
    :initarg :region
    :documentation "Start and end position of a nonterminal."
    )

   (markers
    :initarg :markers
    :documentation "Start and end position in the form of a marker."
    )
   )

  "Class representing a generic node of SQL query tree.")

(defclass pgqa-expr (pgqa-node)
  (
   (args :initarg :args)
   )
  "A node representing an operation on one or multiple arguments.")

;; TODO Recursion.
(defmethod pgqa-set-markers ((node pgqa-node))
  "Turn `region' into a marker and recurse into children."
  (let* ((reg-vec (oref node region))
	 (reg-start (elt reg-vec 0))
	 (reg-end (elt reg-vec 1))
	 (m-start (make-marker))
	 (m-end (make-marker)))
    (set-marker m-start reg-start)
    (set-marker m-end reg-end)

    ;; The insertion types are such that the start and end markers always span
    ;; only the original region.
    (set-marker-insertion-type m-start t)
    (set-marker-insertion-type m-end nil)

    (oset node markers (vector m-start m-end)))
  )

;; `state' is an instance of `pgqa-deparse-state' class.
;;
;; `indent' determines indentation of the node.
(defmethod pgqa-dump ((node pgqa-node) state indent &optional face)
  "Turn a node and its children into string."
  nil)

;; Metadata to control deparsing of an SQL query and the contained
;; expressions.
;;
;; Note: the simplest way to control (not) breaking line in front of a
;; subquery is to create a separate instance of this class for the subquery.
;;
;; XXX If adding a new field, check if it's subject to backup / restore by
;; pgqa-dump method of pgqa-operator class. (Is this backup / restore worth
;; separate functions?)
(defclass pgqa-deparse-state ()
  (
   (indent
    :initarg :indent
    ;; This is useful when the query is not printed out structured, i.e. line
    ;; breaks are only enforced by fill-column. In this case the indentation
    ;; passed to pgqa-dump is useless because it gets increased due to
    ;; recursive calls.
    :documentation "Indentation level of the top level query.")

   (indent-top-expr
    :initarg :indent-top-expr
    :documentation "Indentation, relative to `indent' above, of top level
expression, e.g. the text following top-level keyword. See
`pgqa-deparse-top-keyword' for details.")

   (next-column
    :initarg :next-column
    :documentation "Column at which the following node (or its leading space)
should start.")

   (next-space
    :initarg :next-space
    :documentation "The width of the next space to be printed out.")

   (line-empty
    :initarg :line-empty
    :documentation "Is the current line empty or contains only whitespace?")

   (result
    :initarg :result
    :documentation "String to which each node appends its textual
representation.")
   )

  "State of the deparsing process."
  )

;; init-col-src is column (in addition to the indentation) at which the source
;; query starts.
(defun pgqa-init-deparse-state (indent init-col-src line-empty)
  "Initialize instance of `pgqa-deparse-state'."

  (let ((indent-width (* indent tab-width)))
    (make-instance 'pgqa-deparse-state
		   :indent indent
		   :indent-top-expr 0
		   :next-column (+ indent-width init-col-src)
		   :next-space 0
		   :line-empty t
		   :result (make-string indent-width 32))))

(defmethod pgqa-deparse-newline ((state pgqa-deparse-state) indent)
  "Adjust deparse state so that deparsing continues at a new line, properly
indented."

  ;; No recursive increments of indentation if the query should look "flat".
  (unless pgqa-multiline-query
    (setq indent (oref state indent)))

  (let* ((indent-width (* indent tab-width))
	 (result (oref state result)))
    (setq result (concat result "\n"))
    (setq result (concat result
			 (make-string indent-width 32)))
    (oset state result result)
    (oset state next-column indent-width)
    (oset state line-empty t))
  )

(defmethod pgqa-deparse-space ((state pgqa-deparse-state))
  "Write space to deparsing output."
  (let ((space (oref state next-space)))
    (oset state result
	  (concat (oref state result)
		  (make-string space 32)))
    (oset state next-column (+ (oref state next-column) space)))
  ;; Restore the default value of next-space.
  (oset state next-space 1))

;; Prepare for insertion of `str', i.e. add line break or space, as
;; needed.
(defmethod pgqa-deparse-string-prepare ((state pgqa-deparse-state) str indent)
  (let ((col-incr 0)
	(space (oref state next-space)))
    (if (> space 0)
	(setq col-incr (1+ col-incr)))
    (setq col-incr (+ col-incr (string-width str)))

    ;; Zero space currently can't be broken.
    ;;
    ;; TODO Consider if there are special cases not to subject to this
    ;; restriction, and maybe introduce custom variable that allows breaking
    ;; even the zero space.
    (if (and fill-column (> space 0)
	     (> (+ (oref state next-column) col-incr) fill-column))
	(progn
	  (pgqa-deparse-newline state indent)
	  ;; No space (in addition to indentation) after newline.
	  (oset state next-space 0)))

    (pgqa-deparse-space state))
  )

(defmethod pgqa-deparse-string ((state pgqa-deparse-state) str indent
				&optional face)
  "Write arbitrary string to deparsing output."

  (pgqa-deparse-string-prepare state str indent)

  ;; In some cases we stick the next string to the current one w/o space
  ;; (which currently makes newline impossible - see
  ;; pgqa-deparse-string-prepare).
  (if (or
       (string= str "(") (string= str "["))
      (oset state next-space 0))

  (let ((str-highlighted str))
    (if (and (null noninteractive) face)
	(add-text-properties 0 (string-width str-highlighted)
			     '(font-lock-face pgqa-operator) str-highlighted))
    (oset state result (concat (oref state result) str-highlighted)))
  (oset state next-column (+ (oref state next-column) (string-width str)))
  ;; clear line-empty if there string contains non-whitespace character.
  (if (string-match "\\S-" str)
      (oset state line-empty nil)))

;; Top-level keyword might deserve special attention, e.g. adding tabs between
;; itself and the following expression.
;;
;; `first' indicates that this is the first top-level keyword of the whole
;; query.
(defmethod pgqa-deparse-top-keyword ((state pgqa-deparse-state) keyword indent
				     first)
  "Dump top-level keyword (SELECT, INSERT, FROM, WHERE, etc.)"

  (let ((first-offset 0))
    ;; For the structured output, all top-level keywords except for the first
    ;; one need line break.
    (if (and pgqa-multiline-query (null first))
	(progn
	  (pgqa-deparse-newline state indent)
	  ;; No space in front of the keyword, even if the keyword does not
	  ;; cause line break itself.
	  (oset state next-space 0))
      )

    ;; The first line of the deparsed query may be indented more than the rest
    ;; (see indent-estimate in pgqa-deparse).
    (if (and pgqa-multiline-query first)
	(setq first-offset
	      (- (oref state next-column)
		 (* (oref state indent) tab-width))))

    (pgqa-deparse-string state keyword indent)

    (if (and pgqa-multiline-query (null pgqa-clause-newline))
	;; Ensure the appropriate space in front of the following expression.
	(let* ((indent-top-expr (oref state indent-top-expr))
	       (nspaces (- (* indent-top-expr tab-width) (string-width keyword))))

	  ;; Anything wrong about computation of indent-top-expr?
	  (if (< nspaces 1)
	      (error "indent-top-expr is too low"))

	  ;; No extra space if the next text would exceed fill-column.
	  (if (and
	       fill-column
	       (>= (+ (oref state next-column) nspaces) fill-column))
	      (setq nspaces 1))

	  ;; Shorten the space so that the expression (most likely column
	  ;; list) starts at the same position as the other expressions
	  ;; (e.g. table list).
	  (if (> first-offset 0)
	      (progn
		(setq nspaces (- nspaces first-offset))
		;; Make sure at least one space is left.
		(if (< nspaces 1)
		    (setq nspaces 1))))

	  (oset state next-space nspaces))
      )
    )

  (if pgqa-clause-newline
      ;; Avoid the single space that pgqa-dump of pgqa-operator class puts in
      ;; front of operators.
      (oset state next-space 0))
  )

(defclass pgqa-query (pgqa-node)
  (
   (kind :initarg :kind)
   (target-expr :initarg :target-expr)
   (from-expr :initarg :from-expr)
   ;; Table subject to INSERT / UPDATE / DELETE.
   (target-table :initarg :target-table)
   )
  "A generic SQL query (or subquery).")

(defmethod pgqa-dump ((node pgqa-query) state indent)
  "Turn query into a string."

  ;; For mutiline output, compute the first column for expressions.
  ;;
  ;; TODO Adjust when adding the missing keywords.
  (if pgqa-multiline-query
      (let ((top-clauses)
	    (max-width 0)
	    (indent-expr))
	(if (slot-boundp node 'target-expr)
	    (push "SELECT" top-clauses))

	(if (slot-boundp node 'target-table)
	    (push "UPDATE" top-clauses))

	(if (slot-boundp node 'from-expr)
	    (let ((fe (oref node from-expr)))
	      (if (> (length (oref fe from-list)) 0)
		  (push "FROM" top-clauses))
	      (if (slot-boundp fe 'qual)
		  (push "WHERE" top-clauses))
	      ))

	;; Find out the maximum length.
	(dolist (i top-clauses)
	  (let ((width (string-width i)))
	    (if (> width max-width)
		(setq max-width width))))

	;; At least one space must follow.
	(setq max-width (1+ max-width))

	;; Round up to the nearest multiple of tab-width.
	(setq max-width
	      (+ max-width
		 (- tab-width (% max-width tab-width))))

	(oset state indent-top-expr (/ max-width tab-width)))
    )

  (let ((indent-clause))
    (if pgqa-clause-newline
	(setq indent-clause (1+ indent))
      ;; Extra tab might have been added in front of the clause (to ensure
      ;; that all clauses of the query start at the same position), so all
      ;; lines of the clause must start at that position.
      (setq indent-clause (oref state indent-top-expr)))

    (if (string= (oref node kind) "SELECT")
	(let ((te (oref node target-expr)))
	  (pgqa-deparse-top-keyword state "SELECT" indent t)

	  ;; Enforce line break if necessary.
	  ;;
	  ;; TODO The same for GROUP BY / ORDER BY.
	  (if pgqa-clause-newline
	      (pgqa-deparse-newline state indent-clause))

	  (pgqa-dump te state indent-clause)))

    (if (string= (oref node kind) "UPDATE")
	(let ((tt (oref node target-table))
	      (te (oref node target-expr)))
	  (pgqa-deparse-top-keyword state "UPDATE" indent t)
	  (pgqa-dump tt state indent-clause)
	  (pgqa-deparse-top-keyword state "SET" indent nil)
	  (pgqa-dump te state indent-clause)))

    (if (slot-boundp node 'from-expr)
	(let ((from-expr (oref node from-expr)))
	  ;; Update may or may not have FROM clause.
	  (pgqa-dump from-expr state indent)))
    )
  )

(defclass pgqa-from-expr (pgqa-node)
  (
   (from-list :initarg :from-list)
   (qual :initarg :qual)
   )
  "FROM expression of an SQL query."
)

(defmethod pgqa-dump ((node pgqa-from-expr) state indent)
  (let ((from-list (oref node from-list))
	(indent-clause))

    ;; See the related comment in pgqa-dump method of pgqa-query class.
    (if pgqa-clause-newline
	(setq indent-clause (1+ indent))
      (setq indent-clause (oref state indent-top-expr)))

    ;; INSERT, UPDATE or DELETE statement can have the list empty.
    (if (> (length from-list) 0)
	(progn
	  (pgqa-deparse-top-keyword state "FROM" indent nil)
	  (if pgqa-clause-newline
	      (pgqa-deparse-newline state indent-clause))

	  (if (= (length from-list) 1)
	      (pgqa-dump (car from-list) state indent-clause)
	    ;; Line breaks and indentation are best handled if we turn the list
	    ;; into a comma operator.
	    ;;
	    ;; XXX Shouldn't this be done by parser?
	    (let (
		  (l (make-instance 'pgqa-operator :op ","
				    :args from-list
				    :prec pgqa-precedence-comma))
		  )
	      (pgqa-dump l state indent-clause)
	      )
	    )
	  )
      )
    )

  (if (slot-boundp node 'qual)
      (let ((indent-clause))
	;; Like above. XXX Should the whole body of the function be wrapped in
	;; an extra "let" construct, which initializes the variable only
	;; once?)
	(if pgqa-clause-newline
	    (setq indent-clause (1+ indent))
	  (setq indent-clause (oref state indent-top-expr)))

	;; `space' should be up-to-date as the FROM clause is mandatory.
	(pgqa-deparse-top-keyword state "WHERE" indent nil)
	(if pgqa-clause-newline
	    (pgqa-deparse-newline state indent-clause))
	(pgqa-dump (oref node qual) state indent-clause)))
  )

;; A single argument represents table, function, subquery or VALUES clause. If
;; the 'args slot has elements, the FROM list entry is a join.
;;
;; TODO Consider a superclass that both this and pgqa-obj could inherit from
;; (both need the alias).
(defclass pgqa-from-list-entry (pgqa-expr)
  (
   (alias :initarg :alias)
   ;; For a simple entry, the value is one of "table", "function", "subquery",
   ;; "values". For join it's "left", "rignt", "full" (nil implies inner join
   ;; as long as 'args has 2 elements).
   (kind :initarg :kind)

   ;; Join expression if the entry is a join.
   (qual :initarg :qual)
   )
  "From list entry (table, join, subquery, ...)"
)

(defmethod pgqa-dump ((node pgqa-from-list-entry) state indent)
  "Print out FROM list entry (table, join, subquery, etc.)."

  (let* ((args (oref node args))
	 (nargs (length args))
	 (is-join (= nargs 2)))
    (cl-assert (or (= nargs 1) (= nargs 2)))

    (if (and is-join pgqa-join-newline
	     ;; Only break the line if it hasn't just happened for any reason.
	     (null (oref state line-empty)))
	(progn
	  (oset state next-space 0)
	  (pgqa-deparse-newline state indent)))

    (pgqa-dump (car args) state indent)

    (if is-join
	(progn
	  (if pgqa-multiline-join
	      (progn
		(oset state next-space 0)
		(pgqa-deparse-newline state indent)))

	  (let ((kind (oref node kind)))
	    (if kind
	      (pgqa-deparse-string state (upcase kind) indent)))
	  (pgqa-deparse-string state "JOIN" indent)

	  (pgqa-dump (nth 1 args) state indent)

	  (pgqa-deparse-string state "ON" indent)

	  (pgqa-dump (oref node qual) state (1+ indent))))

    (if (slot-boundp node 'alias)
	(progn
	  (pgqa-deparse-string state "AS" indent)
	  (pgqa-deparse-string state (oref node alias) indent))
      )
    )
  )

(defclass pgqa-func-call (pgqa-node)
  (
   (name :initarg :name)
   ;; arguments are stored as a single pgqa-operator having :op=",".
   (args :initarg :args)
   )
  "Function call"
)

(defmethod pgqa-dump ((node pgqa-func-call) state indent)
  "Print out function call"

  ;; Here pgqa-operator stands for face, not the operator object.
  (pgqa-dump (oref node name) state indent pgqa-operator)

  ;; No space between function name and the parenthesis.
  (oset state next-space 0)
  (pgqa-deparse-string state "(" indent pgqa-operator)
  (let ((args (oref node args)))
    (if (> (length args) 0)
	(pgqa-dump args state indent)))
  ;; Likewise, no space after.
  (oset state next-space 0)
  (pgqa-deparse-string state ")" indent pgqa-operator)
)

;; Number is currently stored as a string - should this be changed?
(defclass pgqa-number (pgqa-node)
  (
   (value :initarg :value)
   )
  "A number.")

(defmethod pgqa-dump ((node pgqa-number) state indent)
  "Turn number into a string."
  (let ((str (oref node value)))
    (pgqa-deparse-string state str indent))
  )

(defclass pgqa-obj (pgqa-node)
  (
   ;; x.y expression can represent either column "y" of table "x" or table "y"
   ;; of schema "x". Instead of teaching parser to recognize the context (is
   ;; it possible?) let's postpone resolution till analysis phase.
   ;;
   ;; Note that the number of arguments is not checked during "raw parsing",
   ;; and that asterisk can be at any position, not only the last one.
   (args :initarg :args)

   (alias :initarg :alias)
   ;; TODO
   ;;
   ;; 1. Remove double-quotes (and remember there were some?)
   ;;
   ;; 2. Turn unquoted values to lower case.
   )
  "Table or column reference.")

(defmethod pgqa-dump ((node pgqa-obj) state indent &optional face)
  "Turn an SQL object into a string."
  (let ((str (mapconcat 'format (oref node args) ".")))
    (pgqa-deparse-string state str indent face)))

(defclass pgqa-operator (pgqa-expr)
  (
   (op :initarg :op)
   (prec :initarg :prec
	 :documentation "Operator precedence, for the sake of printing.")
   (postfix :initarg :postfix
	    :initform nil
	    :documentation "If the expression has only one argument, it's
considered to be an unary operator. This slot tells whether it's a postfix
operator. nil indicates it's a prefix operator.")
   )
  "Generic operator.")

;; TODO Some other constructs need parentheses, e.g. query in a
;; sublink. Should they be given certain precedence just because of this?
(defun pgqa-child-needs-parens (node arg)
  "Find out if argument of an operator node should be parenthesized."
  (let ((prec (oref node prec))
	(prec-child
	 (if (object-of-class-p arg pgqa-operator)
	     (oref arg prec))))
    (and prec prec-child (> prec prec-child)))
  )

(defun pgqa-is-multiline-operator (node)
  "Should the argument be printed in structured way?"

  (if (and pgqa-multiline-operator
	   (eq (eieio-object-class node) 'pgqa-operator))
      (let ((op (oref node op)))

	;; Comma is not a multi-line operator as such, only its arguments
	;; are.
	(null (string= op ",")))
    )
  )

;; indent relates to the operator, not argument.
(defun pgqa-indent-operator-first-argument (state indent arg-idx)
  "Prepare position for the first argument of a multiline operator."

  (let* ((s (oref state result))
	 (i (1- (length s))))
    (if
	;; No duplicate newline if we already have one.
	(and pgqa-clause-newline (= arg-idx 0) (oref state line-empty))
	(let ((indent-extra
	       (- indent (/ (oref state next-column) tab-width))))
	  ;; indent is for the operator, so add 1 more level for the argument.
	  (setq indent-extra (1+ indent-extra))
	  (oset state result
		(concat (oref state result)
			(make-string (* indent-extra tab-width) 32))))
      (progn
	(pgqa-deparse-newline state (1+ indent))
	(oset state next-space 0))))
  )

(defmethod pgqa-dump ((node pgqa-operator) state indent)
  "Turn operator expression into a string."
  (let* ((args (oref node args))
	 (nargs (length args))
	 (op (oref node op))
	 (is-unary
	  (null
	   (or (cdr args)
	       ;; comma operator can have a single arg, so test explicitly.
	       (string= op ","))))
	 (is-comma (string= op ","))
	 (i 0)
	 (multiline
	  (and
	   pgqa-multiline-operator
	   (pgqa-is-multiline-operator node)))
	 (arg-multiline-prev))

    (dolist (arg args)
      (let* ((parens (pgqa-child-needs-parens node arg))
	     (result-backup)
	     (next-column-backup)
	     (next-space-backup)
	     (arg-is-operator (eq (eieio-object-class arg) 'pgqa-operator))
	     (arg-is-comma (and arg-is-operator (string= (oref arg op) ",")))
	     (arg-is-te (eq (eieio-object-class arg) 'pgqa-target-entry))
	     ;; FROM list entry?
	     (arg-is-fe (eq (eieio-object-class arg) 'pgqa-from-list-entry))
	     (arg-multiline)
	     (indent-arg indent))

	(if arg-is-te
	    (setq arg-multiline (pgqa-is-multiline-operator (oref arg expr)))
	  (setq arg-multiline (pgqa-is-multiline-operator arg)))

	(if (or (and is-unary (null (oref node postfix))) (> i 0))
	    (let ((omit-space))

	      ;; Never put space in front of comma.
	      (setq omit-space is-comma)

	      ;; Should each arg start at a new line?
	      (if multiline
		  (progn
		    (pgqa-deparse-newline state indent)
		    (setq omit-space t)))

	      (if omit-space
	      	  (oset state next-space 0))

	      ;; Assign a face if op is a "regular operator".
	      (let ((face))
		(if (null (string= op ","))
		    (setq face pgqa-operator))
		(pgqa-deparse-string state op indent face))))

	;; Ensure correct initial position for the argument output in case the
	;; operator spans multiple lines.
	(if multiline
	    ;; If the arg is also an operator, it'll take care of the newline
	    ;; (and correct indentation) itself. Otherwise we need to break
	    ;; the line explicitly.
	    ;;
	    ;; Unary operator is an exception - it looks better if both
	    ;; operator and argument end up on the same line. Again, if the
	    ;; argument of the unary operator is operator itself (which
	    ;; includes parentheses), it'll take care of the line breaks
	    ;; automatically.
	    (if (and
		 (null arg-is-operator)
		 (> (length args) 1))
		(pgqa-indent-operator-first-argument state indent i))

	  (if is-comma
	      (if (or
		   ;; If an "ordinary" expression follows a multi-line
		   ;; operator within comma operator (e.g. SELECT list), break
		   ;; the line so that the multi-line operator does not share
		   ;; even a single line with the current argument.
		   ;;
		   ;; TODO Consider a custom variable to switch this behavior
		   ;; on / off.
		   (and arg-multiline-prev (null arg-multiline))

		   ;; Definitely break the line if user requires each target
		   ;; list / from list entry to begin on a new line.
		   ;;
		   ;; Do nothing for i = 0 because pgqa-clause-item-newline
		   ;; requires pgqa-clause-newline to be set, which takes care
		   ;; of the first entry. Only take action if the comma is a
		   ;; target list of FROM list (i.e. do not affect things like
		   ;; function argument list).
		   (and pgqa-clause-item-newline (> i 0)
			(or arg-is-te arg-is-fe)))
		(progn
		  (pgqa-deparse-newline state indent)
		  (oset state next-space 0))))
	  )

	(if parens
	    (progn
	      (if multiline
		  ;; "(" should appear on a new line, indented as the argument
		  ;; would be if there were no parentheses. (The argument
		  ;; itself will eventually be given extra indentation.)
		  (pgqa-indent-operator-first-argument state indent i))

	      (pgqa-deparse-string state "(" indent)
	      ;; No space, whatever follows "(".
	      (oset state next-space 0)))

	;; Comma needs special treatment because it doesn't look nice if it's
	;; the first character on a line.
	;;(if is-comma
	;; TODO Methods to backup / restore the state as a whole.
	(progn
	  (setq result-backup (oref state result))
	  (setq next-column-backup (oref state next-column))
	  (setq next-space-backup (oref state next-space)))
	;;)

	;; Serialize the argument now, giving it additional indentation if
	;; user wants the output structured.
	(let ((indent-extra 0))
	  (if multiline
	      (progn
		(setq indent-extra (1+ indent-extra))
		;; The extra indentation due to parentheses, mentioned above.
		(if parens
		    (setq indent-extra (1+ indent-extra)))))
	  (setq indent-arg (+ indent indent-extra))
	  (pgqa-dump arg state indent-arg))

	(if
	    ;; If the argument should be followed by comma, line should have
	    ;; broken in front of the argument. So restore the previous state
	    ;; and dump the argument again, with fill-column temporarily
	    ;; decreased by one. That should make the argument appear on the
	    ;; new line too.
	    (and
	     ;;(string= op ",")
	     (>= (oref state next-column) fill-column)
	     (< i (1- nargs)))
	    (progn
	      (oset state result result-backup)
	      (oset state next-column next-column-backup)
	      (oset state next-space next-space-backup)

	      ;; TODO Loop until the line is broken correctly, but don't let
	      ;; fill-column reach value that lets little or no space on the
	      ;; line. But only try once if the related custom variable(s)
	      ;; allow for line break between opening paren and the following
	      ;; character or closing paren and the preceding character.
	      (let ((fill-column (1- fill-column)))
		(pgqa-dump arg state indent-arg))
	      )
	  )

	(if parens
	    (progn
	      ;; The closing parenthesis should be on a separate line, like
	      ;; the opening one.
	      (if (and multiline
		       ;; Row expression is not considered a multi-line
		       ;; operator, so it looks better if the ")" is stuck to
		       ;; it.
		       ;;
		       ;; TODO Verify the behavior when the last expression of
		       ;; the row is a multi-line operator.
		       (or (null arg-is-comma) (= (length (oref arg args)) 1)))
		  (pgqa-deparse-newline state (1+ indent)))
	      ;; Never space in front of ")".
	      (oset state next-space 0)
	      (pgqa-deparse-string state ")" indent)))

	(if (and is-unary (oref node postfix))
	    (pgqa-deparse-string state op indent))

	(setq i (1+ i))

	(setq arg-multiline-prev arg-multiline))
      )
    )
)

(defclass pgqa-target-entry (pgqa-node)
  (
   (expr :initarg :expr)
   (alias :initarg :alias)
   )
  "Target list entry")

(defmethod pgqa-dump ((node pgqa-target-entry) state indent)
  "Turn target list entry into a string."

  (pgqa-dump (oref node expr) state indent)

  (if (slot-boundp node 'alias)
      (progn
	(pgqa-deparse-string state "AS" indent)
	(pgqa-deparse-string state (oref node alias) indent))
    )
)

(provide 'pgqa-node)
