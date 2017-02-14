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

;; Besides attaching the markers and overlays to nodes, add them to
;; pgqa-query-markers and pgqa-query-overlays lists, for easy cleanup.
(defun pgqa-setup-node-gui (node context)
  "Turn region(s) into a markers and add overlay(s) to the node."
  (let* ((reg-vec (oref node region))
	 (reg-start (elt reg-vec 0))
	 (reg-end (elt reg-vec 1))
	 (m-start (make-marker))
	 (m-end (make-marker))
	 (o))
    (set-marker m-start reg-start)
    (set-marker m-end reg-end)

    ;; The insertion types are such that the start and end markers always span
    ;; only the original region.
    (set-marker-insertion-type m-start t)
    (set-marker-insertion-type m-end nil)

    ;; Create an overlay and make it point at the node.
    (setq o (make-overlay m-start m-end))
    (overlay-put o 'node node)

    ;; Keep track of both markers and overlay.
    (push m-start pgqa-query-markers)
    (push m-end pgqa-query-markers)
    (push o pgqa-query-overlays)

    (oset node markers (vector m-start m-end)))
  )

;; For performance reasons (see the Overlays section of Elisp documentation)
;; we assign the face as text property, although cleanup would be simpler if
;; we assigned the face via overlay.
(defun pgqa-set-node-face (node context)
  (if (eq (eieio-object-class node) 'pgqa-operator)
      (let ((op-node (oref node op-node)))
	(if (and op-node (slot-boundp op-node 'markers))
	    (let* ((m (oref op-node markers))
		   (m-start (elt m 0))
		   (m-end (elt m 1)))
	      (put-text-property m-start m-end
				 'font-lock-face 'pgqa-operator-face))
	  )
	)
    )
  )

;; Remove the face added previously by pgqa-set-node-face.
(defun pgqa-reset-node-face (node context)
  (if (eq (eieio-object-class node) 'pgqa-operator)
      (let ((op-node (oref node op-node)))
	(if (and op-node (slot-boundp op-node 'markers))
	    (let* ((m (oref op-node markers))
		   (m-start (elt m 0))
		   (m-end (elt m 1)))
	      (remove-text-properties m-start m-end
				 '(font-lock-face nil)))
	  )
	)
    )
  )

;; An utility to apply a function to all nodes of a tree.
;;
;; If the walker function changes the nodes, caller is responsible for having
;; taken a copy of the original node.
;;
;; Currently it seems more useful to process the sub-nodes before the actual
;; node.
(defmethod pgqa-node-walk ((node pgqa-node) walker context)
  "Run the walker function on sub-nodes and the node itself"

  ;; It seems safer to force each node to implement this function explicitly
  ;; than to process the node w/o sub-nodes here.
  (error (format "walker method not implemented for %s class"
		 (eieio-object-class-name node))))

(defun pgqa-node-walk-list (node-list walker context)
  "Run the node walker function on each item of a list."
  (dolist (node node-list)
    (pgqa-node-walk node walker context)))

(defclass pgqa-query (pgqa-node)
  (
   (kind :initarg :kind)
   (target-expr :initarg :target-expr)
   (from-expr :initarg :from-expr)
   (group-expr :initarg :group-expr)
   (order-expr :initarg :order-expr)
   ;; Table subject to INSERT / UPDATE / DELETE.
   (target-table :initarg :target-table)
   )
  "A generic SQL query (or subquery).")

(defmethod pgqa-node-walk ((node pgqa-query) walker context)
  (if (slot-boundp node 'target-expr)
      (pgqa-node-walk (oref node target-expr) walker context))

  (if (slot-boundp node 'target-table)
      (pgqa-node-walk (oref node target-table) walker context))

  (if (slot-boundp node 'from-expr)
      (let ((fe (oref node from-expr)))
	(if (oref fe from-list)
	    (pgqa-node-walk-list (oref fe from-list) walker context))
	(if (slot-boundp fe 'qual)
	    (pgqa-node-walk (oref fe qual) walker context))))

  (if (slot-boundp node 'group-expr)
      (pgqa-node-walk (oref node group-expr) walker context))

  (if (slot-boundp node 'order-expr)
      (pgqa-node-walk (oref node order-expr) walker context))

  (funcall walker node context))

(defclass pgqa-from-expr (pgqa-node)
  (
   (from-list :initarg :from-list)
   (qual :initarg :qual)
   )
  "FROM expression of an SQL query."
)

;; A single argument represents table, function, subquery or VALUES clause. If
;; the 'args slot has elements, the FROM list entry is a join.
(defclass pgqa-from-list-entry (pgqa-expr)
  (
   ;; Instance of pgqa-from-list-entry-alias.
   (alias :initarg :alias)
   ;; For a simple entry, the value is one of "table", "function", "query",
   ;; "values". For join it's "left", "rignt", "full" (nil implies inner join
   ;; as long as 'args has 2 elements).
   (kind :initarg :kind)

   ;; Join expression if the entry is a join.
   (qual :initarg :qual)
   )
  "From list entry (table, join, subquery, ...)"
)

(defmethod pgqa-node-walk ((node pgqa-from-list-entry) walker context)
  (if (slot-boundp node 'alias)
      (funcall walker (oref node alias) context))

  ;; If node is a join, recurse into the sides and process qualifier.
  (let ((args (oref node args)))
    (if (= (length args) 2)
	(progn
	  (pgqa-node-walk (car args) walker context)
	  (pgqa-node-walk (car (cdr args)) walker context)
	  (pgqa-node-walk (oref node qual) walker context)))
    (funcall walker node context)))

;; Query in the FROM list is not a typical from-list-entry.
;;
;; TODO Consider custom variable that controls whether parentheses are on the
;; same lines the query starts and ends respectively.
(defun pgqa-dump-from-list-query (query state indent)
  ;; XXX Can we do anything batter than breaking the line if either or
  ;; pgqa-join-newline or pgqa-multiline-join (or both) are nil?
  (if (and (null (oref state line-empty)) pgqa-multiline-query)
      (progn
	(oset state next-space 0)
	(pgqa-deparse-newline state indent)))

  (pgqa-deparse-string state "(" indent)

  (let ((state-loc state))
    (if pgqa-multiline-query
	;; Use a separate state to print out query.
	;;
	;; init-col-src of 1 stands for the opening parenthesis.
	(progn
	  (setq state-loc (pgqa-init-deparse-state
			   (+ (oref state indent) indent) 1 t
			   (oref state buffer-pos)))
	  (oset state-loc next-column (oref state next-column))
	  (oset state-loc result (oref state result))))

    (pgqa-dump query state-loc 0)
    (oset state result (oref state-loc result)))

  (oset state next-space 0)
  (pgqa-deparse-string state ")" indent)
)

;; TODO Store argument list to :args if the alias has some.
(defclass pgqa-from-list-entry-alias (pgqa-expr)
  (
   (name :initarg :name)
   )
  "From list entry alias."
)

(defclass pgqa-sortgroup-expr (pgqa-expr)
  (
   ;; t and nil mean GROUP BY and ORDER BY respectively.
   (is-group :initarg :is-group)
   )
  "GROUP BY or ORDER BY expression."
)

(defmethod pgqa-node-walk ((node pgqa-sortgroup-expr) walker context)
  (pgqa-node-walk-list (oref node args) walker context))

(defclass pgqa-func-call (pgqa-node)
  (
   (name :initarg :name)
   ;; arguments are stored as a single pgqa-operator having :op=",".
   (args :initarg :args)
   )
  "Function call"
)

(defmethod pgqa-node-walk ((node pgqa-func-call) walker context)
  (funcall walker (oref node name) context)
  (if (oref node args)
      (pgqa-node-walk (oref node args) walker context))
  (funcall walker node context))

;; Number is currently stored as a string - should this be changed?
(defclass pgqa-number (pgqa-node)
  (
   (value :initarg :value)
   )
  "A number.")

(defmethod pgqa-node-walk ((node pgqa-number) walker context)
  (funcall walker node context))

(defclass pgqa-obj (pgqa-expr)
  (
   ;; The :args slot (inherited from pgqa-expr) contains the dot-separated
   ;; components of table / column reference.
   ;;
   ;; XXX Can't we simply use pgqa-expr class here?
   ;;
   ;; x.y expression can represent either column "y" of table "x" or table "y"
   ;; of schema "x". Instead of teaching parser to recognize the context (is
   ;; it possible?) let's postpone resolution till analysis phase.
   ;;
   ;; Note that the number of arguments is not checked during "raw parsing",
   ;; and that asterisk can be at any position, not only the last one.
   )
  "Table or column reference.")

(defmethod pgqa-node-walk ((node pgqa-obj) walker context)
  ;; The individual args are strings, so only process the alias.
  (funcall walker node context))

(defclass pgqa-operator (pgqa-expr)
  (
   (op :initarg :op)

   ;; Region info and marker of the operator string is stored separate so that
   ;; access to the string remains straightforward.
   (op-node :initarg :op-node
	    :initform nil)

   (prec :initarg :prec
	 :documentation "Operator precedence, for the sake of printing.")
   (postfix :initarg :postfix
	    :initform nil
	    :documentation "If the expression has only one argument, it's
considered to be an unary operator. This slot tells whether it's a postfix
operator. nil indicates it's a prefix operator.")
   )
  "Generic operator.")

(defmethod pgqa-node-walk ((node pgqa-operator) walker context)
  (pgqa-node-walk-list (oref node args) walker context)
  (if (oref node op-node)
      (funcall walker (oref node op-node) context))
  (funcall walker node context))

(defclass pgqa-target-entry (pgqa-node)
  (
   (expr :initarg :expr)
   (alias :initarg :alias)
   )
  "Target list entry")

(defmethod pgqa-node-walk ((node pgqa-target-entry) walker context)
  (pgqa-node-walk (oref node expr) walker context)
  (funcall walker node context))

(provide 'pgqa-node)
