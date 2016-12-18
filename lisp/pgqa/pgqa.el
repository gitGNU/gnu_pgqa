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

(require 'seq)
(require 'semantic)
;(require 'wisent)
(require 'pgqa-node)
(require 'pgqa-parser)

(defgroup pgqa nil
  "PGQA mode user settings."
  :group 'programming-group)

(defcustom pgqa-multiline-query t
  "If non-nil, query will be deparsed in structured format that spans multiple
lines."
  :type 'boolean
)

(defcustom pgqa-multiline-join t
  "If non-nil, the JOIN operator (including the INNER, LEFT, etc. predicate)
will always start on a new line.

Can only be set if `pgqa-multiline-query' variable is set."
  :type 'boolean
)

(defcustom pgqa-join-newline t
  "If non-nil, the tables joined (i.e. not only the JOIN operator) start on a
new line.

Can only be set if `pgqa-multiline-join' variable is set."
  :type 'boolean
)

(defcustom pgqa-multiline-operator nil
  "If non-nil, operators will be deparsed in a structured way.

Can only be set if both `pgqa-multiline-query' and `pgqa-multiline-join'
variables are set."
  :type 'boolean
)

(defun pgqa-check-customizations ()
  "Throw `user-error' if value of any custom variable is illegal."

  (if pgqa-multiline-join
      (if (null pgqa-multiline-query)
	  (user-error "`pgqa-multiline-join' requires
`pgqa-multiline-query'")))

  (if pgqa-join-newline
      (if (null pgqa-multiline-join)
	  (user-error "`pgqa-join-newline' requires `pgqa-multiline-join'")))

  (if pgqa-multiline-operator
      (if (or (null pgqa-multiline-query) (null pgqa-multiline-join))
	  (user-error "`pgqa-multiline-operator' requires both
`pgqa-multiline-query' and `pgqa-multiline-join'"))))

;; XXX Consider defcustom instead.
(defvar pgqa-mode-prefix-key "\C-c")

(defvar pgqa-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" '(menu-item "Format Query" pgqa-format-query
				    :visible t))
    map))

(defvar pgqa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar pgqa] (cons "PGQA" pgqa-mode-prefix-map))
    (define-key map pgqa-mode-prefix-key pgqa-mode-prefix-map)
    map))

(defvar pgqa-mode-syntax-table
  (let ((st (make-syntax-table))
	(op-chars))
    ;; Characters that constitute SQL operators are best handled as
    ;; punctuation.
    ;;
    ;; TODO Modify the entries for characters that do not belong to
    ;; pgqa-terminals-non-grouped, e.g. colon - it should be punctuation
    ;; because it's contained in double-colon (i.e. cast operator), but is not
    ;; legal alone. Find out if other operators like this exist. However, if
    ;; it's hard for lexer to accept such "multi-character punctuations",
    ;; introduce special kind of token for them.

    ;; First, collect all the characters to be possibly contained in
    ;; operators.
    (dolist (g pgqa-operator-groups)
      (let ((ops (cdr (cdr g))))
	(dolist (op ops)
	  (if (= (string-width op) 1)
	      (push (string-to-char op) op-chars)))))
    (setq op-chars (append op-chars pgqa-terminals-non-grouped))

    ;; Now adjust the syntax table accordingly.
    (dolist (i op-chars)
      (let ((cs (char-syntax i)))
	;; Do not change syntax class of terminals indispensable as opening
	;; and closing parentheses - these are not likely to be used in
	;; operators in valid PG expression.
	(if (null (or (eq cs ?() (eq cs ?))))
	    (modify-syntax-entry i "." st))))

    ;; Underscore is a legal component of SQL identifiers.
    (modify-syntax-entry ?_ "w" st)

    ;; Single quote is punctuation character in the parent table, but we need
    ;; it to denote strings.
    (modify-syntax-entry ?' "\"" st)
    ;; In addition to its "punctuation" role, dash is a comment starter.
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n "> " st)

    ;; Asterix should constitute for a symbol rather than punctuation. That
    ;; makes more sense if we want to use it as a column wildcard. However
    ;; this distinction only matters to syntax highlighting. As for the
    ;; grammar we need a special class for it anyway, see
    ;; get-next-query-token.
    (modify-syntax-entry ?* "w" st)

    ;; All the other, ordinary punctuations.
    (modify-syntax-entry ?= "." st)

    st)
  "Syntax table used while in 'pgqa-mode'.")

;; Keywords that parser does not understand (so far?), but user expect them to
;; be highlighted.
;;
;; TODO Consider if strings are safer than symbols, in terms of conflict with
;; other elisp modules.
(defvar pgqa-keywors-higlight-only
  '(
   ;; We (mis)use this list to highlight keywords that represent
   ;; operators. Since there are only few (and PG user should not be able to
   ;; add more), special list does not seem to make sense. XXX We might want
   ;; to remove these anyway if we decide to use special face for operators.
   AND IS ISNULL OR NOTNULL

   BEGIN COMMENT CREATE DECLARE ELSE END EXCEPTION FUNCTION IF
   LANGUAGE RETURN RETURNS TABLE THEN))

;; Turn pgqa-keywords-base list into a regular expression.
(defun pgqa-keywords ()
  (list (concat "\\<"
		(mapconcat 'symbol-name
			   (append pgqa-keyword-symbols
				   pgqa-keywors-higlight-only)
			   "\\>\\|\\<")
		"\\>")))

;; TODO Make sure the funcion can also turn the mode off.
(define-derived-mode pgqa-mode
  prog-mode
  "PGQA"
  "Major mode to parse and analyze SQL queries, with respect to the SQL \
dialect and concepts of query processing that PostgreSQL uses."

  ;; GNU Emacs 25 was available when the PGQA development started. Don't let
  ;; us accept the burden of supporting older versions.
  ;;
  ;; Raising the error during mode initialization seems relatively good
  ;; place. It does not prevent user from calling functions manually (and
  ;; possibly getting various errors), doing the check in the individual
  ;; functions is not appropriate either.
  (if (< emacs-major-version 25)
      (error "PGQA requires GNU Emacs 25 or newer"))

  (setq-local font-lock-defaults
	      '((pgqa-keywords) nil t nil))

  ;; As we don't have a regular comment starter, the lexers' regular
  ;; expression needs to be adjusted.
  (setq-local semantic-lex-comment-regex "--.*")

  ;; For fill-column function to work properly.
  (setq-local comment-start "--")

  (add-hook 'kill-buffer-hook 'kill-query-buffer-hook t t))

;; A list of buffers for which at least one query buffer was created.
;;
;; Although user can yank the link button into any buffer, the default
;; yank-excluded-properties should not allow creation of a working link this
;; way. Only the root buffers have the important elements removed from the
;; exclude list, see separate-query.
(defvar pgqa-root-buffers ())

(defun query-button-action (pos)
  (switch-to-buffer-other-window (get-text-property pos 'target-buffer-name)))

;; Get query string out of the current region, put it into a new buffer and
;; replace the source region with a mark. Finally set major mode of the new
;; buffer to pgqa-mode.
;;
;; TODO Make sure that by including the button into region user selects the
;; whole original query, not only the visible part. Also, inclusion of the
;; button should probably kill the query buffer, because we can hardly avoid
;; yanking the whole text and losing the overlay.
;;(global-set-key "\C-xp" 'separate-query)
;;
;; TODO Check sort input values by position before doing anything. (Mark can
;; be at greater position than point.)
(defun separate-query (p m)
  (interactive "r")

  ;; Reject calling the function on a buffer that another call might have
  ;; produced.
  ;;
  ;; The point of separate-query is to treat SQL query separately from the
  ;; containing file, which may follow another syntax (typically plpgsql
  ;; language). Further splitting of the SQL query is not that beneficial, and
  ;; would increase the code complexity a lot. (One particular issue that
  ;; recursive separation would introduce is potential insertion of a query
  ;; buffer into the pointed-by query buffer itself. yank-excluded-properties
  ;; is a question too.)
  (if (local-variable-p 'is-query-buffer)
      (error "Query separation does not work recursively"))

  ;; Also reject the separation if the pgqa-mode was set manually (or
  ;; automatically, because of file suffix).
  (if (eq major-mode 'pgqa-mode)
      (error "separate-query is not allowed in pgqa-mode"))

  ;; TODO Make sure the following actions are performed atomically.
  (let ((query-buffer)
	(query-buf-name)
	(query-buf-name-width)
	(query-buf-button-name)
	(query-buf-button-width)
	(region-width)
	(query-button))

    ;; Make sure no "query button" exists in the region yet.
    (if (text-property-any p m 'action 'query-button-action)
	(error "The region already contains a query button"))

    (setq query-buffer
	 (let ((oldbuf (current-buffer)))
	    (with-current-buffer (generate-new-buffer "query")
	      (insert-buffer-substring oldbuf p m)
	      (pgqa-mode)

	      ;; Distinguish the buffer from one in which user enabled the
	      ;; pgqa-mode manually.
	      (setq-local is-query-buffer t)

	      ;; Disallow yanking of links into the query buffer. Such an
	      ;; attempt should always result in addition of plain text, so
	      ;; that we don't end up with too complex buffer hierarchy,
	      ;; possibly containing cyclic dependencies.
	      ;;
	      ;; XXX As I don't exactly understand make-variable-buffer-local
	      ;; (e.g. does the note "cannot be undone" in documentation mean
	      ;; that variable setup this way is inappropriate for a major
	      ;; mode, because such mode can't be disabled properly?) I prefer
	      ;; to setup the buffer-local variable from scratch.
	      (setq-local yank-excluded-properties
			  (copy-sequence yank-excluded-properties))
	      (dolist (i (list 'type 'button 'category 'face 'invisible
			       'display 'action 'rear-nonsticky))
		(if (not (memq i yank-excluded-properties))
		    (push i yank-excluded-properties)))
	      (current-buffer))))

    ;; Make kill-query-buffer-hook aware of a buffer that needs to be checked.
    ;;
    ;; (Do nothing if another query of this buffer was separated before.)
    (if (not (memq (current-buffer) pgqa-root-buffers))
	(push (current-buffer) pgqa-root-buffers))

    ;; Turn the initial part of the query into a button.
    (setq query-buf-name (buffer-name query-buffer))
    (setq query-buf-name-width (string-width query-buf-name))

    ;; Make sure the button does not hide any text.
    (setq region-width (- m p))
    (if (<= query-buf-name-width region-width)
	(progn
	  (setq query-buf-button-width query-buf-name-width)
	  (setq query-buf-button-name query-buf-name))
      (progn
	  (setq query-buf-button-width region-width)
	  (setq query-buf-button-name
		(truncate-string-to-width query-buf-name region-width))))

    (setq query-button (make-text-button p m))
    (button-put query-button 'display query-buf-button-name)

    ;; Hide the part of the query not included in the button label.
    (add-text-properties
     (+ p query-buf-button-width) m
     '(invisible t rear-nonsticky t mouse-face highlight
		 help-echo "mouse-1: visit this query in other window"))

    ;; Assign implementation function to the button.
    (button-put query-button 'action 'query-button-action)
    (button-put query-button 'follow-link
            (lambda (pos)
              (eq (get-char-property pos 'action) 'query-button-action)))

    ;; Make the button aware of the buffer it points to.
    (button-put query-button 'target-buffer-name query-buf-name))

  ;; Allow copying of the buttons (links) within the current buffer. (This
  ;; includes copying from other buffers.)
  (if (not (local-variable-p 'yank-excluded-properties))
      (setq-local yank-excluded-properties ()))

  ;; Add a (local) kill-buffer-hook to remove ourselves from "root buffers".
  (add-hook 'kill-buffer-hook (lambda ()
				(setq pgqa-root-buffers
				      (delq (current-buffer)
					    pgqa-root-buffers)))
	    t t))

;; Kill the buffers created by separate-query function, after having their
;; updates applied to the "root buffer".
(defun kill-query-buffers ()
  (interactive)

  ;; separate-query does not allow for recursion, so we should not as well.
  (if (local-variable-p 'is-query-buffer)
      (error "The current buffer should have no (child) query buffers"))

  (let (
	(qbutton-pos)
	(qbuf-name))
    ;; Instead of computing the staring position (that should take the shift
    ;; due to deletion into account) we always start from beginning, relying
    ;; on the kill-buffer-hook of the query buffer to remove the text the
    ;; properties are associated with. XXX Consider if it's yet worth
    ;; computing the position.
    (while
	(setq qbutton-pos
	      (text-property-any (point-min) (point-max)
					     'action 'query-button-action))
      (setq qbuf-name (get-text-property qbutton-pos 'target-buffer-name))
      (kill-buffer qbuf-name)))

  ;; Restore the original behaviour of yanking.
  (kill-local-variable 'yank-excluded-properties))

;; TODO 1. Remove all occurrences of a link pointing to this buffer from the
;; kill ring. (Maybe simply clear the whole kill ring.) 2. Make sure tha undo
;; can't restore the link in any root buffer.
(defun kill-query-buffer-hook ()
  ;; Assuming the mode has been set on buffer creation, we don't expect this
  ;; function to be called in other modes. If user switched the mode off
  ;; deliberatly, we don't know what else could have changed, so it's better
  ;; to give up. XXX Should we try to make turning off the mode impossible
  ;; (unless user plays with elisp) if this is the query buffer?
  (if (not (equal major-mode 'pgqa-mode))
      (error "The current buffer is not a query buffer"))

  ;; The contents should not be referenced by any button if we set the
  ;; 'pgqa-mode manually, i.e. without separate-query command.
  (when (local-variable-p 'is-query-buffer)
    (let ((qbuf-name (buffer-name (current-buffer)))
	  (qbutton-pos)
	  (qbutton)
	  (start)
	  (start-column)
	  (end)
	  (qbuf-min (point-min))
	  (qbuf-max (point-max)))

      (dolist (parent-buffer pgqa-root-buffers)
	(let ((found-some))
	  (with-current-buffer (get-buffer parent-buffer)
	    ;; TODO Skip the following if *local* undo list of the query
	    ;; buffer is empty.
	    (while
		(setq qbutton-pos (text-property-any
				   (point-min) (point-max)
				   'target-buffer-name qbuf-name))

	      ;; Locate the original query (the button could have been moved
	      ;; in the meantime).
	      (setq qbutton (button-at qbutton-pos))
	      (setq start (button-start qbutton))
	      (setq end (button-end qbutton))
	      (delete-region start end)

	      ;; Insert the contents of the query buffer.
	      (goto-char start)
	      (setq start-column (current-column))
	      (insert-buffer-substring qbuf-name qbuf-min qbuf-max)
	      (indent-region start (+ start (- qbuf-max qbuf-min)) start-column)
	      (setq found-some t))

	    (if found-some
		;; Make restoration of the link to the query buffer
		;; impossible.
		;;
		;; There's no easy way to remove only some items from the undo
		;; list, as each undo list item may depend on the previous
		;; one(s).
		(setq buffer-undo-list nil)))))))

  ;; Prevent user from yanking link to the buffer anywhere.
  ;;
  ;; TODO Try to remove only the related item of the ring, or avoid cleaning
  ;; of the ring if no link to the buffer being killed is there.
  (setq kill-ring nil))

(defun pgqa-format-query (&optional indent)
 "Format SQL query that the current buffer contains. If region is active,
only the selected part of the buffer is processed.

The optional prefix argument INDENT tells how much should the query be
indented. If it's passed, then INDENT times `tab-width' spaces are inserted
in front of each line. Without it, the command tries to guess the indentation
from first position of the query."
 (interactive "P")

 ;; The customizations do not affect parsing, but by checking early we avoid
 ;; wasting effort on parsing.
 (pgqa-check-customizations)

 (pgqa-parse)
 (pgqa-deparse indent))

(defun pgqa-format-query-batch ()
  "Read SQL query from the standard input, format it and write it to the
standard output."

  (if (null noninteractive)
      (user-error "pgqa-format-query-batch function should only be used in
batch mode"))

  (pgqa-mode)

  (let ((state))
    (pgqa-check-customizations)
    (pgqa-parse)
    (setq state (pgqa-deparse-batch))
    (princ (oref state result))))

(provide 'pgqa)
