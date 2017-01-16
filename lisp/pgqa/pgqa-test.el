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

(require 'pgqa)

(defun pgqa-run-single-formatting-test (&optional indent)
  (let ((state))
    (pgqa-check-customizations)
    (pgqa-parse)
    (setq state (pgqa-deparse-batch indent))
    (princ (oref state result))))

(defun prepare-next-test ()
  (erase-buffer)
  (insert input-text)
  (pgqa-mode))

(defun pgqa-test-formatting ()
  "Test query formatting using various values of the related custom variables."

  (if (null noninteractive)
      (user-error "pgqa-format-query-batch function should only be used in
batch mode"))

  (let ((input-text (buffer-string)))

    (pgqa-mode)
    (setq pgqa-multiline-query nil)
    (setq pgqa-multiline-join nil)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (setq fill-column 40)

    (pgqa-run-single-formatting-test)

    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join nil)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; pgqa-clause-newline with pgqa-multiline-operator set.
    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator nil)
    (setq pgqa-clause-newline t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; pgqa-clause-newline with pgqa-multiline-operator not set.
    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator t)
    (setq pgqa-clause-newline t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (prepare-next-test)
    (setq tab-width 4)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; This test differs from the previous one by
    ;; pgqa-clause-newline. (pgqa-multiline-operator is cleared too, to make
    ;; the effect of pgqa-clause-newline visible). The test verifies that:
    ;; 1. the top level clause receives extra indentation if the "top keyword"
    ;; (e.g. SELECT) reaches behind the first tab position, 2. all the lines
    ;; of the clause obey this indentation - this is why we adjust fill-column
    ;; too.
    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator nil)
    (setq pgqa-clause-newline nil)
    (setq fill-column 20)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; pgqa-clause-item-newline
    (prepare-next-test)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator nil)
    (setq pgqa-clause-newline t)
    (setq pgqa-clause-item-newline t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; The same, with pgqa-multiline-operator set.
    (prepare-next-test)
    (setq pgqa-multiline-operator t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    ;; Tests of non-zero indentation.
    (setq pgqa-multiline-query nil)
    (setq pgqa-multiline-join nil)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (setq pgqa-clause-newline nil)
    (setq pgqa-clause-item-newline nil)
    (setq fill-column 78)
    (princ "\n\n")
    (pgqa-run-single-formatting-test 2)

    (setq pgqa-multiline-query t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test 2)))
