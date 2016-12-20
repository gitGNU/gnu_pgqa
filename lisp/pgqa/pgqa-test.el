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

(defun pgqa-run-single-formatting-test ()
  (let ((state))
    (pgqa-check-customizations)
    (pgqa-parse)
    (setq state (pgqa-deparse-batch))
    (princ (oref state result))))

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

    (erase-buffer)
    (insert input-text)
    (pgqa-mode)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join nil)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)


    (erase-buffer)
    (insert input-text)
    (pgqa-mode)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline nil)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (erase-buffer)
    (insert input-text)
    (pgqa-mode)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator nil)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)

    (erase-buffer)
    (insert input-text)
    (pgqa-mode)
    (setq pgqa-multiline-query t)
    (setq pgqa-multiline-join t)
    (setq pgqa-join-newline t)
    (setq pgqa-multiline-operator t)
    (princ "\n\n")
    (pgqa-run-single-formatting-test)))
