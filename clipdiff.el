;;; clipdiff.el --- Apply quick‑and‑dirty diffs from kill‑ring  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: convenience, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a quick and dirty way to apply diffs from
;; `kill-ring' to the current buffer.  It handles ordinary unified
;; diffs or headerless +/- patches (which the built-in `diff-*'
;; commands require).

;; The function `clipdiff-apply' applies the diff stored in
;; `kill-ring' to the current buffer.  It uses the first entry in
;; `kill-ring' by default, but you can specify a different entry with
;; a prefix argument.  The function `clipdiff-apply-diff' can be used
;; to apply a diff string programmatically.

;;; Code:

(provide 'clipdiff)
;;; clipdiff.el ends here

(eval-when-compile (require 'cl-lib))

(cl-defstruct clipdiff--ln
  "A struct representing a line in the diff output.
TEXT is the text of the line.
FLAG is the flag for the line, one of ?\s, ?-, or ?+."
  text flag)

(defun clipdiff--parse-hunks (diff)
  "Return a list of hunks parsed from unified or header‑less DIFF.
Each element is (BEFORE . AFTER) where BEFORE / AFTER are lists of
`clipdiff--ln' structs preserving the original diff flag."
  (let* ((lines (split-string diff "\n" t))
         hunks before after in)
    (dolist (l lines)
      (cond
       ;; Skip file headers (--- and +++ lines)
       ((or (string-prefix-p "--- " l)
            (string-prefix-p "+++ " l))
        nil)
       ;; Hunk headers
       ((string-prefix-p "@@" l)
        (when in
          (push (cons (nreverse before) (nreverse after)) hunks)
          (setq before nil after nil))
        (setq in t))
       ((and (not (string-empty-p l))
             (member (aref l 0) '(?+ ?- ?\s)))
        (setq in t)
        (let ((text (substring l 1)))
          (pcase (aref l 0)
            (?- (push (make-clipdiff--ln :text text :flag ?-) before))
            (?+ (push (make-clipdiff--ln :text text :flag ?+) after))
            (?  (push (make-clipdiff--ln :text text :flag ?\s) before)
                (push (make-clipdiff--ln :text text :flag ?\s) after)))))
       (t nil)))
    (when in
      (push (cons (nreverse before) (nreverse after)) hunks))
    (nreverse hunks)))

(defun clipdiff--lines->string (lines &optional indent-context)
  "Concatenate LINES (list of `clipdiff--ln') into a newline‑joined string.
When INDENT-CONTEXT is non-nil, add an extra space to context lines."
  (mapconcat
   (lambda (l)
     (concat
      (when (and indent-context (eq (clipdiff--ln-flag l) ?\s))
        " ")
      (clipdiff--ln-text l)))
   lines "\n"))

;;;###autoload
(defun clipdiff-apply (&optional arg)
  "Apply diff stored in `kill-ring' to the current buffer.
With prefix ARG, use the kill‑ring entry at position ARG.
Handles ordinary unified diffs or header‑less +/‑ patches.  When a
literal search for a hunk fails, retry with an extra space added to each
context line (those that were \" \" in the diff).  Stops on the first
failing hunk."
  (interactive "P")
  (clipdiff-apply-diff (current-kill (or arg 0) t)))

(defun clipdiff-apply-diff (diff)
  "Apply DIFF to the current buffer.
See `clipdiff-apply' for details on diff handling."
  (let* ((hunks (clipdiff--parse-hunks diff))
         (n 0) (n-adjusted 0))
    (save-excursion
      (dolist (h hunks)
        (incf n)
        (pcase-let* ((`(,before . ,after) h)
                     (b-str (clipdiff--lines->string before))
                     (a-str (clipdiff--lines->string after)))
          (goto-char (point-min))
          (if (search-forward b-str nil t)
              (replace-match a-str t t)
            ;; fallback: indent context lines one space
            (let ((b2 (clipdiff--lines->string before t))
                  (a2 (clipdiff--lines->string after t)))
              (goto-char (point-min))
              (if (search-forward b2 nil t)
                  (progn
                    (replace-match a2 t t)
                    (incf n-adjusted))
                (error "Hunk %d failed \
(no literal or spaced context match)" n)))))))
    (message "Applied %d hunk%s%s."
             n (if (= n 1) "" "s")
             (if (> n-adjusted 0)
                 (format " (%d used spaced‑context fallback)" n-adjusted)
               ""))))

(provide 'clipdiff)
;;; clipdiff.el ends here
