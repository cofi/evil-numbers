;;; evil-numbers.el --- increment/decrement numbers like in vim

;; Copyright (C) 2011 by Michael Markert
;;               2018 by Jan Path
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Maintainer: Jan Path <jan@jpath.de>
;; Contributors: Matthew Fidler <matthew.fidler@gmail.com>
;;               Michael Markert <markert.michael@gmail.com>
;;               Jan Path <jan@jpath.de>
;; URL: http://github.com/janpath/evil-numbers
;; Git-Repository: git://github.com/janpath/evil-numbers.git
;; Created: 2011-09-02
;; Version: 0.5
;; Keywords: numbers increment decrement octal hex binary

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Increment / Decrement binary, octal, decimal and hex literals

;; works like C-a/C-x in vim, i.e. searches for number up to eol and then
;; increments or decrements and keep zero padding up

;; Known Bugs:
;; See http://github.com/janpath/evil-numbers/issues

;; Install:

;; (require 'evil-numbers)

;; and bind, for example:

;; (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
;; (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
;; (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)

;; or only in evil's normal and visual state:

;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
;; (define-key evil-visual-state-map (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
;;
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
;; (define-key evil-visual-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)
;; (define-key evil-visual-state-map (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)

;; Usage:
;; Go and play with your numbers!

;;; Code:

(require 'evil)

(defconst evil-numbers/superscript-alist
  (cons
   (cons ?- ?⁻)
   (cons
    (cons ?+ ?⁺)
    (mapcar (lambda (i) (cons
                         (string-to-char (number-to-string i))
                         (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" i)))
            (number-sequence 0 9)))))

(defconst evil-numbers/subscript-alist
  (cons
   (cons ?- ?₋)
   (cons
    (cons ?+ ?₊)
    (mapcar (lambda (i) (cons
                         (string-to-char (number-to-string i))
                         (aref "₀₁₂₃₄₅₆₇₈₉" i)))
            (number-sequence 0 9)))))

(defgroup evil-numbers nil
  ""
  :group 'convenience)

;;;###autoload
(defcustom evil-numbers/padDefault nil
  "Whether numbers are padded by default"
  :group 'evil-numbers
  :type 'boolean
  :options '(nil t))

(defun evil-numbers/swap-alist (alist)
  "Swap association list"
  (mapcar (lambda (x) (cons (cdr x) (car x))) alist))

(defun evil-numbers/translate-with-alist (alist string)
  "Translate every symbol in string using alist"
  (funcall
   (if (stringp string) #'concat (lambda (x) x))
   (mapcar (lambda (c) (cdr (assoc c alist))) string)))

;;;###autoload (autoload 'evil-numbers/inc-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt (amount beg end type &optional incremental padded)
  "Increment the number at point or after point before end-of-line by AMOUNT.
When region is selected, increment all numbers in the region by AMOUNT

NO-REGION is internal flag that allows
`evil-numbers/inc-at-point' to be called recursively when
applying the regional features of `evil-numbers/inc-at-point'.

INCREMENTAL causes the first number to be increased by 1*AMOUNT, the second by
2*AMOUNT and so on.

PADDED is whether numbers should be padded (e.g. 10 -> 09). nil is default
behaviour set by `evil-numbers/pad-default', t is the opposite of
`evil-numbers/pad-default', '(t) enables padding and '(nil) disables padding.
Numbers with a leading zero are always padded. Signs are preserved when padding
is enabled, i.e. increasing a negative number to a positive will result in a
number with a + sign.

"
  :motion nil
  (interactive "*<c><R>")
  (let ((amount (or amount 1))
        (padded (if (consp padded) (car padded)
                  (funcall
                   (if padded #'not (lambda (x) x))
                   evil-numbers/padDefault))))
    (cond
     ((and beg end type)
      (let ((count 1))
        (save-excursion
          (save-match-data
            (funcall
             (if (eq type 'block)
                 (lambda (f) (evil-apply-on-block f beg end nil))
               (lambda (f) (funcall f beg end)))
             (lambda (beg end)
               (evil-with-restriction beg end
                 (while (re-search-forward "\\(?:0\\(?:[Bb][01]+\\|[Oo][0-7]+\\|[Xx][0-9A-Fa-f]+\\)\\|[+-]?[0-9]+\\|[⁻⁺]?[⁰¹²³⁴⁵⁶⁷⁸⁹]\\|[₋₊]?[₀₁₂₃₄₅₆₇₈₉]\\)" nil t)
                   ;; Backward char, to cancel out the forward-char below. We need
                   ;; this, as re-search-forwards puts us behind the match.
                   (backward-char)
                   (evil-numbers/inc-at-pt (* amount count) nil nil nil)
                   (if incremental (setq count (+ count 1)))
                   ;; Undo vim compatability.
                   (forward-char 1)))))))))
     (t (save-match-data
          ;; forward-char, so that we do not match the number directly behind us.
          (forward-char)
          (if (not (evil-numbers/search-number))
              (error "No number at point or until end of line")
            (let ((pad
                   (lambda (len sign arg)
                     (format
                      (format "%%%s0%dd" (if sign "+" "") len)
                      arg)))
                  (replace-with
                   (lambda (from to)
                     (skip-chars-backward
                      (funcall from "0123456789"))
                     (skip-chars-backward
                      (funcall from "+-") (- (point) 1))
                     (when (looking-at
                            (format
                             "[%s]?\\([%s]+\\)"
                             (funcall from "-+")
                             (funcall from "0123456789")))
                       (replace-match
                        (funcall
                         from
                         (let* ((padded
                                 (or padded
                                     (eq ?0 (string-to-char (match-string 1)))))
                                (input (string-to-number
                                        (funcall to (match-string 0))))
                                (output (+ amount input))
                                (len (- (match-end 0) (match-beginning 0)))
                                (signed (and
                                         (memq (string-to-char (match-string 0))
                                               (funcall from '(?+ ?-)))
                                         (or padded (>= input 0)))))
                           (format
                            (format "%%%s0%dd"
                                    (if signed "+" "")
                                    (if padded len 0))
                            output))))
                       ;; Moves point one position back to conform with Vim
                       (forward-char -1)
                       t))))
              (or
               ;; find binary literals
               (evil-numbers/search-and-replace "0[bB][01]+" "01" "\\([01]+\\)" amount 2)

               ;; find octal literals
               (evil-numbers/search-and-replace "0[oO][0-7]+" "01234567" "\\([0-7]+\\)" amount 8)

               ;; find hex literals
               (evil-numbers/search-and-replace "0[xX][0-9a-fA-F]*"
                                                "0123456789abcdefABCDEF"
                                                "\\([0-9a-fA-F]+\\)" amount 16)

               ;; find superscript literals
               (funcall
                replace-with
                (lambda (x)
                  (evil-numbers/translate-with-alist
                   evil-numbers/superscript-alist x))
                (lambda (x)
                  (evil-numbers/translate-with-alist
                   (evil-numbers/swap-alist evil-numbers/superscript-alist)
                   x)))

               ;; find subscript literals
               (funcall
                replace-with
                (lambda (x)
                  (evil-numbers/translate-with-alist
                   evil-numbers/subscript-alist x))
                (lambda (x)
                  (evil-numbers/translate-with-alist
                   (evil-numbers/swap-alist evil-numbers/subscript-alist)
                   x)))

               ;; find normal decimal literals
               (funcall replace-with (lambda (x) x) (lambda (x) x))
               (error "No number at point or until end of line")))))))))

;;;###autoload (autoload 'evil-numbers/dec-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt (amount beg end type &optional incremental padded)
  "Decrement the number at point or after point before end-of-line by AMOUNT.

If a region is active, decrement all the numbers at a point by AMOUNT.

This function uses `evil-numbers/inc-at-pt'"
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type incremental padded))

;;;###autoload (autoload 'evil-numbers/inc-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt-incremental (amount beg end type padded)
  "Increment the number at point or after point before end-of-line by AMOUNT.

If a region is active, increment all the numbers at a point by AMOUNT*n, where
n is the index of the number among the numbers in the region, starting at 1.
That is increment the first number by AMOUNT, the second by 2*AMOUNT, and so
on."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt amount beg end type 'incremental padded))

;;;###autoload (autoload 'evil-numbers/dec-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt-incremental (amount beg end type padded)
  "Like `evil-numbers/inc-at-pt-incremental' but with negated argument AMOUNT"
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type 'incemental padded))

;;; utils

(defun evil-numbers/search-number ()
  "Return non-nil if a binary, octal, hexadecimal or decimal literal at or after point.
If point is already within or after a literal it stays.

The literals have to be in the following forms:
binary: 0[bB][01]+, e.g. 0b101 or 0B0
octal: 0[oO][0-7]+, e.g. 0o42 or 0O5
hexadecimal 0[xX][0-9a-fA-F]+, e.g. 0xBEEF or 0Xcafe
decimal: [0-9]+, e.g. 42 or 23"
  (or
   ;; numbers or format specifier in front
   (looking-back (rx (or (+? digit)
                         (in "⁰¹²³⁴⁵⁶⁷⁸⁹")
                         (in "₀₁₂₃₄₅₆₇₈₉" )
                         (and "0" (or (and (in "bB") (*? (in "01")))
                                      (and (in "oO") (*? (in "0-7")))
                                      (and (in "xX") (*? (in digit "A-Fa-f"))))))))
   ;; search for number in rest of line
   ;; match 0 of specifier or digit, being in a literal and after specifier is
   ;; handled above
   (and
	  (re-search-forward "[[:digit:]⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉]" (point-at-eol) t)
	  (or
	   (not (memq (char-after) '(?b ?B ?o ?O ?x ?X)))
	   (/= (char-before) ?0)
	   (and (> (point) 2)				; Should also take bofp into consideration
		      (not (looking-back "\\W0" 2)))
	   ;; skip format specifiers and interpret as bool
	   (<= 0 (skip-chars-forward "bBoOxX"))))))

(defun evil-numbers/search-and-replace (look-back skip-back search-forward inc base)
  "When looking back at LOOK-BACK skip chars SKIP-BACK backwards and replace number incremented by INC in BASE and return non-nil."
  (when (looking-back look-back)
    (skip-chars-backward skip-back)
    (search-forward-regexp search-forward)
    (replace-match (evil-numbers/format (+ inc (string-to-number (match-string 1) base))
                                        (if evil-numbers/padDefault (length (match-string 1)) 1)
                                        base))
	  ;; Moves point one position back to conform with Vim
	  (forward-char -1)
    t))

(defun evil-numbers/format (num width base)
  "Format NUM with at least WIDTH space in BASE"
  (cond
   ((= base 2) (evil-numbers/format-binary num width))
   ((= base 8) (format (format "%%0%do" width) num))
   ((= base 16) (format (format "%%0%dX" width) num))
   (t "")))

(defun evil-numbers/format-binary (number &optional width fillchar)
  "Format NUMBER as binary.
Fill up to WIDTH with FILLCHAR (defaults to ?0) if binary
representation of NUMBER is smaller."
  (let (nums
        (fillchar (or fillchar ?0)))
    (while (> number 0)
      (push (number-to-string (% number 2)) nums)
      (setq number (truncate number 2)))
    (let ((len (length nums)))
      (apply #'concat
             (if (and width (< len width))
                 (make-string (- width len) fillchar)
               "")
             nums))))

(provide 'evil-numbers)
;;; evil-numbers.el ends here
