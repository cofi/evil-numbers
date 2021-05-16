;;; evil-numbers.el --- Increment/decrement numbers like in VIM -*- lexical-binding: t -*-

;; Copyright (C) 2011 by Michael Markert
;;               2020 by Julia Path
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Maintainer: Julia Path <julia@jpath.de>
;; Contributors: Matthew Fidler <matthew.fidler@gmail.com>
;;               Michael Markert <markert.michael@gmail.com>
;;               Julia Path <julia@jpath.de>
;; URL: http://github.com/juliapath/evil-numbers
;; Git-Repository: git://github.com/juliapath/evil-numbers.git
;; Created: 2011-09-02
;; Version: 0.5
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience tools

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

;; Increment / Decrement binary, octal, decimal and hex literals.

;; Works like C-a/C-x in vim, i.e. searches for number up to EOL and
;; then increments or decrements and keep zero padding up.

;; Known Bugs:
;; See http://github.com/juliapath/evil-numbers/issues

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

(defconst evil-numbers--superscript-alist
  (cons
   (cons ?- ?⁻)
   (cons
    (cons ?+ ?⁺)
    (mapcar (lambda (i) (cons
                         (string-to-char (number-to-string i))
                         (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" i)))
            (number-sequence 0 9)))))

(defconst evil-numbers--subscript-alist
  (cons
   (cons ?- ?₋)
   (cons
    (cons ?+ ?₊)
    (mapcar (lambda (i) (cons
                         (string-to-char (number-to-string i))
                         (aref "₀₁₂₃₄₅₆₇₈₉" i)))
            (number-sequence 0 9)))))

(defgroup evil-numbers nil
  "Support number increment/decrement."
  :group 'convenience)

;;;###autoload
(defcustom evil-numbers/padDefault nil
  "Whether numbers are padded by default."
  :group 'evil-numbers
  :type 'boolean
  :options '(nil t))

(defun evil-numbers--swap-alist (alist)
  "Swap association list ALIST."
  (mapcar (lambda (x) (cons (cdr x) (car x))) alist))

(defun evil-numbers--translate-with-alist (alist string)
  "Translate every symbol in STRING using ALIST."
  (funcall
   (if (stringp string) #'concat #'identity)
   (mapcar (lambda (c) (cdr (assoc c alist))) string)))

(defun evil-numbers--encode-super (x)
  "Convert X string into super-script."
  (evil-numbers--translate-with-alist
   evil-numbers--superscript-alist x))
(defun evil-numbers--decode-super (x)
  "Convert X string from super-script into regular characters."
  (evil-numbers--translate-with-alist
   (evil-numbers--swap-alist evil-numbers--superscript-alist) x))

(defun evil-numbers--encode-sub (x)
  "Convert X string into sub-script."
  (evil-numbers--translate-with-alist
   evil-numbers--subscript-alist x))
(defun evil-numbers--decode-sub (x)
  "Convert X string from sub-script into regular characters."
  (evil-numbers--translate-with-alist
   (evil-numbers--swap-alist evil-numbers--subscript-alist) x))

(defun evil-numbers--inc-at-pt-impl (amount beg end padded)
  "Increment the number at the current POINT by AMOUNT limited by BEG and END.

Keep padding when PADDED is non-nil.

Return non-nil on success, leaving the point at the end of the number."
  (save-match-data
    (cond
     ;; Failure (caller may error).
     ((not (evil-numbers--search-number beg end))
      nil)

     ;; Find binary literals.
     ((evil-numbers--search-and-replace
       '(("+-" . *)
         ("0"  . 1)
         ("bB" . 1)
         ("01" . +))
       1 ;; Sign group.
       4 ;; Number group.
       amount 2 padded
       #'identity #'identity)
      t)

     ;; Find octal literals.
     ((evil-numbers--search-and-replace
       '(("+-"  . *)
         ("0"   . 1)
         ("oO"  . 1)
         ("0-7" . +))
       1 ;; Sign group.
       4 ;; Number group.
       amount 8 padded
       #'identity #'identity)
      t)

     ;; Find hex literals.
     ((evil-numbers--search-and-replace
       '(("+-"         . *)
         ("0"          . 1)
         ("xX"         . 1)
         ("[:xdigit:]" . +))
       1 ;; Sign group.
       4 ;; Number group.
       amount 16 padded
       #'identity #'identity)
      t)

     ;; Find decimal literals.
     ((evil-numbers--search-and-replace
       '(("+-"         . *)
         ("0123456789" . +))
       1 ;; Sign group.
       2 ;; Number group.
       amount 10 padded
       #'identity #'identity)
      t)

     ;; Find decimal literals (super-script).
     ((evil-numbers--search-and-replace
       '(("⁺⁻"         . *)
         ("⁰¹²³⁴⁵⁶⁷⁸⁹" . +))
       1 ;; Sign group.
       2 ;; Number group.
       amount 10 padded
       #'evil-numbers--decode-super
       #'evil-numbers--encode-super)
      t)

     ;; Find decimal literals (sub-script).
     ((evil-numbers--search-and-replace
       '(("₊₋"         . *)
         ("₀₁₂₃₄₅₆₇₈₉" . +))
       1 ;; Sign group.
       2 ;; Number group.
       amount 10 padded
       #'evil-numbers--decode-sub
       #'evil-numbers--encode-sub)
      t)

     ;; Failure (caller may error).
     (t
      nil))))

;;;###autoload (autoload 'evil-numbers/inc-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt
  (amount beg end type &optional incremental padded)

  "Increment the number at point or after point before `end-of-line' by AMOUNT.
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
number with a + sign."
  :motion nil
  (interactive "*<c><R>")

  (setq amount (or amount 1))
  (setq padded (if (consp padded)
                   (car padded)
                 (funcall (if padded #'not #'identity)
                          evil-numbers/padDefault)))
  (cond
   ;; Handle selection (block or line).
   ;; Run this function in a loop (falling through to the `t' case).
   ((and beg end type)
    (let ((count 1))
      (save-excursion
        (funcall
         (if (eq type 'block)
             (lambda (f) (evil-apply-on-block f beg end nil))
           (lambda (f) (funcall f beg end)))
         (lambda (beg end)
           (evil-with-restriction beg end
             (goto-char beg)
             (while (evil-numbers--inc-at-pt-impl
                     amount (point) (point-max) padded)
               (when incremental
                 (setq count (+ count 1))))))))))

   ;; Handle the simple case, either the cursor is over a number,
   ;; or a number exists between the cursor and `end-of-line'.
   (t
    (let ((point-next
           (save-excursion
             (when (evil-numbers--inc-at-pt-impl
                    amount (point-at-bol) (point-at-eol) padded)
               (point)))))

      (if (null point-next)
          (error "No number at point or until end of line")

        ;; Moves point one position back to conform with VIM,
        ;; see `evil-adjust-cursor' for details.
        (goto-char (1- point-next))
        t)))))

;;;###autoload (autoload 'evil-numbers/dec-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt (amount beg end type &optional incremental padded)
  "Decrement the number at point or after point before `end-of-line' by AMOUNT.

If a region is active, decrement all the numbers at a point by AMOUNT.

This function uses `evil-numbers/inc-at-pt'"
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type incremental padded))

;;;###autoload (autoload 'evil-numbers/inc-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt-incremental (amount beg end type padded)
  "Increment the number at point or after point before `end-of-line' by AMOUNT.

If a region is active, increment all the numbers at a point by AMOUNT*n, where
n is the index of the number among the numbers in the region, starting at 1.
That is increment the first number by AMOUNT, the second by 2*AMOUNT, and so
on."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt amount beg end type 'incremental padded))

;;;###autoload (autoload 'evil-numbers/dec-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt-incremental (amount beg end type padded)
  "Like `evil-numbers/inc-at-pt-incremental' but with negated argument AMOUNT."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type 'incremental padded))

;;; Utilities.

(defun evil-numbers--search-number (beg end)
  "Return non-nil if a number literal at or after point.

If point is already within or after a literal it stays.

Literals may be in binary, octal, hexadecimal or decimal forms:
binary: 0[bB][01]+, e.g. 0b101 or 0B0
octal: 0[oO][0-7]+, e.g. 0o42 or 0O5
hexadecimal 0[xX][0-9a-fA-F]+, e.g. 0xBEEF or 0Xcafe
decimal: [0-9]+, e.g. 42 or 23.

BEG and END are limits for searching,
note that searching still starts at POINT."
  (or
   ;; Numbers or format specifier in front.
   (and (> (point) beg)
        (save-excursion
          ;; `forward-char' so that we do not match the number
          ;; directly behind us.
          ;;
          ;; Check the range to avoid end-of-buffer warning.
          ;; Harmless but happens with block selection every
          ;; time which is unreasonably noisy.
          (unless (>= (1+ (point)) (point-max))
            (forward-char))
          (looking-back
           (rx (or (+? digit)
                   (in "⁰¹²³⁴⁵⁶⁷⁸⁹")
                   (in "₀₁₂₃₄₅₆₇₈₉" )
                   (and "0"
                        (or (and (in "bB") (*? (in "01")))
                            (and (in "oO") (*? (in "0-7")))
                            (and (in "xX") (*? (in xdigit)))))))
           beg)))
   ;; Search for number in rest of line match 0 of specifier or digit,
   ;; being in a literal and after specifier is handled above.
   (and
    (re-search-forward "[[:digit:]⁰¹²³⁴⁵⁶⁷⁸⁹₀₁₂₃₄₅₆₇₈₉]" end t)
    (or
     (not (memq (char-after) '(?b ?B ?o ?O ?x ?X)))
     (/= (char-before) ?0)
     ;; Should also take bofp into consideration.
     (not (looking-back "\\W0" (max beg (- (point) 2))))
     ;; Skip format specifiers and interpret as boolean.
     (<= 0 (skip-chars-forward "bBoOxX" end))))))

(defun evil-numbers--match-from-skip-chars (skip-chars dir do-check do-match)
  "Match SKIP-CHARS in DIR (-1 or 1).

When DO-CHECK is non-nil, any failure to match returns nil.
When DO-MATCH is non-nil, match data is set.

Each item in SKIP-CHARS is a cons pair.
- The first item is the argument to pass to
  `skip-chars-forward' or `skip-chars-backward'.
- The second item specifies how many characters to match,
  Valid values:
  - Symbol `+' one or more.
  - Symbol `*' zero or more.
  - `integerp' this number exactly."
  (catch 'result
    (let* ((is-forward (< 0 dir))
           (skip-chars-fn (if is-forward
                              #'skip-chars-forward
                            #'skip-chars-backward))
           (clamp-fn (if is-forward
                         #'min
                       #'max))
           (limit (if is-forward
                      (line-end-position)
                    (line-beginning-position)))
           (point-init (point))
           ;; Fill when `do-match' is set.
           (match-list (list)))
      (dolist (ch-pair (if is-forward
                           skip-chars
                         (reverse skip-chars)))
        (pcase-let ((`(,ch-skip . ,ch-num) ch-pair))

          ;; Beginning of the match.
          (when do-match
            (push (point) match-list))

          (cond
           ((integerp ch-num)
            (let ((skipped (funcall
                            skip-chars-fn
                            ch-skip
                            (funcall
                             clamp-fn
                             (+ (point) (* ch-num dir)) limit))))
              (when do-check
                (unless (eq skipped ch-num)
                  (throw 'result nil)))))
           ((memq ch-num (list '+ '*))
            (let ((skipped (funcall
                            skip-chars-fn
                            ch-skip limit)))
              (when do-check
                (unless (>= skipped (if (eq ch-num '+) 1 0))
                  (throw 'result nil)))))
           (t
            (error (format "Unknown type %S" ch-skip))))

          ;; End of the match.
          (when do-match
            (push (point) match-list))))

      ;; Match 0 for the full range (expected at the beginning).
      (when do-match
        (cond
         (is-forward
          (setq match-list (nreverse match-list))
          (push (point) match-list)
          (push point-init match-list))
         (t
          (push point-init match-list)
          (push (point) match-list)))

        (set-match-data match-list)))
    t))

(defun evil-numbers--search-and-replace
    (skip-chars sign-group num-group amount base padded decode-fn encode-fn)
  "Perform the increment/decrement on the current line.

For SKIP-CHARS docs see `evil-numbers--match-from-skip-chars'.
NUM-GROUP is the match group used to evaluate the number.
SIGN-GROUP is the match group used for the sign ('-' or '+').

When PADDED is non-nil,
the number keeps it's current width (with leading zeroes).

When all characters are found in sequence,
replace number incremented by AMOUNT in BASE and return non-nil."
  (save-match-data
    (when (save-excursion
            ;; Skip backwards (as needed), there may be no
            ;; characters to skip back, so don't check the result.
            (evil-numbers--match-from-skip-chars skip-chars -1 nil nil)
            ;; Skip forwards from the beginning, setting match data.
            (evil-numbers--match-from-skip-chars skip-chars 1 t t))

      (goto-char (match-end num-group))
      (let* ((num-prev
              (string-to-number
               (funcall decode-fn
                        (concat (match-string sign-group)
                                (match-string num-group)))
               base))
             (num-next (+ amount num-prev))
             (str-next
              (evil-numbers--format
               (abs num-next)
               (if padded
                   (- (match-end num-group)
                      (match-beginning num-group))
                 1)
               base)))

        ;; Replace the sign (as needed).
        (cond
         ;; From negative to positive.
         ((and (< num-prev 0) (not (< num-next 0)))
          (replace-match "" t t nil sign-group))
         ;; From positive to negative.
         ((and (not (< num-prev 0)) (< num-next 0))
          (replace-match (funcall encode-fn "-") t t nil sign-group)))

        ;; Replace the number.
        (replace-match (funcall encode-fn str-next) t t nil num-group))

      t)))

(defun evil-numbers--format (num width base)
  "Format NUM with at least WIDTH space in BASE."
  (cond
   ((= base 2) (evil-numbers--format-binary num width))
   ((= base 8) (format (format "%%0%do" width) num))
   ((= base 16) (format (format "%%0%dX" width) num))
   ((= base 10) (format (format "%%0%dd" width) num))
   (t "")))

(defun evil-numbers--format-binary (number &optional width fillchar)
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
