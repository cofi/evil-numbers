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
;; Version: 0.7
;; Package-Requires: ((emacs "24.1") (evil "1.2.0"))
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

;; Works like C-a/C-x in VIM, i.e. searches for number up to EOL and
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

;; (evil-define-key '(normal visual) 'global (kbd "C-c +")
;;                  'evil-numbers/inc-at-pt)
;; (evil-define-key '(normal visual) 'global (kbd "C-c -")
;;                  'evil-numbers/dec-at-pt)
;; (evil-define-key '(normal visual) 'global (kbd "C-c C-+")
;;                  'evil-numbers/inc-at-pt-incremental)
;; (evil-define-key '(normal visual) 'global (kbd "C-c C--")
;;                  'evil-numbers/dec-at-pt-incremental)

;; Usage:
;; Go and play with your numbers!

;;; Code:

(require 'evil)

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup evil-numbers nil
  "Support number increment/decrement."
  :group 'convenience)

(define-obsolete-variable-alias
  'evil-numbers/padDefault 'evil-numbers-pad-default "evil-numbers v0.6")

(defcustom evil-numbers-pad-default nil
  "Whether numbers are padded by default."
  :type 'boolean
  :options '(nil t))

(defcustom evil-numbers-separator-chars nil
  "Support separator characters in numeric literals for visual grouping.

This value is a string containing separator characters,
typically \"_\" or \",\" which are allowed in numeric literals in some systems.

Otherwise nil will disable this functionality."
  :type '(choice (const nil) string))

(defcustom evil-numbers-case nil
  "Case to use for hexadecimal numbers."
  :type
  '(choice
    (const :tag "Current Case" nil)
    (const :tag "Upper Case" upcase)
    (const :tag "Lower Case" downcase)))

(defcustom evil-numbers-use-cursor-at-end-of-number nil
  "When non-nil, recognize numbers directly before the cursor.

This doesn't match VIM's behavior."
  :type 'boolean
  :options '(nil t))

;; ---------------------------------------------------------------------------
;; Internal Variables

(defconst evil-numbers--chars-superscript "⁰¹²³⁴⁵⁶⁷⁸⁹")
(defconst evil-numbers--chars-subscript "₀₁₂₃₄₅₆₇₈₉")

(defconst evil-numbers--superscript-alist
  (cons
   (cons ?- ?⁻)
   (cons
    (cons ?+ ?⁺)
    (mapcar
     (lambda (i)
       (cons
        (string-to-char (number-to-string i))
        (aref evil-numbers--chars-superscript i)))
     (number-sequence 0 9)))))

(defconst evil-numbers--subscript-alist
  (cons
   (cons ?- ?₋)
   (cons
    (cons ?+ ?₊)
    (mapcar
     (lambda (i)
       (cons
        (string-to-char (number-to-string i))
        (aref evil-numbers--chars-subscript i)))
     (number-sequence 0 9)))))


;; ---------------------------------------------------------------------------
;; Internal String Separator Utilities
;;
;; To remove, and restore separators.

(defun evil-numbers--strip-chars (str sep-chars)
  "Remove SEP-CHARS from STR."
  (dotimes (i (length sep-chars))
    (let ((ch (char-to-string (aref sep-chars i))))
      (setq str (replace-regexp-in-string (regexp-quote ch) "" str t t))))
  str)

(defun evil-numbers--strip-chars-apply (str-src str-dst sep-chars)
  "Add SEP-CHARS into STR-DST from STR-SRC."
  (let ((sep-chars-list (append sep-chars nil))
        ;; Strings to list.
        (str-src-rev (nreverse (append str-src nil)))
        (str-dst-rev (nreverse (append str-dst nil)))
        (result (list)))
    (while str-dst-rev
      (let ((ch-src (pop str-src-rev)))
        (if (and ch-src (memq ch-src sep-chars-list))
            (push ch-src result)
          (push (pop str-dst-rev) result))))
    (apply #'string result)))

;; ---------------------------------------------------------------------------
;; Internal Utilities
;;
;; Not directly related to incrementing numbers.

(defun evil-numbers--case-category (str default)
  "Categorize the case of STR or return DEFAULT when there is no case.

- default: No case.
-       1: Upper case.
-      -1: Lower case.
-     nil: Mixed case."
  (let ((str-dn (downcase str))
        (str-up (upcase str)))
    (if (string-equal str str-dn)
        (if (string-equal str str-up)
            default
          -1)
      (if (string-equal str str-up)
          1
        nil))))

(defun evil-numbers--format-binary (number &optional width fillchar)
  "Format NUMBER as binary.
Fill up to WIDTH with FILLCHAR (defaults to ?0) if binary
representation of NUMBER is smaller."
  (let ((nums (list))
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

(defun evil-numbers--format (num width base)
  "Format NUM with at least WIDTH space in BASE."
  (cond
   ((= base 2) (evil-numbers--format-binary num width))
   ((= base 8) (format (format "%%0%do" width) num))
   ((= base 16) (format (format "%%0%dX" width) num))
   ((= base 10) (format (format "%%0%dd" width) num))
   (t "")))

(defun evil-numbers--skip-chars-impl
    (ch-skip ch-sep-optional dir ch-num limit)
  "Wrapper for `skip-chars-forward' and `skip-chars-backward'.

CH-SKIP: Characters to skip.
CH-SEP-OPTIONAL: Separator characters (single instances are stepped over).
DIR: Direction to step in (1 -1).
CH-NUM: Number of characters to step.
LIMIT: Point which will not be stepped past."
  (let* ((is-forward (< 0 dir))
         (skip-chars-fn (if is-forward
                            #'skip-chars-forward
                          #'skip-chars-backward))
         (clamp-fn (if is-forward
                       #'min
                     #'max))
         (skipped
          (abs (funcall
                skip-chars-fn ch-skip
                ;; Limit.
                (funcall clamp-fn (+ (point) (* ch-num dir)) limit)))))

    ;; Step over single separators, as long as there is a number after them.
    ;; Allow '100,123' and '16_777_216' to be handled as single numbers.
    (when ch-sep-optional
      (let ((point-next nil)
            (skipped-next 0))
        (setq ch-num (- ch-num skipped))
        (while (and (not (zerop ch-num))
                    (save-excursion
                      (and (eq 1 (evil-numbers--skip-chars-impl
                                  ch-sep-optional nil
                                  dir 1 limit))
                           (progn
                             ;; Note counted towards 'skipped'
                             ;; as this character is to be ignored entirely.
                             (setq skipped-next
                                   (evil-numbers--skip-chars-impl
                                    ch-skip nil
                                    dir ch-num limit))
                             (unless (zerop skipped-next)
                               (setq point-next (point))
                               ;; Found (apply `point-new').
                               t)))))
          ;; Step over the separator and contents found afterwards.
          (when point-next
            (goto-char point-next)
            (setq skipped (+ skipped skipped-next))
            (setq ch-num (- ch-num skipped-next))
            t))))

    skipped))

(defun evil-numbers--match-from-skip-chars
    (match-chars dir limit do-check do-match)
  "Match MATCH-CHARS in DIR (-1 or 1), until LIMIT.

When DO-CHECK is non-nil, any failure to match returns nil.
When DO-MATCH is non-nil, match data is set.

Each item in MATCH-CHARS is a cons pair.
- The first item is the argument to pass to
  `skip-chars-forward' or `skip-chars-backward'.
- The second item specifies how many characters to match,
  Valid values:
  - Symbol `+' one or more.
  - Symbol `*' zero or more.
  - `integerp' this number exactly."
  (catch 'result
    (let* ((is-forward (< 0 dir))
           (point-init (point))
           ;; Fill when `do-match' is set.
           (match-list (list)))

      ;; Sanity check.
      (when (if is-forward (> (point) limit) (< (point) limit))
        (error "Limit is on wrong side of point (internal error)"))

      (unless is-forward
        (setq match-chars (reverse match-chars)))

      (pcase-dolist (`(,ch-skip ,ch-num ,ch-sep-optional) match-chars)
        ;; Beginning of the match.
        (when do-match
          (push (point) match-list))

        (cond
         ((integerp ch-num)
          (let ((skipped
                 (evil-numbers--skip-chars-impl
                  ch-skip ch-sep-optional dir ch-num limit)))
            (when do-check
              (unless (eq skipped ch-num)
                (throw 'result nil)))))
         ((eq ch-num '+)
          (let ((skipped
                 (evil-numbers--skip-chars-impl
                  ch-skip ch-sep-optional dir most-positive-fixnum limit)))
            (when do-check
              (unless (>= skipped 1)
                (throw 'result nil)))))

         ;; No length checking needed as zero is acceptable.
         ;; Skip these characters if they exist.
         ((eq ch-num '*)
          (evil-numbers--skip-chars-impl
           ch-skip ch-sep-optional dir most-positive-fixnum limit))
         ((eq ch-num '\?)
          (evil-numbers--skip-chars-impl
           ch-skip ch-sep-optional dir 1 limit))
         (t
          (error (format "Unknown type %S (internal error)" ch-skip))))

        ;; End of the match.
        (when do-match
          (push (point) match-list)))

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


;; ---------------------------------------------------------------------------
;; Internal Implementation

(defun evil-numbers--inc-at-pt-impl-with-match-chars
    (match-chars
     sign-group num-group
     base
     beg end
     padded
     do-case
     range-check-fn
     number-xform-fn
     decode-fn encode-fn)
  "Perform the increment/decrement on the current line.

For MATCH-CHARS docs see `evil-numbers--match-from-skip-chars'.
NUM-GROUP is the match group used to evaluate the number.
SIGN-GROUP is the match group used for the sign ('-' or '+').

When PADDED is non-nil,
the number keeps it's current width (with leading zeroes).

When RANGE-CHECK-FN is non-nil, it's called with the match beginning & end.
A non nil result causes this function to return nil.

When all characters are found in sequence, evaluate the number in BASE,
replacing it by the result of NUMBER-XFORM-FN and return non-nil."
  (save-match-data
    (when (and
           (save-excursion
             ;; Skip backwards (as needed), there may be no
             ;; characters to skip back, so don't check the result.
             (evil-numbers--match-from-skip-chars match-chars -1 beg nil nil)
             ;; Skip forwards from the beginning, setting match data.
             (evil-numbers--match-from-skip-chars match-chars 1 end t t))

           ;; Either there is no range checking or the range must
           ;; be accepted by the caller.
           (or (null range-check-fn)
               (funcall range-check-fn (match-beginning 0) (match-end 0))))

      (goto-char (match-end num-group))
      (let* ((sep-char
              (nth 2 (nth (1- num-group) match-chars)))
             (str-prev
              (funcall decode-fn
                       (concat (match-string sign-group)
                               (match-string num-group))))

             (str-prev-strip
              (if sep-char
                  (evil-numbers--strip-chars str-prev sep-char)
                str-prev))

             (num-prev (string-to-number str-prev-strip base))
             (num-next (funcall number-xform-fn num-prev))
             (str-next
              (evil-numbers--format
               (abs num-next)
               (if padded
                   (- (match-end num-group)
                      (match-beginning num-group))
                 1)
               base)))

        ;; Maintain case.
        (when do-case
          ;; Upper case (already set), no need to handle here.
          (cond
           ;; Keep current case.
           ((null evil-numbers-case)
            (when (eq -1 (or (evil-numbers--case-category str-prev -1) -1))
              (setq str-next (downcase str-next))))
           ((eq evil-numbers-case 'downcase)
            (setq str-next (downcase str-next)))))

        (when sep-char
          ;; This is a relatively expensive operation,
          ;; only apply separators back if any were found to begin with.
          (unless (string-equal str-prev str-prev-strip)
            (setq str-next (evil-numbers--strip-chars-apply str-prev str-next sep-char))))

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

(defun evil-numbers--inc-at-pt-impl (beg end padded range-check-fn number-xform-fn)
  "Increment the number at the current POINT by AMOUNT limited by BEG and END.

Keep padding when PADDED is non-nil.

See `evil-numbers--inc-at-pt-impl-with-match-chars' for details on
RANGE-CHECK-FN and NUMBER-XFORM-FN.

Return non-nil on success, leaving the point at the end of the number."
  (or
   ;; Find binary literals:
   ;; 0[bB][01]+, e.g. 0b101 or 0B0
   (evil-numbers--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0" 1) ("bB" 1) ("01" + ,evil-numbers-separator-chars))
    1 4 ;; Sign & number groups.
    2 beg end padded nil range-check-fn number-xform-fn
    #'identity #'identity)

   ;; Find octal literals:
   ;; 0[oO][0-7]+, e.g. 0o42 or 0O5
   (evil-numbers--inc-at-pt-impl-with-match-chars

    `(("+-" \?) ("0" 1) ("oO" 1) ("0-7" + ,evil-numbers-separator-chars))
    1 4 ;; Sign & number groups.
    8 beg end padded nil range-check-fn number-xform-fn
    #'identity #'identity)

   ;; Find hex literals:
   ;; 0[xX][0-9a-fA-F]+, e.g. 0xBEEF or 0Xcafe
   (evil-numbers--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0" 1) ("xX" 1) ("[:xdigit:]" + ,evil-numbers-separator-chars))
    1 4 ;; Sign & number groups.
    16 beg end padded t range-check-fn number-xform-fn
    #'identity #'identity)

   ;; Find decimal literals:
   ;; [0-9]+, e.g. 42 or 23.
   (evil-numbers--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0123456789" + ,evil-numbers-separator-chars))
    1 2 ;; Sign & number groups.
    10 beg end padded nil range-check-fn number-xform-fn
    #'identity #'identity)

   ;; Find decimal literals (super-script).
   (evil-numbers--inc-at-pt-impl-with-match-chars
    `(("⁺⁻" \?) (,evil-numbers--chars-superscript + nil))
    1 2 ;; Sign & number groups.
    10 beg end padded nil range-check-fn number-xform-fn
    #'evil-numbers--decode-super #'evil-numbers--encode-super)

   ;; Find decimal literals (sub-script).
   (evil-numbers--inc-at-pt-impl-with-match-chars
    `(("₊₋" \?) (,evil-numbers--chars-subscript + nil))
    1 2 ;; Sign & number groups.
    10 beg end padded nil range-check-fn number-xform-fn
    #'evil-numbers--decode-sub #'evil-numbers--encode-sub)))

(defun evil-numbers--inc-at-pt-impl-with-search (amount beg end padded range-check-fn)
  "Increment the number at the current POINT by AMOUNT limited by BEG and END.

Keep padding when PADDED is non-nil.

See `evil-numbers--inc-at-pt-impl-with-match-chars' for details on
RANGE-CHECK-FN.

Return non-nil on success, leaving the point at the end of the number."
  (let ((found nil))
    (save-match-data
      ;; Search for any text that might be part of a number,
      ;; if `evil-numbers--search-and-replace' cannot parse it - that's fine,
      ;; keep searching until `end'
      ;; This avoids doubling up on number parsing logic.
      ;;
      ;; Note that the while body is empty.
      (while (and
              ;; Found item, exit the loop.
              (null
               (when (evil-numbers--inc-at-pt-impl
                      ;; Clamp limits to line bounds.
                      ;; The caller may use a range that spans lines to
                      ;; allow searching and finding items across
                      ;; multiple lines (currently used for selection).
                      (max beg (point-at-bol))
                      (min end (point-at-eol))
                      padded
                      range-check-fn
                      #'(lambda (n) (+ n amount)))
                 (setq found t)))

              ;; Search failed, exit the loop.
              (re-search-forward
               (concat "["
                       "[:xdigit:]"
                       evil-numbers--chars-superscript
                       evil-numbers--chars-subscript
                       "]")
               end t))))
    found))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload (autoload 'evil-numbers/inc-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt
  (amount beg end type &optional incremental padded)

  "Increment the number at point or after point before `end-of-line' by AMOUNT.
When region is selected, increment all numbers in the region by AMOUNT.

NO-REGION is internal flag that allows
`evil-numbers/inc-at-point' to be called recursively when
applying the regional features of `evil-numbers/inc-at-point'.

INCREMENTAL causes the first number to be increased by 1*AMOUNT,
the second by 2*AMOUNT and so on.

PADDED is whether numbers should be padded (e.g. 10 -> 09).
-    nil: is default behavior set by `evil-numbers-pad-default',
-      t: is the opposite of `evil-numbers-pad-default',
- `'(t)': enables padding and `'(nil)' disables padding.

Numbers with a leading zero are always padded.
Signs are preserved when padding is enabled, for example: increasing a
negative number to a positive will result in a number with a + sign."
  :motion nil
  (interactive "*<c><R>")

  (setq amount (or amount 1))
  (setq padded (if (consp padded)
                   (car padded)
                 (funcall (if padded #'not #'identity)
                          evil-numbers-pad-default)))
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
             (while (evil-numbers--inc-at-pt-impl-with-search
                     (* amount count) (point) (point-max) padded nil)
               (when incremental
                 (setq count (+ count 1))))))))))

   ;; Handle the simple case, either the cursor is over a number,
   ;; or a number exists between the cursor and `end-of-line'.
   (t
    (let ((point-next
           (save-excursion
             (when (evil-numbers--inc-at-pt-impl-with-search
                    amount (point-at-bol) (point-at-eol) padded
                    ;; Optional range checking function, only needed when
                    ;; `evil-numbers-use-cursor-at-end-of-number' is not nil.
                    (cond
                     (evil-numbers-use-cursor-at-end-of-number
                      nil)
                     (t
                      ;; VIM doesn't allow the number directly before
                      ;; the cursor to be manipulated.
                      ;; This function checks the range to disallow that case.
                      ;;
                      ;; Note that this turns out to be simpler than enforcing
                      ;; the limit in the searching logic.
                      (let ((point-init (point)))
                        (lambda (_beg end) (< point-init end))))))
               (point)))))

      (cond
       ((null point-next)
        ;; Point not found, note that VIM doesn't report anything in this case.
        (message "No number at point or until end of line")
        nil)
       (t
        ;; Moves point one position back to conform with VIM,
        ;; see `evil-adjust-cursor' for details.
        (goto-char (1- point-next))
        t))))))

;;;###autoload (autoload 'evil-numbers/dec-at-pt "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt
  (amount beg end type &optional incremental padded)
  "Decrement the number at point or after point before `end-of-line' by AMOUNT.

If a region is active, decrement all the numbers at a point by AMOUNT."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type incremental padded))

;;;###autoload (autoload 'evil-numbers/inc-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/inc-at-pt-incremental
  (amount beg end type padded)
  "Increment the number at point or after point before `end-of-line' by AMOUNT.

When a region is active, increment all the numbers at a point by AMOUNT*n, where
n is the index of the number among the numbers in the region, starting at 1.
That is increment the first number by AMOUNT, the second by 2*AMOUNT,
and so on."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt amount beg end type 'incremental padded))

;;;###autoload (autoload 'evil-numbers/dec-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator evil-numbers/dec-at-pt-incremental
  (amount beg end type padded)
  "Like `evil-numbers/inc-at-pt-incremental' but with negated argument AMOUNT."
  :motion nil
  (interactive "*<c><R>")
  (evil-numbers/inc-at-pt (- (or amount 1)) beg end type 'incremental padded))

(provide 'evil-numbers)

;; Local Variables:
;; fill-column: 80
;; End:

;;; evil-numbers.el ends here
