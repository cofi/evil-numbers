;; evil-numbers -- increment/decrement numbers like in vim

;; Copyright (C) 2011 by Michael Markert
;; Author: 2011 Michael Markert <markert.michael@googlemail.com>
;; Created: 2011-09-02
;; Version: 0.
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

;; Known Bugs:
;; See http://github.com/cofi/evil-numbers/issues

;; Install:
;; (require 'evil-numbers)

;; and bind, for example:
;; (global-define-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (global-define-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; or only in evil's normal state:
;; (define-key 'evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key 'evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Usage:
;; Go and play with your numbers!

;; Homepage: http://github.com/cofi/evil-numbers
;; Git-Repository: git://github.com/cofi/evil-numbers.git

(defun evil-numbers/inc-at-pt (amount)
  "Increment the number at point or after point before end-of-line by `amount'"
  (interactive "p*")
  (save-match-data
    (if (not (evil-numbers/search-number))
        (error "No number at point or until end of line")
      (or
       ;; find binary literals
       (when (looking-back "0[bB][01]*")
         (skip-chars-backward "01")
         (search-forward-regexp "[01]*")
         (replace-match
          (evil-numbers/format-binary (+ amount (string-to-number (match-string 0) 2))
                                      (- (match-end 0) (match-beginning 0))))
         t)

       ;; find octal literals
       (when (looking-back "0[oO][0-7]*")
         (skip-chars-backward "01234567")
         (search-forward-regexp "[0-7]+")
         (replace-match
          (format (format "%%0%do" (- (match-end 0) (match-beginning 0)))
                  (+ amount (string-to-number (match-string 0) 8))))
         t)

       ;; find hex literals
       (when (looking-back "0[xX][0-9a-fA-F]*")
         (skip-chars-backward "0123456789abcdefABCDEF")
         (search-forward-regexp "[0-9a-fA-F]+")
         (replace-match
          (format (format "%%0%dX" (- (match-end 0) (match-beginning 0)))
                  (+ amount (string-to-number (match-string 0) 16))))
         t)

       ;; find decimal literals
       (progn
         (skip-chars-backward "0123456789")
         (skip-chars-backward "-")
         (when (looking-at "-?\\([0-9]+\\)")
           (replace-match
            (format (format "%%0%dd" (- (match-end 1) (match-beginning 1)))
                    (+ amount (string-to-number (match-string 0) 10))))
           t))
       (error "No number at point or until end of line")))))

(defun evil-numbers/dec-at-pt (amount)
  "Decrement the number at point or after point before end-of-line by `amount'"
  (interactive "p*")
  (evil-numbers/inc-at-pt (- amount)))

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
   (and
    (looking-back (rx (or (+? digit)
                          (and "0" (or (and (in "bB") (*? (in "01")))
                                       (and (in "oO") (*? (in "0-7")))
                                       (and (in "xX") (*? (in digit "A-Fa-f"))))))))
    ;; being on a specifier is handled in progn
    (not (looking-at "[bBoOxX]")))
   ;; search for number in rest of line
   (progn
     ;; match 0 of specifier or digit, being in a literal and after specifier is handled above
     (re-search-forward "[[:digit:]]" (point-at-eol) t)
     ;; skip format specifiers and interpret as bool
     (<= 0 (skip-chars-forward "bBoOxX")))))

(defun evil-numbers/format-binary (number &optional width fillchar)
  "Format `NUMBER' as binary.
Fill up to `WIDTH' with `FILLCHAR' (defaults to ?0) if binary
representation of `NUMBER' is smaller."
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
