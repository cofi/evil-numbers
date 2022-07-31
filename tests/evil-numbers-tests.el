;;; evil-numbers-tests.el --- Testing -*- lexical-binding: t -*-

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

;; See: `evil-numbers-tests.sh' for launching this script.

;; TODO: tests that handle bugs: #20, #21, #24, #26, #27.
;; Bugs fixed in:
;; c37a4cf92a9cf8aa9f8bd752ea856a9d1bc6c84c

(require 'ert)

(setq evil-numbers-tests-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path evil-numbers-tests-basedir)
(require 'evil-numbers)

;;; Code:

;; ---------------------------------------------------------------------------
;; Global State

;; VIM keys.
(global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Not VIM keys.
(global-set-key (kbd "C-M-a") 'evil-numbers/inc-at-pt-incremental)
(global-set-key (kbd "C-M-x") 'evil-numbers/dec-at-pt-incremental)


;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 1))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (execute-kbd-macro keys))))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (simulate-input (kbd "i"))
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text))
  (simulate-input (kbd "<escape>"))
  (buffer-enable-undo))

(defmacro with-evil-numbers-test (initial-buffer-text &rest body)
  "Run BODY adding any message call to the MESSAGE-LIST list.
Setting the buffers text to INITIAL-BUFFER-TEXT."
  (declare (indent 1))
  ;; Messages make test output noisy (mainly evil mode switching messages).
  ;; Disable when debugging tests.
  `(let ((inhibit-message t))
     (evil-mode 1)
     (buffer-reset-text ,initial-buffer-text)
     (prog1 (progn ,@body)
       (evil-mode 0))))


;; ---------------------------------------------------------------------------
;; Tests

(ert-deftest simple ()
  "Check a single number increments."
  (let ((text-expected "2|")
        (text-initial "1"))
    (with-evil-numbers-test
     text-initial
     ;; Select the line & increment.
     (simulate-input (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-negative ()
  "Check a single number increments."
  (let ((text-expected "-1|")
        (text-initial "0"))
    (with-evil-numbers-test
     text-initial
     ;; Select the line & increment.
     (simulate-input (kbd "C-x") "a|")
     (should (equal text-expected (buffer-string))))))

;; See bug #18.
(ert-deftest simple-hex ()
  "Check hexadecimal is detected at all parts."
  (let ((text-initial " 0xFFF "))
    ;; Test incrementing at different offsets,
    ;; this ensures scanning the hexadecimal is handled properly.
    (dotimes (i 6)
      (with-evil-numbers-test
       text-initial
       (dotimes (_ i)
         (simulate-input "l"))
       (simulate-input (kbd "C-a") "a|")
       (should (equal " 0x1000| " (buffer-string)))))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "llllll") (kbd "C-a") "a|" (kbd "<escape>"))
     (should (equal " 0xFFF |" (buffer-string))))))

;; See bug #17.
(ert-deftest simple-hex-positive-to-negative ()
  "Change negative hex to negative."
  (let ((text-expected " -0x1| ")
        (text-initial " 0x1 "))
    (dotimes (i 4)
      (with-evil-numbers-test
       text-initial
       (dotimes (_ i)
         (simulate-input "l"))
       (simulate-input (kbd "C-x") (kbd "C-x"))
       (simulate-input "a|" (kbd "<escape>"))
       (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-hex-negative-to-positive ()
  "Change negative hex to positive."
  (let ((text-expected " 0x1| ")
        (text-initial " -0x1 "))
    (dotimes (i 5)
      (with-evil-numbers-test
       text-initial
       (dotimes (_ i)
         (simulate-input "l"))
       (simulate-input (kbd "C-a") (kbd "C-a"))
       (simulate-input "a|" (kbd "<escape>"))
       (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-nop-non-number ()
  "Do nothing, the value under the cursor is not a number."
  (let ((text-expected "X|")
        (text-initial "X"))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-non-number-signed ()
  "Do nothing, the value under the cursor is not a number, but it has a sign."
  (let ((text-expected "-|X")
        (text-initial "-X"))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

;; See bug #25.
(ert-deftest simple-nop-non-number-with-newline-before ()
  "Do nothing, ensure the newline isn't stepped over."
  (let ((text-expected "|\n0")
        (text-initial "\n0"))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "<end>") (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-non-number-with-newline-after ()
  "Do nothing, ensure the newline isn't stepped over."
  (let ((text-expected "0\n|")
        (text-initial "0\n"))
    (with-evil-numbers-test
     text-initial
     (simulate-input "j" (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-cursor-after-decimal ()
  "Do nothing, the cursor is after the number so it shouldn't be modified."
  (let ((text-expected "1 |\n")
        (text-initial "1 \n"))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "<end>") (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-cursor-after-hex ()
  "Do nothing, the cursor is after the number so it shouldn't be modified."
  (let ((text-expected "0xBEEF |\n")
        (text-initial "0xBEEF \n"))
    (with-evil-numbers-test
     text-initial
     (simulate-input (kbd "<end>") (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest simple-separator-chars ()
  "Check a single number increments."
  (let ((text-expected "1_11_111|")
        (text-initial "1_11_110"))
    ;; Test at different offsets to ensure
    ;; there are no bugs similar to #18 occurring.
    (dotimes (i 8)
      (with-evil-numbers-test
       text-initial
       (setq-local evil-numbers-separator-chars "_")
       (dotimes (_ i)
         (simulate-input "l"))
       (simulate-input (kbd "C-a") "a|")
       (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-separator-chars-disabled ()
  "Check a single number increments."
  (let ((text-expected "2|_11_111")
        (text-initial "1_11_111"))
    (with-evil-numbers-test
     text-initial
     (setq-local evil-numbers-separator-chars nil)
     (simulate-input (kbd "C-a") "a|")
     (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-first ()
  "Block selection test."
  (let ((text-expected
         (concat "1| 0 0\n"
                 "1 0 0\n"
                 "1 0 0\n"))
        (text-initial
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test
     text-initial
     (simulate-input
      (kbd "C-v") "jj"          ;; Block select the column
      (kbd "C-a")               ;; Increment.
      "a|")                     ;; Show cursor location.
     (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-second ()
  "Block selection test."
  (let ((text-expected
         (concat "0 1| 0\n"
                 "0 1 0\n"
                 "0 1 0\n"))
        (text-initial
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test
     text-initial
     (simulate-input
      (kbd "w")
      (kbd "C-v") "jj"          ;; Block select the column
      (kbd "C-a")               ;; Increment.
      "a|")                     ;; Show cursor location.
     (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-third ()
  "Block selection test."
  (let ((text-expected
         (concat "0 0 1|\n"
                 "0 0 1\n"
                 "0 0 1\n"))
        (text-initial
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test
     text-initial
     (simulate-input
      (kbd "ww")
      (kbd "C-v") "jj"          ;; Block select the column
      (kbd "C-a")               ;; Increment.
      "a|")                     ;; Show cursor location.
     (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-first-incremental ()
  "Incremental block selection test."
  (let ((text-expected
         (concat "1| 0 0\n"
                 "2 0 0\n"
                 "3 0 0\n"))
        (text-initial
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test
     text-initial
     (simulate-input
      (kbd "C-v") "jj"          ;; Block select the column
      (kbd "C-M-a")             ;; Increment.
      "a|")                     ;; Show cursor location.
     (should (equal text-expected (buffer-string))))))

(ert-deftest selected-lines-incremental ()
  "Incremental line selection test."
  (let ((text-expected
         (concat "1| 2 3\n"
                 "4 5 6\n"
                 "7 8 9\n"))
        (text-initial
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test
     text-initial
     (simulate-input
      (kbd "V") "jj"            ;; Block select the column
      (kbd "C-M-a")             ;; Increment.
      "a|")                     ;; Show cursor location.
     (should (equal text-expected (buffer-string))))))

(provide 'evil-numbers-tests)
;;; evil-numbers-tests.el ends here
