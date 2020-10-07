;;; scroll-text.el --- Scroll the text for content  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-05 16:19:50

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Scroll the text for content.
;; Keyword: scroll animation animate text
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/scroll-text

;; This file is NOT part of GNU Emacs.

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
;;
;; Scroll the text for content.
;;

;;; Code:

(defgroup scroll-text nil
  "Scroll the text for content."
  :prefix "scroll-text-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/scroll-text"))

(defcustom scroll-text-delay 0.1
  "Time to delay for each character."
  :type 'float
  :group 'scroll-text)

(defvar-local scroll-text--queue '()
  "Queue for text to scroll.
Form by (`point' . `string').")

(defvar-local scroll-text--timer nil
  "Timer to animate scroll text.")

;;; Util

(defun scroll-text--kill-timer (timer)
  "Safe way to kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun scroll-text--concat-string-list (lst-str)
  "Convert list of string, LST-STR to one string."
  (let ((full-str ""))
    (dolist (s lst-str) (when (stringp s) (setq full-str (concat full-str s))))
    full-str))

;;; Core

(defun scroll-text--insert (str)
  "Real insert function when `scroll-text-mode' is enabled."
  (let (scroll-text-mode) (insert str)))

(defun scroll-text--display-char ()
  "Display a character."
  (interactive)
  (let* ((len (length scroll-text--queue))
         next-queue pt char-lst char)
    (unless (= len 0)
      (setq next-queue (nth (1- len) scroll-text--queue)
            pt (car next-queue)
            char-lst (cdr next-queue))
      (if (not char-lst)
          (progn
            (setq scroll-text--queue (butlast scroll-text--queue))
            (scroll-text--display-char))
        (setq char (pop (cdr next-queue)))
        (save-excursion
          (goto-char pt)
          (scroll-text--insert char)
          (setf (car next-queue) (point)))))))

(defun scroll-text--animate ()
  "Start the animation."
  (scroll-text--display-char)
  (unless (= (length scroll-text--queue) 0)
    (scroll-text--kill-timer scroll-text--timer)
    (setq scroll-text--timer
          (run-with-timer scroll-text-delay nil #'scroll-text--animate))))

(defun scroll-text--add-queue (str)
  "Add STR to animation queue."
  (push (cons (point) (split-string str  "" t)) scroll-text--queue))

(defun scroll-text--insert--advice-around (fnc &rest args)
  "Bind execution around `insert' function."
  (if (not scroll-text-mode)
      (apply fnc args)
    (scroll-text--add-queue (scroll-text--concat-string-list args))
    (scroll-text--animate)))

(advice-add 'insert :around #'scroll-text--insert--advice-around)

;;;###autoload
(define-minor-mode scroll-text-mode
  "Minor mode 'scroll-text-mode'."
  :lighter " ScrTxt"
  :group scroll-text)

(provide 'scroll-text)
;;; scroll-text.el ends here
