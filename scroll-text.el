;;; scroll-text.el --- Scroll the text for content  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-05 16:19:50

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/scroll-text
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (dash "2.14.1"))
;; Keywords: scroll animation animate text

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

(require 'dash)

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
Form by (`point' . `list-string').")

(defvar-local scroll-text--timer nil
  "Timer to animate scroll text.")

;;; Util

(defun scroll-text--kill-timer (timer)
  "Safe way to kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun scroll-text--concat-string-list (lst-str)
  "Convert list of string, LST-STR to one string."
  (let ((full-str ""))
    (when (consp lst-str) (setq lst-str (-flatten lst-str)))
    (dolist (s lst-str)
      (when (stringp s)
        (setq full-str (concat full-str s))))
    full-str))

;;; Core

(defun scroll-text--display-char ()
  "Display a character."
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
          (insert char)
          (setf (car next-queue) (point)))))))

(defun scroll-text-animate ()
  "Start the animation."
  (scroll-text--display-char)
  (unless (= (length scroll-text--queue) 0)
    (scroll-text--kill-timer scroll-text--timer)
    (setq scroll-text--timer
          (run-with-timer scroll-text-delay nil #'scroll-text-animate))))

(defun scroll-text-add-queue (str &optional pt)
  "Add STR to animation queue.
Optional argument PT is the starting display point."
  (unless pt (setq pt (point)))
  (push (cons pt (split-string str "" t)) scroll-text--queue))

;;;###autoload
(defun scroll-text-start (&rest args)
  "Start scroll text with list of string (ARGS)."
  (scroll-text-add-queue (scroll-text--concat-string-list args))
  (scroll-text-animate))

(defun scroll-text--enable ()
  "Enable `scroll-text' in current buffer."
  (setq scroll-text--queue '()))

(defun scroll-text--disable ()
  "Disable `scroll-text' in current buffer."
  (setq scroll-text--queue nil))

;;;###autoload
(define-minor-mode scroll-text-mode
  "Minor mode 'scroll-text-mode'."
  :lighter " ScrTxt"
  :group scroll-text
  (if scroll-text-mode (scroll-text--enable) (scroll-text--disable)))

(provide 'scroll-text)
;;; scroll-text.el ends here
