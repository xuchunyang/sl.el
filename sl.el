;;; sl.el --- Emacs clone of sl(1)                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/sl.el
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Version: 0.0

;; This program is free software; you can redistribute it and/or modify
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

;; sl.el is an Emacs clone of sl(1) <https://github.com/mtoyoda/sl>.
;;
;; To use, type M-x sl or M-x sl-little
;;
;; Currently, sl(1) with no option and with -l option are implemented, other
;; options (i.e., -aFc) are not supported.

;;; Code:

(require 'cl-lib)

(defun sl-read-file (file)
  (let ((result '()))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward "[^]*" nil t)
        (push (buffer-substring (match-beginning 0) (- (match-end 0) 2)) result)
        ;; Skip one newline
        (forward-line 1))
      result)))

(defvar sl-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar sl-D51-trains
  (sl-read-file (expand-file-name "D51-trains.txt" sl-dir)))

(defvar sl-little-trains
  (sl-read-file (expand-file-name "little-trains.txt" sl-dir)))

(defvar sl-smokes
  (sl-read-file (expand-file-name "smokes.txt" sl-dir)))

(defun sl-pad-spaces (length text)
  "Pad LENGTH blank spaces at the beginning of text."
  (substring                        ; Remove trailing \n added by cl-loop-concat
   (cl-loop with spaces = (make-string length ?\s)
            for line in (split-string text "\n")
            concat (concat spaces line "\n"))
   0 -1))

(defun sl-insert (linum column window-width text)
  "Insert TEXT at (LINUM, COLUMN) in the current buffer.
The current buffer should be empty.
TEXT can be multiple lines.
COLUMN can be negative."
  (insert (make-string (- linum 1) ?\n))
  (when (< column 0)
    (setq text (mapconcat (lambda (line)
                            (if (> (- column) (length line))
                                ""
                              (substring line (- column))))
                          (split-string text "\n")
                          "\n"))
    (setq column 0))
  (let ((leading (make-string column ?\s))
        (available-width (- window-width column)))
    (insert
     (mapconcat (lambda (line)
                  (concat leading
                          (if (> (length line) available-width)
                              (substring line 0 available-width)
                            line)))
                (split-string text "\n") "\n"))))

(defun sl-subr (trains smokes train-height)
  "Subroutine of sl.
TRAIN-HEIGHT is the total height of TRAINS and its SMOKES."
  (let ((buf (get-buffer-create "*sl*")))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (buffer-disable-undo)
      (setq cursor-type nil)
      (let* ((width (window-width))
             (height (window-height))
             ;; 12 is the height of the train (including its smoke)
             (linum (/ (- height train-height) 2))
             (text1 (car trains))
             (text-width (cl-loop for line in (split-string text1 "\n")
                                  maximize (length line))))
        (cl-loop for col from width downto (- text-width)
                 for counter from 0
                 with smoke-counter = 0
                 when (= 0 (% counter 4)) do (cl-incf smoke-counter)
                 for spaces = (% counter 4)
                 do (progn
                      (erase-buffer)
                      (sl-insert linum col width
                                 (concat
                                  (sl-pad-spaces
                                   spaces
                                   (elt smokes (% smoke-counter (length smokes))))
                                  "\n"
                                  (elt trains (% counter (length trains)))))
                      (sleep-for 0 80)  ; Defaults to 80 ms
                      (discard-input)
                      (redisplay))))
      (kill-buffer))))

;;;###autoload
(defun sl ()
  (interactive)
  (sl-subr sl-D51-trains
           ;; C51 & D%1 needs 7 blank spaces at the beginning,
           ;; the template has 4 spaces
           (mapcar (lambda (text) (sl-pad-spaces 3 text))
                   sl-smokes)
           ;; 10 is the height of DS1, 7 is the height of smoke
           (+ 10 6)))

;;;###autoload
(defun sl-little ()
  "Little version."
  (interactive)
  (sl-subr sl-little-trains sl-smokes
           ;; Both the height of DS1 and the height of smoke are 6
           (+ 6 6)))

(provide 'sl)
;;; sl.el ends here
