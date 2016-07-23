;;; sl.el --- Emacs clone of sl(1)                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Package-Requires: ((emacs "24"))
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

;;; Code:

(require 'cl-lib)

(defvar sl-little-trains
  (let ((file (expand-file-name
               "little-trains.txt"
               (file-name-directory (or load-file-name buffer-file-name))))
        (trains '()))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward "[^]*" nil t)
        (push (buffer-substring (match-beginning 0) (- (match-end 0) 2)) trains)
        ;; Skip one newline
        (forward-line 1))
      trains)))

(defun sl-insert (linum column window-width text)
  "Insert TEXT at (LINUM, COLUMN) in the current buffer.
The current buffer should be empty.
TEXT can be multiple lines.
COLUMN can be negative."
  (insert (make-string (- linum 1) ?\n))
  (when (< column 0)
    (setq text (mapconcat (lambda (line) (substring line (- column)))
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

;;;###autoload
(defun sl ()
  (interactive)
  (let ((buf (get-buffer-create "*sl*")))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (buffer-disable-undo)
      (setq cursor-type nil)
      (let* ((width (window-width))
             (text1 (car sl-little-trains))
             (text-width (length (car (split-string text1 "\n")))))
        (cl-loop for col from width downto (- text-width)
                 for counter from 0
                 do (progn
                      (erase-buffer)
                      (sl-insert 24 col width (elt sl-little-trains (% counter (length sl-little-trains))))
                      (sleep-for 0 80)  ; Defaults to 80 ms
                      (discard-input)
                      (redisplay))))
      (kill-buffer))))

(provide 'sl)
;;; sl.el ends here
